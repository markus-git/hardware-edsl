{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Language.Embedded.VHDL.Command
  ( SequentialCMD
  , ConcurrentCMD
  , HeaderCMD
  , compile

    -- sequential statements
  , constantL, variableL, fileL
  , (<==), (==:)
  , iff, switch, when, ifThen

    -- concurrent statements
  , constantG, signalG, variableG, fileG
  , process

    -- header statements
  , constantPort, constantGeneric
  , signalPort, signalGeneric
  , variablePort, variableGeneric
  , filePort, fileGeneric
  , entity
  , architecture
  ) where

import Language.VHDL (Identifier(..), Label, Expression, Mode(..))
import qualified Language.VHDL as V

import Language.Embedded.VHDL.Monad      (VHDLT, VHDL)
import Language.Embedded.VHDL.Monad.Type (Type, Kind)
import Language.Embedded.VHDL.Interface
import qualified Language.Embedded.VHDL.Monad       as M
import qualified Language.Embedded.VHDL.Monad.Type  as T
import qualified Language.Embedded.VHDL.Expression.Hoist as H

import Control.Arrow (second)
import Control.Monad.Identity           hiding (when)
import Control.Monad.Operational.Higher hiding (when)
import Control.Applicative
import Data.Typeable
import Data.ALaCarte

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

instance CompileExp exp => Interp (SequentialCMD exp) VHDL
  where
    interp = compileSequential

instance CompileExp exp => Interp (ConcurrentCMD exp) VHDL
  where
    interp = compileConcurrent

instance CompileExp exp => Interp (HeaderCMD exp) VHDL
  where
    interp = compileHeader

-- | Compile an VHDL program into a pretty printed text
compile :: (Interp instr VHDL, HFunctor instr) => Program instr a -> String
compile = show . M.prettyVHDL . interpret

--------------------------------------------------------------------------------

-- | Compile if 'exp' is set
compEM
  :: forall exp a.
     ( PredicateExp exp a
     , CompileExp   exp)
  => Maybe (exp a)
  -> VHDL (Maybe Expression)
compEM = maybe (return Nothing) (>>= return . Just) . fmap compE

-- | Compile hidden type
compTM
  :: forall exp a.
     ( PredicateExp exp a
     , CompileExp   exp)
  => Maybe (exp a)
  -> VHDL Type
compTM _ = compT (undefined :: exp a)

--------------------------------------------------------------------------------
-- ** Sequential commands offered by VHDL

data SequentialCMD (exp :: * -> *) (prog :: * -> *) a
  where
    Local
      :: PredicateExp exp a
      => Kind
      -> Maybe (exp a)
      -> SequentialCMD exp prog Identifier

    Assignment
      :: PredicateExp exp a
      => Identifier
      -> Kind
      -> exp a
      -> SequentialCMD exp prog ()

    If
      :: PredicateExp exp Bool
      => (exp Bool, prog ())     -- if
      -> [(exp Bool,  prog ())]  -- else-ifs
      -> prog ()                 -- else
      -> SequentialCMD exp prog ()

    Case
      :: PredicateExp exp a
      => exp a
      -> [(exp a, prog ())]
      -> Maybe (prog ())
      -> SequentialCMD exp prog ()
    
instance HFunctor (SequentialCMD exp)
  where
    hfmap _ (Local k e  )      = Local k e
    hfmap _ (Assignment i k e) = Assignment i k e
    hfmap f (If (b, t) os e)   = If (b, f t) (map (second f) os) (f e)
    hfmap f (Case e cs d)      = Case e (map (second f) cs) (fmap f d)

type instance IExp (SequentialCMD e)       = e
type instance IExp (SequentialCMD e :+: i) = e

--------------------------------------------------------------------------------

-- | Declare local constands/variables/files
constantL, variableL, fileL
  :: (SequentialCMD (IExp instr) :<: instr, PredicateExp (IExp instr) a)
  => Maybe (IExp instr a)
  -> ProgramT instr m Identifier
constantL = singleE . Local T.Constant
variableL = singleE . Local T.Variable
fileL     = singleE . Local T.File

-- | Assign a signal to some expression
(<==) :: (SequentialCMD (IExp instr) :<: instr, PredicateExp (IExp instr) a)
      => Identifier
      -> IExp instr a
      -> ProgramT instr m ()
(<==) i = singleE . Assignment i T.Signal

-- | Assign a variable to some expression
(==:) :: (SequentialCMD (IExp instr) :<: instr, PredicateExp (IExp instr) a)
      => Identifier
      -> IExp instr a
      -> ProgramT instr m ()
(==:) i = singleE . Assignment i T.Variable

-- | Conventional if statement
iff 
  :: (SequentialCMD (IExp instr) :<: instr, PredicateExp (IExp instr) Bool)
  =>  (IExp instr Bool, ProgramT instr m ())
  -> [(IExp instr Bool, ProgramT instr m ())]
  -> ProgramT instr m ()
  -> ProgramT instr m ()
iff th eif el = singleE $ If th eif el

-- | Conventional switch (or case) statement
switch
  :: (SequentialCMD (IExp instr) :<: instr, PredicateExp (IExp instr) a)
  => IExp instr a
  -> [(IExp instr a, ProgramT instr m ())]
  -> Maybe (ProgramT instr m ())
  -> ProgramT instr m ()
switch e choices def = singleE $ Case e choices def

--------------------------------------------------------------------------------

-- | Guards a program by some predicate
when
  :: ( SequentialCMD (IExp instr) :<: instr
     , PredicateExp  (IExp instr) Bool
     , Monad m)
  => IExp instr Bool
  -> ProgramT instr m ()
  -> ProgramT instr m ()
when b prg = singleE $ If (b, prg) [] (return ())

-- | Standard 'if .. then .. else ..' statement
ifThen
  :: ( SequentialCMD (IExp instr) :<: instr
     , PredicateExp  (IExp instr) Bool)
  => IExp instr Bool
  -> ProgramT instr m ()
  -> ProgramT instr m ()
  -> ProgramT instr m ()
ifThen b th el = singleE $ If (b, th) [] el

--------------------------------------------------------------------------------

compileSequential :: CompileExp exp => SequentialCMD exp VHDL a -> VHDL a
compileSequential (Local k e) =
  do v <- compEM e
     t <- compTM e
     i <- M.newSym
     M.addLocal $ case k of
       T.Constant -> M.declConstant i t v
       T.Signal   -> M.declSignal   i t v
       T.Variable -> M.declVariable i t v
     return i
compileSequential (Assignment i k e) =
  do v <- compE e
     M.addSequential $ case k of
       T.Signal -> M.assignSignal   i v
       _        -> M.assignVariable i v
compileSequential (If (b, th) eif els) =
  do let (cs, es) = unzip eif
     v  <- compE b
     bs <- mapM compE cs
     s  <- M.inConditional (v, th) (zip bs es) els
     M.addSequential $ V.SIf s
compileSequential (Case e choices def) =
  do let (cs, es) = unzip choices
     v  <- compE e
     bs <- mapM (compE >=> return . lower) cs
     s  <- M.inCase v (others def $ zip bs es)
     M.addSequential $ V.SCase s
  where
    lower :: V.Expression -> V.Choices
    lower exp = V.Choices . (:[]) . V.ChoiceSimple $
      case exp of
        (V.EAnd  rels) -> head' rels
        (V.EOr   rels) -> head' rels
        (V.EXor  rels) -> head' rels
        (V.ENand r _)  -> drop' r
        (V.ENor  r _)  -> drop' r
        (V.EXnor rels) -> head' rels
      where
        head' :: [V.Relation] -> V.SimpleExpression
        head' [] = H.lift $ V.PrimLit V.LitNull
        head' xs = drop' (head xs)

        drop' :: V.Relation -> V.SimpleExpression
        drop' (V.Relation (V.ShiftExpression x _) _) = x

    others :: Maybe x -> [(V.Choices, x)] -> [(V.Choices, x)]
    others (Nothing) cs = cs
    others (Just d)  cs = cs ++ [(V.Choices [V.ChoiceOthers], d)]

--------------------------------------------------------------------------------
-- ** Concurrent commands offered by VHDL

data ConcurrentCMD exp (prog :: * -> *) a
  where
    Global
      :: PredicateExp exp a  
      => Kind
      -> Maybe (exp a)
      -> ConcurrentCMD exp prog (Identifier)

    Process
      :: Label
      -> [Identifier]
      -> prog ()
      -> ConcurrentCMD exp prog ()

    PortMap
      :: [Identifier] -> ConcurrentCMD exp prog (Identifier)

instance HFunctor (ConcurrentCMD exp)
  where
    hfmap _ (Global  k e)    = Global  k e
    hfmap f (Process l is p) = Process l is (f p)
    hfmap _ (PortMap is)     = PortMap is

type instance IExp (ConcurrentCMD e)       = e
type instance IExp (ConcurrentCMD e :+: i) = e

--------------------------------------------------------------------------------

-- | Declare global constands/variables/files
constantG, signalG, variableG, fileG
  :: ( ConcurrentCMD (IExp instr) :<: instr
     , PredicateExp  (IExp instr) a)
  => Maybe (IExp instr a)
  -> ProgramT instr m Identifier
constantG = singleE . Global T.Constant
signalG   = singleE . Global T.Signal
variableG = singleE . Global T.Variable
fileG     = singleE . Global T.File

-- | Declare a process
process
  :: (ConcurrentCMD (IExp instr) :<: instr)
  => String
  -> [Identifier]
  -> ProgramT instr m ()
  -> ProgramT instr m ()
process i is = singleE . Process (Ident i) is

--------------------------------------------------------------------------------

compileConcurrent :: CompileExp exp => ConcurrentCMD exp VHDL a -> VHDL a
compileConcurrent (Global k e) =
  do v <- compEM e
     t <- compTM e
     i <- M.newSym
     M.addGlobal $ case k of
       T.Constant -> M.declConstant i t v
       T.Signal   -> M.declSignal   i t v 
       T.Variable -> M.declVariable i t v
     return i
compileConcurrent (Process l is p) =
  do (a, process) <- M.inProcess l is p
     M.addConcurrent (V.ConProcess process)
     return a
compileConcurrent (PortMap is) =
  do let ads = fmap (V.ADSignal . V.NSimple) is
     n   <- M.newSym
     lbl <- M.newLabel
     M.addConcurrent (M.portMap lbl n ads)
     return n
     
--------------------------------------------------------------------------------
-- ** Entity declaration related commands offered by VHDL

data DeclKind = Port | Generic

data Record a = Record Identifier

data Array  a = Array  Identifier

data HeaderCMD exp (prog :: * -> *) a
  where
    DeclarePort
      :: PredicateExp exp a
      => DeclKind
      -> Kind
      -> Mode
      -> Maybe (exp a)
      -> HeaderCMD exp prog Identifier

    DeclareRecord
      :: [(Identifier, Type)]
      -> HeaderCMD exp prog (Record a)

    DeclareArray
      :: HeaderCMD exp prog (Array a)

    Entity
      :: Identifier
      -> prog a
      -> HeaderCMD exp prog a

    Architecture
      :: Identifier -- architecture's name
      -> Identifier -- entity's name
      -> prog a
      -> HeaderCMD exp prog a

instance HFunctor (HeaderCMD exp)
  where
    hfmap _ (DeclarePort d k m e) = DeclarePort d k m e
    hfmap _ (DeclareRecord rs)    = DeclareRecord rs
    hfmap _ (DeclareArray)        = DeclareArray
    hfmap f (Entity e p)          = Entity e (f p)
    hfmap f (Architecture a e p)  = Architecture a e (f p)

type instance IExp (HeaderCMD e)       = e
type instance IExp (HeaderCMD e :+: i) = e

--------------------------------------------------------------------------------

-- | Declare constands/signal/variables/files ports and generics.
constantPort, constantGeneric, signalPort, signalGeneric, variablePort, variableGeneric, filePort, fileGeneric
  :: (HeaderCMD (IExp instr) :<: instr, PredicateExp (IExp instr) a)
  => Mode
  -> Maybe (IExp instr a)
  -> ProgramT instr m Identifier
constantPort    m = singleE . DeclarePort Port    T.Constant m
constantGeneric m = singleE . DeclarePort Generic T.Constant m
signalPort      m = singleE . DeclarePort Port    T.Signal   m
signalGeneric   m = singleE . DeclarePort Generic T.Signal   m
variablePort    m = singleE . DeclarePort Port    T.Variable m
variableGeneric m = singleE . DeclarePort Generic T.Variable m
filePort        m = singleE . DeclarePort Port    T.File     m
fileGeneric     m = singleE . DeclarePort Generic T.File     m

-- | Declare an entity.
entity
  :: (HeaderCMD (IExp instr) :<: instr)
  => String
  -> ProgramT instr m a
  -> ProgramT instr m a
entity name = singleE . Entity (Ident name)

-- | Declare an architecture.
architecture
  :: (HeaderCMD (IExp instr) :<: instr)
  => String
  -> String
  -> ProgramT instr m a
  -> ProgramT instr m a
architecture name entity = singleE . Architecture (Ident name) (Ident entity)

--------------------------------------------------------------------------------

compileHeader :: CompileExp exp => HeaderCMD exp VHDL a -> VHDL a
compileHeader (DeclarePort Port k m e) =
  do v <- compEM e
     t <- compTM e
     i <- M.newSym
     M.addPort $ case k of
       T.Constant -> M.interfaceConstant i   t v
       T.Signal   -> M.interfaceSignal   i m t v
       T.Variable -> M.interfaceVariable i m t v
     return i
compileHeader (DeclarePort Generic k m e) =
  do v <- compEM e
     t <- compTM e
     i <- M.newSym
     M.addGeneric $ case k of
       T.Constant -> M.interfaceConstant i   t v
       T.Signal   -> M.interfaceSignal   i m t v
       T.Variable -> M.interfaceVariable i m t v
     return i
compileHeader (DeclareRecord rs) =
  do i <- M.newSym
     r <- return $ M.declRecord i rs
     undefined
compileHeader (DeclareArray) =
  do error "imperative-vhdl: arrays are not yet supported."
compileHeader (Entity name prg) =
  do M.entity name prg
compileHeader (Architecture name entity prg) =
  do M.architecture name entity prg

--------------------------------------------------------------------------------
