{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Language.Embedded.VHDL.Command
{-  ( SequentialCMD
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
  , newArray, newArray_
  , entity
  , architecture
  , library
  , imports
  )-} where

import Language.VHDL (Identifier(..), Mode(..), Expression)
import qualified Language.VHDL as V

import Language.Embedded.VHDL.Monad      (VHDLT, VHDL)
import Language.Embedded.VHDL.Monad.Type (Type, Kind)
import Language.Embedded.VHDL.Interface
import qualified Language.Embedded.VHDL.Monad            as M
import qualified Language.Embedded.VHDL.Monad.Type       as T
import qualified Language.Embedded.VHDL.Monad.Expression as M
import qualified Language.Embedded.VHDL.Expression.Hoist as H

import Control.Arrow (second)
import Control.Monad.Identity           hiding (when)
import Control.Monad.Operational.Higher hiding (when)
import Control.Applicative
import Data.Typeable
import Data.ALaCarte
import Data.Ix

--------------------------------------------------------------------------------
-- * ...
--------------------------------------------------------------------------------

data Clause   = Port    | Generic

data Scope    = Process | Architecture | Entity

class ToIdent a
  where
    toIdent :: a -> V.Identifier

--------------------------------------------------------------------------------
-- ** ...

compEM :: forall exp a. (PredicateExp exp a, CompileExp exp) => Maybe (exp a) -> VHDL (Maybe Expression)
compEM e = maybe (return Nothing) (>>= return . Just) $ fmap compE e

compTM :: forall exp a. (PredicateExp exp a, CompileExp exp) => Maybe (exp a) -> VHDL Type
compTM _ = compT (undefined :: exp a)

--------------------------------------------------------------------------------
-- * ... Signals
--------------------------------------------------------------------------------

-- | ...
data Signal a = Signal Integer

instance ToIdent (Signal a)
  where
    toIdent (Signal i) = Ident $ 's' : show i

-- | ...
data SignalCMD (exp :: * -> *) (prog :: * -> *) a
  where
    NewSignal
      :: PredicateExp exp a
      => Clause
      -> Scope
      -> Mode
      -> Maybe (exp a)
      -> SignalCMD exp prog (Signal a)

    GetSignal
      :: PredicateExp exp a
      => Signal a
      -> SignalCMD exp prog (exp a)

    SetSignal
      :: PredicateExp exp a
      => Signal a
      -> exp a
      -> SignalCMD exp prog ()

--------------------------------------------------------------------------------
-- ** ...

type instance IExp (SignalCMD e)       = e
type instance IExp (SignalCMD e :+: i) = e

instance HFunctor (SignalCMD exp)
  where
    hfmap _ (NewSignal c s m e) = NewSignal c s m e
    hfmap _ (GetSignal s)       = GetSignal s
    hfmap _ (SetSignal s e)     = SetSignal s e

instance CompileExp exp => Interp (SignalCMD exp) VHDL
  where
    interp = compileSignal

-- | ...
compileSignal :: forall exp a. CompileExp exp => SignalCMD exp VHDL a -> VHDL a
compileSignal (NewSignal clause scope mode exp) =
  do v <- compEM exp
     t <- compTM exp
     i <- Signal <$> M.freshUnique
     let block     = M.declSignal      (toIdent i)      t v
         interface = M.interfaceSignal (toIdent i) mode t v
     case scope of
       Process      -> M.addLocal  block
       Architecture -> M.addGlobal block
       Entity    -> case clause of
         Port    -> M.addPort    interface
         Generic -> M.addGeneric interface
     return i
compileSignal (GetSignal (Signal s)) =
  do undefined -- Don't know what to put here, something like '... (varE s)' but that loses the 's'..
compileSignal (SetSignal s exp) =
  do M.addSequential =<< M.assignSequentialSignal (toIdent s) <$> compE exp

--------------------------------------------------------------------------------
-- ** ..

-- | Declare a signal.
newSignal  :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a) => IExp i a -> ProgramT i m (Signal a)
newSignal  = singleE . NewSignal Port Architecture InOut . Just

-- | Declare an uninitialized signal.
newSignal_ :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a) => ProgramT i m (Signal a)
newSignal_ = singleE $ NewSignal Port Architecture InOut Nothing

-- | Fetches the current value of a signal.
getSignal :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a) => Signal a -> ProgramT i m (IExp i a)
getSignal = singleE . GetSignal

-- | Update the value of a signal.
setSignal :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a) => Signal a -> IExp i a -> ProgramT i m ()
setSignal s = singleE . SetSignal s

--------------------------------------------------------------------------------

-- | Short-hand for 'setSignal'.
(<==) :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a) => Signal a -> IExp i a -> ProgramT i m ()
(<==) = setSignal

-- | Declare port/generic signals of the given mode.
newPort, newGeneric :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a) => Mode -> ProgramT i m (Signal a)
newPort    m = singleE $ NewSignal Port    Entity m Nothing
newGeneric m = singleE $ NewSignal Generic Entity m Nothing

--------------------------------------------------------------------------------
-- * ... Variables
--------------------------------------------------------------------------------

data Variable a = Variable Integer

instance ToIdent (Variable a)
  where
    toIdent (Variable i) = Ident $ 'v' : show i

data VariableCMD (exp :: * -> *) (prog :: * -> *) a
  where
    NewVariable
      :: PredicateExp exp a
      => Maybe (exp a)
      -> VariableCMD exp prog (Variable a)

    GetVariable
      :: PredicateExp exp a
      => Variable a
      -> VariableCMD exp prog (exp a)

    SetVariable
      :: PredicateExp exp a
      => Variable a
      -> exp a
      -> VariableCMD exp prog ()

--------------------------------------------------------------------------------
-- ** ...

type instance IExp (VariableCMD e)       = e
type instance IExp (VariableCMD e :+: i) = e

instance HFunctor (VariableCMD exp)
  where
    hfmap _ (NewVariable e)   = NewVariable e
    hfmap _ (GetVariable s)   = GetVariable s
    hfmap _ (SetVariable s e) = SetVariable s e

instance CompileExp exp => Interp (VariableCMD exp) VHDL
  where
    interp = compileVariable

compileVariable :: forall exp a. CompileExp exp => VariableCMD exp VHDL a -> VHDL a
compileVariable (NewVariable exp) =
  do v <- compEM exp
     t <- compTM exp
     i <- Variable <$> M.freshUnique
     M.addLocal $ M.declVariable (toIdent i) t v
     return i
compileVariable (GetVariable v) =
  do undefined -- ... same as for signals
compileVariable (SetVariable v exp) =
  do M.addSequential =<< M.assignVariable (toIdent v) <$> compE exp

--------------------------------------------------------------------------------
-- ** ...

-- | Declare a variable.
newVariable  :: (VariableCMD (IExp i) :<: i, PredicateExp (IExp i) a) => IExp i a -> ProgramT i m (Variable a)
newVariable  = singleE . NewVariable . Just

-- | Declare an uninitialized variable.
newVariable_ :: (VariableCMD (IExp i) :<: i, PredicateExp (IExp i) a) => ProgramT i m (Variable a)
newVariable_ = singleE $ NewVariable Nothing

-- | Fetches the current value of a variable.
getVariable  :: (VariableCMD (IExp i) :<: i, PredicateExp (IExp i) a) => Variable a -> ProgramT i m (IExp i a)
getVariable = singleE . GetVariable

-- | Updates the value of a variable.
setVariable :: (VariableCMD (IExp i) :<: i, PredicateExp (IExp i) a) => Variable a -> IExp i a -> ProgramT i m ()
setVariable v = singleE . SetVariable v

--------------------------------------------------------------------------------

-- | Short-hand for 'setVariable'.
(==:) :: (VariableCMD (IExp i) :<: i, PredicateExp (IExp i) a) => Variable a -> IExp i a -> ProgramT i m ()
(==:) = setVariable

--------------------------------------------------------------------------------
-- * ... Arrays
--------------------------------------------------------------------------------

-- ...

--------------------------------------------------------------------------------
-- * ... Packages
--------------------------------------------------------------------------------

-- ...

--------------------------------------------------------------------------------
-- * ... Entities
--------------------------------------------------------------------------------

-- | ...
data EntityCMD (exp :: * -> *) (prog :: * -> *) a
  where
    NewEntity
      :: String
      -> prog a
      -> EntityCMD exp prog a

--------------------------------------------------------------------------------
-- ** ...

type instance IExp (EntityCMD e)       = e
type instance IExp (EntityCMD e :+: i) = e

instance HFunctor (EntityCMD exp)
  where
    hfmap f (NewEntity e p) = NewEntity e (f p)

instance CompileExp exp => Interp (EntityCMD exp) VHDL
  where
    interp = compileEntity

compileEntity :: forall exp a. CompileExp exp => EntityCMD exp VHDL a -> VHDL a
compileEntity (NewEntity s prog) = M.entity (Ident s) prog

--------------------------------------------------------------------------------
-- ** ...

-- | Declare a new entity by running the given program to initialize ports.
newEntity :: (EntityCMD (IExp i) :<: i) => String -> ProgramT i m a -> ProgramT i m a
newEntity e = singleE . NewEntity e

--------------------------------------------------------------------------------
-- * ... Architectures
--------------------------------------------------------------------------------

-- | ...
data ArchitectureCMD (exp :: * -> *) (prog :: * -> *) a
  where
    NewArchitecture
      :: String
      -> String
      -> prog a
      -> ArchitectureCMD exp prog a

--------------------------------------------------------------------------------
-- ** ...

type instance IExp (ArchitectureCMD e)       = e
type instance IExp (ArchitectureCMD e :+: i) = e

instance HFunctor (ArchitectureCMD exp)
  where
    hfmap f (NewArchitecture e a p) = NewArchitecture e a (f p)

instance CompileExp exp => Interp (ArchitectureCMD exp) VHDL
  where
    interp = compileArchitecture

compileArchitecture :: forall exp a. CompileExp exp => ArchitectureCMD exp VHDL a -> VHDL a
compileArchitecture (NewArchitecture e a prog) = M.architecture (Ident e) (Ident a) prog

--------------------------------------------------------------------------------
-- ** ...

-- | Declare a new architecture by running the given program to produce a body.
newArchitecture :: (ArchitectureCMD (IExp i) :<: i) => String -> String -> ProgramT i m a -> ProgramT i m a
newArchitecture e a = singleE . NewArchitecture e a

--------------------------------------------------------------------------------
-- * ... Processes
--------------------------------------------------------------------------------

-- | ...
data SignalX = forall a. SignalX (Signal a)

-- | ...
data ProcessCMD (exp :: * -> *) (prog :: * -> *) a
  where
    NewProcess
      :: [SignalX]
      -> prog ()
      -> ProcessCMD exp prog ()

--------------------------------------------------------------------------------
-- ** ...

type instance IExp (ProcessCMD e)       = e
type instance IExp (ProcessCMD e :+: i) = e

instance HFunctor (ProcessCMD exp)
  where
    hfmap f (NewProcess is p) = NewProcess is (f p)

instance CompileExp exp => Interp (ProcessCMD exp) VHDL
  where
    interp = compileProcess

compileProcess :: forall exp a. CompileExp exp => ProcessCMD exp VHDL a -> VHDL a
compileProcess (NewProcess is prog) =
  do l      <- M.newLabel
     (a, c) <- M.inProcess l (fmap unX is) prog
     M.addConcurrent (V.ConProcess c)
     return a
  where
    unX :: SignalX -> Identifier
    unX (SignalX s) = toIdent s

--------------------------------------------------------------------------------
-- ** ...

-- | ...
toX :: Signal a -> SignalX
toX = SignalX

-- | ...
newProcess :: (ProcessCMD (IExp i) :<: i) => [SignalX] -> ProgramT i m () -> ProgramT i m ()
newProcess is = singleE . NewProcess is

--------------------------------------------------------------------------------
-- * ... Conditionals
--------------------------------------------------------------------------------

-- ...

{-
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

-- | ...
--
-- *** Array operations are concurrent first?
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

    GetArray
      :: ( PredicateExp exp a
         , PredicateExp exp n
         , Integral n
         , Ix n )
      => exp n
      -> Array n a
      -> SequentialCMD exp prog (exp a)

    SetArray
      :: ( PredicateExp exp a
         , PredicateExp exp n
         , Integral n
         , Ix n )
      => exp n
      -> exp a
      -> Array n a
      -> SequentialCMD exp prog ()
    
instance HFunctor (SequentialCMD exp)
  where
    hfmap _ (Local k e  )      = Local k e
    hfmap _ (Assignment i k e) = Assignment i k e
    hfmap f (If (b, t) os e)   = If (b, f t) (map (second f) os) (f e)
    hfmap f (Case e cs d)      = Case e (map (second f) cs) (fmap f d)
    hfmap _ (GetArray i a)     = GetArray i a
    hfmap _ (SetArray i v a)   = SetArray i v a

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

-- | ...
getArray
  :: ( SequentialCMD (IExp instr) :<: instr
     , PredicateExp (IExp instr) a
     , PredicateExp (IExp instr) n
     , Integral n
     , Ix n )
  => IExp instr n
  -> Array n a
  -> ProgramT instr m (IExp instr a)
getArray i = singleE . GetArray i

-- | ...
setArray
  :: ( SequentialCMD (IExp instr) :<: instr
     , PredicateExp (IExp instr) a
     , PredicateExp (IExp instr) n
     , Integral n
     , Ix n )
  => IExp instr n
  -> IExp instr a
  -> Array n a
  -> ProgramT instr m ()
setArray i v = singleE . SetArray i v

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

compileSequential :: forall exp a. CompileExp exp => SequentialCMD exp VHDL a -> VHDL a
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
compileSequential (GetArray index (Array a :: Array n b)) =
  do i <- compE  index
     t <- compAT index (undefined :: Array n b)
     
     undefined
compileSequential (SetArray index v (Array a :: Array n b)) =
  do i <- compE  index
     t <- compAT index (undefined :: Array n b)
     
     undefined

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

data DeclKind  = Port | Generic

data Record  a = Record Identifier

data Array n a = Array Identifier

-- | ...
--
-- *** Swap DeclareRecord/Array for Declare(Composite)Type?..
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
      :: ( PredicateExp exp a
         , PredicateExp exp n
         , Integral n
         , Ix n )
      => Maybe (exp n)
      -> HeaderCMD exp prog (Array n a)

    -- ^ ...
    Entity
      :: Identifier
      -> prog a
      -> HeaderCMD exp prog a

    -- ^ ...
    Architecture
      :: Identifier -- architecture's name
      -> Identifier -- entity's name
      -> prog a
      -> HeaderCMD exp prog a

    Library :: String -> HeaderCMD exp prog ()
    Import  :: String -> HeaderCMD exp prog ()

instance HFunctor (HeaderCMD exp)
  where
    hfmap _ (DeclarePort d k m e) = DeclarePort d k m e
    hfmap _ (DeclareRecord rs)    = DeclareRecord rs
    hfmap _ (DeclareArray i)      = DeclareArray i
    hfmap f (Entity e p)          = Entity e (f p)
    hfmap f (Architecture a e p)  = Architecture a e (f p)
    hfmap _ (Library s)           = Library s
    hfmap _ (Import s)            = Import s

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

-- | Declare a constrained array.
newArray
  :: ( HeaderCMD (IExp instr) :<: instr
     , PredicateExp (IExp instr) a
     , PredicateExp (IExp instr) n
     , Integral n
     , Ix n )
  => IExp instr n
  -> ProgramT instr m (Array n a)
newArray i = singleE $ DeclareArray (Just i)

-- | Declare an unconstrained array.
newArray_
  :: ( HeaderCMD (IExp instr) :<: instr
     , PredicateExp (IExp instr) a
     , PredicateExp (IExp instr) n
     , Integral n
     , Ix n )
  => ProgramT instr m (Array n a)
newArray_ = singleE $ DeclareArray Nothing

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

-- | Imports a library.
library :: (HeaderCMD (IExp instr) :<: instr) => String -> ProgramT instr m ()
library = singleE . Library

-- | Imports a library module.
imports :: (HeaderCMD (IExp instr) :<: instr) => String -> ProgramT instr m ()
imports = singleE . Import

--------------------------------------------------------------------------------

compileHeader :: forall exp a. CompileExp exp => HeaderCMD exp VHDL a -> VHDL a
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
compileHeader (DeclareArray size) =
  do i <- M.newSym
     v <- compEM  size
     t <- compATM size (undefined :: a)
     M.addType $ case v of
       Just range -> M.constrainedArray   i t (range `M.downto` (H.lift (M.lit "0"))) -- Meh...
       Nothing    -> M.unconstrainedArray i t
     return (Array i)
compileHeader (Entity       name        prg) = M.entity name prg
compileHeader (Architecture name entity prg) = M.architecture name entity prg
compileHeader (Library s)                    = M.newLibrary s
compileHeader (Import s)                     = M.newImport s

--------------------------------------------------------------------------------

compAT
  :: forall exp n a.
     ( CompileExp exp
     , PredicateExp exp n
     , PredicateExp exp a )
  => exp n
  -> Array n a
  -> VHDL Type
compAT i a = compATM (Just i) a

-- | ...
compATM
  :: forall exp n a.
     ( CompileExp exp
     , PredicateExp exp n
     , PredicateExp exp a )
  => Maybe (exp n)
  -> Array n a
  -> VHDL Type
compATM _ _ = compT (undefined :: exp a)

--------------------------------------------------------------------------------
-}
