{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Language.Embedded.VHDL.Command where

import Language.VHDL (Identifier(..), Label, Expression, Mode(..))
import qualified Language.VHDL as V

import Language.Embedded.VHDL.Monad           (VHDLT, VHDL)
import Language.Embedded.VHDL.Interface
import Language.Embedded.VHDL.Expression.Type (Type, Kind)
import qualified Language.Embedded.VHDL.Monad           as M
import qualified Language.Embedded.VHDL.Expression.Type as T

import Control.Arrow (second)
import Control.Monad.Identity
import Control.Monad.Operational.Compositional
import Control.Applicative
import Data.Typeable

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
compile :: (Interp instr VHDL, MapInstr instr) => Program instr a -> String
compile = show . M.prettyVHDL . interpret

--------------------------------------------------------------------------------

-- | Compile if 'exp' is set
compEM :: CompileExp exp => Maybe (exp a) -> VHDL (Maybe Expression)
compEM = maybe (return Nothing) (>>= return . Just) . fmap compE

-- | Compile hidden type
compTM :: forall exp a. (PredicateExp exp a, CompileExp exp) => Maybe (exp a) -> VHDL Type
compTM _ = compT (undefined :: exp a)

--------------------------------------------------------------------------------
-- ** Sequential commands offered by VHDL

data SequentialCMD (exp :: * -> *) (prog :: * -> *) a
  where
    Local
      :: PredicateExp exp a
      => Identifier
      -> Kind
      -> Maybe (exp a)
      -> SequentialCMD exp prog ()

    Assignment
      :: PredicateExp exp a
      => Identifier
      -> Kind
      -> exp a
      -> SequentialCMD exp prog ()

    If
      :: (exp Bool, prog ())     -- if
      -> [(exp Bool,  prog ())]  -- elseif*
      -> prog ()                 -- else
      -> SequentialCMD exp prog ()
    
instance MapInstr (SequentialCMD exp)
  where
    imap _ (Local i k e)      = Local i k e
    imap _ (Assignment i k e) = Assignment i k e
    imap f (If (b, t) os e)   = If (b, f t) (map (second f) os) (f e)

type instance IExp (SequentialCMD e)       = e
type instance IExp (SequentialCMD e :+: i) = e

--------------------------------------------------------------------------------

-- | Declare local constands/variables/files
constantL, variableL, fileL
  :: (SequentialCMD (IExp instr) :<: instr, PredicateExp (IExp instr) a)
  => Identifier
  -> Maybe (IExp instr a)
  -> ProgramT instr m ()
constantL i = singleE . Local i T.Constant
variableL i = singleE . Local i T.Variable
fileL     i = singleE . Local i T.File

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

-- | Guards a program by some predicate
when 
  :: (SequentialCMD (IExp instr) :<: instr, Monad m)
  => IExp instr Bool
  -> ProgramT instr m ()
  -> ProgramT instr m ()
when b prg = singleE $ If (b, prg) [] (return ())

-- | Standard 'if .. then .. else ..' statement
ifThen 
  :: (SequentialCMD (IExp instr) :<: instr)
  => IExp instr Bool
  -> ProgramT instr m ()
  -> ProgramT instr m ()
  -> ProgramT instr m ()
ifThen b th el = singleE $ If (b, th) [] el

--------------------------------------------------------------------------------

compileSequential :: CompileExp exp => SequentialCMD exp VHDL a -> VHDL a
compileSequential (Local i k e) =
  do v <- compEM e
     t <- compTM e
     M.addLocal $ case k of
       T.Constant -> M.declConstant i t v
       T.Signal   -> M.declSignal   i t v
       T.Variable -> M.declVariable i t v
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

--------------------------------------------------------------------------------
-- ** Concurrent commands offered by VHDL

data ConcurrentCMD exp (prog :: * -> *) a
  where
    Global
      :: PredicateExp exp a  
      => Identifier
      -> Kind
      -> Maybe (exp a)
      -> ConcurrentCMD exp prog ()

    Process
      :: Label
      -> [Identifier]
      -> prog ()
      -> ConcurrentCMD exp prog ()

instance MapInstr (ConcurrentCMD exp)
  where
    imap _ (Global  i k e)  = Global  i k e
    imap f (Process l is p) = Process l is $ f p

type instance IExp (ConcurrentCMD e)       = e
type instance IExp (ConcurrentCMD e :+: i) = e

--------------------------------------------------------------------------------

-- | Declare global constands/variables/files
constantG, signalG, variableG, fileG 
  :: (ConcurrentCMD (IExp instr) :<: instr, PredicateExp (IExp instr) a)
  => Identifier
  -> Maybe (IExp instr a)
  -> ProgramT instr m ()
constantG i = singleE . Global i T.Constant
signalG   i = singleE . Global i T.Signal
variableG i = singleE . Global i T.Variable
fileG     i = singleE . Global i T.File

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
compileConcurrent (Global i k e) =
  do v <- compEM e
     t <- compTM e
     M.addGlobal $ case k of
       T.Constant -> M.declConstant i t v
       T.Signal   -> M.declSignal   i t v 
       T.Variable -> M.declVariable i t v
compileConcurrent (Process l is p) =
  do (a, process) <- M.inProcess l is p
     M.addConcurrent (V.ConProcess process)
     return a

--------------------------------------------------------------------------------
-- ** Entity declaration related commands offered by VHDL

data DeclKind = Port | Generic

data HeaderCMD exp (prog :: * -> *) a
  where
    Declare
      :: PredicateExp exp a
      => DeclKind
      -> Identifier
      -> Kind
      -> Mode
      -> Maybe (exp a)
      -> HeaderCMD exp prog Identifier

    Architecture
      :: String
      -> prog a
      -> HeaderCMD exp prog a

instance MapInstr (HeaderCMD exp)
  where
    imap _ (Declare d i k m e) = Declare d i k m e
    imap f (Architecture s p)  = Architecture s (f p)

type instance IExp (HeaderCMD e)       = e
type instance IExp (HeaderCMD e :+: i) = e

--------------------------------------------------------------------------------

-- | Declare constands/signal/variables/files ports and generics
constantPort, constantGeneric, signalPort, signalGeneric, variablePort, variableGeneric, filePort, fileGeneric
  :: (HeaderCMD (IExp instr) :<: instr, PredicateExp (IExp instr) a)
  => Identifier
  -> Mode
  -> Maybe (IExp instr a)
  -> ProgramT instr m Identifier
constantPort    i m = singleE . Declare Port    i T.Constant m
constantGeneric i m = singleE . Declare Generic i T.Constant m
signalPort      i m = singleE . Declare Port    i T.Signal   m
signalGeneric   i m = singleE . Declare Generic i T.Signal   m
variablePort    i m = singleE . Declare Port    i T.Variable m
variableGeneric i m = singleE . Declare Generic i T.Variable m
filePort        i m = singleE . Declare Port    i T.File     m
fileGeneric     i m = singleE . Declare Generic i T.File     m

-- | Declares a clock input port
clock :: forall instr m. (HeaderCMD (IExp instr) :<: instr, PredicateExp (IExp instr) Bool)
      => ProgramT instr m Identifier
clock = signalPort (Ident "clk") In (Nothing :: Maybe ((IExp instr) Bool))

-- | Declare an architecture
architecture
  :: (HeaderCMD (IExp instr) :<: instr)
  => String
  -> ProgramT instr m a
  -> ProgramT instr m a
architecture name = singleE . Architecture name

--------------------------------------------------------------------------------

compileHeader :: CompileExp exp => HeaderCMD exp VHDL a -> VHDL a
compileHeader (Declare Port i k m e) =
  do v <- compEM e
     t <- compTM e
     M.addPort $ case k of
       T.Constant -> M.interfaceConstant i   t v
       T.Signal   -> M.interfaceSignal   i m t v
       T.Variable -> M.interfaceVariable i m t v
     return i
compileHeader (Declare Generic i k m e) =
  do v <- compEM e
     t <- compTM e
     M.addGeneric $ case k of
       T.Constant -> M.interfaceConstant i   t v
       T.Signal   -> M.interfaceSignal   i m t v
       T.Variable -> M.interfaceVariable i m t v
     return i
compileHeader (Architecture name prg) =
  do M.inArchitecture name prg

--------------------------------------------------------------------------------
