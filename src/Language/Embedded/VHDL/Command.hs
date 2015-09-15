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

import Language.Embedded.VHDL.Monad (VHDLT, VHDL, Type, Kind(..))
import Language.Embedded.VHDL.Interface
import qualified Language.Embedded.VHDL.Monad as M

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

compile :: (Interp instr VHDL, MapInstr instr) => Program instr a -> String
compile = show . M.prettyVHDL . interpret

--------------------------------------------------------------------------------

-- | Compile if 'exp' is set
compEM :: CompileExp exp => Maybe (exp a) -> VHDL (Maybe Expression)
compEM = maybe (return Nothing) (>>= return . Just) . fmap compE

-- | Compile hidden type
compTM :: forall exp a. (CompileExp exp, Typeable a) => Maybe (exp a) -> VHDL Type
compTM _ = compT (undefined :: exp a)

--------------------------------------------------------------------------------
-- **

data SequentialCMD (exp :: * -> *) (prog :: * -> *) a
  where
    Local
      :: Typeable a
      => Identifier
      -> Kind
      -> Maybe (exp a)
      -> SequentialCMD exp prog ()

    SAssignment
      :: Typeable a
      => Identifier
      -> Kind
      -> exp a
      -> SequentialCMD exp prog ()
    
instance MapInstr (SequentialCMD exp)
  where
    imap _ (Local i k e)       = Local i k e
    imap _ (SAssignment i k e) = SAssignment i k e

type instance IExp (SequentialCMD e)       = e
type instance IExp (SequentialCMD e :+: i) = e

--------------------------------------------------------------------------------

constantL, variableL, fileL
  :: (SequentialCMD (IExp instr) :<: instr, Typeable a)
  => Identifier
  -> Maybe (IExp instr a)
  -> ProgramT instr m ()
constantL i = singleE . Local i M.Constant
variableL i = singleE . Local i M.Variable
fileL     i = singleE . Local i M.File

(<==) :: (SequentialCMD (IExp instr) :<: instr, Typeable a) => Identifier -> IExp instr a -> ProgramT instr m ()
(<==) i = singleE . SAssignment i Signal

(==:) :: (SequentialCMD (IExp instr) :<: instr, Typeable a) => Identifier -> IExp instr a -> ProgramT instr m ()
(==:) i = singleE . SAssignment i Variable

--------------------------------------------------------------------------------

compileSequential :: CompileExp exp => SequentialCMD exp prog a -> VHDL a
compileSequential (Local i k e) =
  do v <- compEM e
     t <- compTM e
     M.addLocal $ case k of
       M.Constant -> M.declConstant i t v
       M.Signal   -> M.declSignal   i t v
       M.Variable -> M.declVariable i t v
compileSequential (SAssignment i k e) =
  do v <- compE e
     M.addSequential $ case k of
       Signal -> M.assignSignal   i v
       _      -> M.assignVariable i v

--------------------------------------------------------------------------------
-- **

data ConcurrentCMD exp (prog :: * -> *) a
  where
    Global           -- I should merge these into a 'Seq + Conc'-CMD type
      :: Typeable a  -- These do however also take a 'Mode' param...
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

constantG, signalG, variableG, fileG
  :: (ConcurrentCMD (IExp instr) :<: instr, Typeable a)
  => Identifier
  -> Maybe (IExp instr a)
  -> ProgramT instr m ()
constantG i = singleE . Global i M.Constant
signalG   i = singleE . Global i M.Signal
variableG i = singleE . Global i M.Variable
fileG     i = singleE . Global i M.File

process
  :: (ConcurrentCMD (IExp instr) :<: instr)
  => Label
  -> [Identifier]
  -> ProgramT instr m ()
  -> ProgramT instr m ()
process i is = singleE . Process i is

--------------------------------------------------------------------------------

compileConcurrent :: CompileExp exp => ConcurrentCMD exp VHDL a -> VHDL a
compileConcurrent (Global i k e) =
  do v <- compEM e
     t <- compTM e
     M.addGlobal $ case k of
       M.Constant -> M.declConstant i t v
       M.Signal   -> M.declSignal   i t v 
       M.Variable -> M.declVariable i t v
compileConcurrent (Process l is p) =
  do (a, process) <- M.inProcess l is p
     M.addConcurrent (V.ConProcess process)
     return a

--------------------------------------------------------------------------------
-- **

data DeclKind = Port | Generic

data HeaderCMD exp (prog :: * -> *) a
  where
    Declare
      :: Typeable a
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

constantPort, constantGeneric, signalPort, signalGeneric, variablePort, variableGeneric, filePort, fileGeneric 
  :: (HeaderCMD (IExp instr) :<: instr, Typeable a)
  => Identifier
  -> Mode
  -> Maybe (IExp instr a)
  -> ProgramT instr m Identifier
constantPort    i m = singleE . Declare Port    i M.Constant m
constantGeneric i m = singleE . Declare Generic i M.Constant m
signalPort      i m = singleE . Declare Port    i M.Signal   m
signalGeneric   i m = singleE . Declare Generic i M.Signal   m
variablePort    i m = singleE . Declare Port    i M.Variable m
variableGeneric i m = singleE . Declare Generic i M.Variable m
filePort        i m = singleE . Declare Port    i M.File     m
fileGeneric     i m = singleE . Declare Generic i M.File     m

-- | Declares a clock input port
clock :: forall instr m. (HeaderCMD (IExp instr) :<: instr) => ProgramT instr m Identifier
clock = signalPort (Ident "clk") In (Nothing :: Maybe ((IExp instr) Bool))

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
       M.Constant -> M.interfaceConstant i   t v
       M.Signal   -> M.interfaceSignal   i m t v
       M.Variable -> M.interfaceVariable i m t v
     return i
compileHeader (Declare Generic i k m e) =
  do v <- compEM e
     t <- compTM e
     M.addGeneric $ case k of
       M.Constant -> M.interfaceConstant i   t v
       M.Signal   -> M.interfaceSignal   i m t v
       M.Variable -> M.interfaceVariable i m t v
     return i
compileHeader (Architecture name prg) =
  do M.inArchitecture name prg

--------------------------------------------------------------------------------
