{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Language.Embedded.VHDL.Command where

import Control.Monad.Operational.Compositional

import Language.VHDL (Identifier, Label, Expression)
import Language.VHDL as V

import Language.Embedded.VHDL.Monad (VHDL, Kind, Type)
import Language.Embedded.VHDL.Interface
import qualified Language.Embedded.VHDL.Monad as M

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
compile prog = let (decl, body) = M.runVHDL (M.behavioural "test") (interpret prog) in
     show (pp decl) ++ "\n\n" ++ show (pp body)

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
    LocalS
      :: Typeable a
      => Identifier
      -> Kind
      -> Maybe (exp a)
      -> SequentialCMD exp prog ()

    AssignSignal
      :: Typeable a
      => Identifier
      -> exp a
      -> SequentialCMD exp prog ()

    AssignVariable
      :: Typeable a
      => Identifier
      -> exp a
      -> SequentialCMD exp prog ()
    
instance MapInstr (SequentialCMD exp)
  where
    imap _ (LocalS i k e)       = LocalS i k e
    imap _ (AssignSignal i e)   = AssignSignal i e
    imap _ (AssignVariable i e) = AssignVariable i e

type instance IExp (SequentialCMD e)       = e
type instance IExp (SequentialCMD e :+: i) = e

--------------------------------------------------------------------------------

constantL, variableL, fileL
  :: (SequentialCMD (IExp instr) :<: instr, Typeable a)
  => Identifier
  -> Maybe (IExp instr a)
  -> ProgramT instr m ()
constantL i = singleE . LocalS i M.Constant
variableL i = singleE . LocalS i M.Variable
fileL     i = singleE . LocalS i M.File

(<==) :: (SequentialCMD (IExp instr) :<: instr, Typeable a) => Identifier -> IExp instr a -> ProgramT instr m ()
(<==) i = singleE . AssignSignal i

(==:) :: (SequentialCMD (IExp instr) :<: instr, Typeable a) => Identifier -> IExp instr a -> ProgramT instr m ()
(==:) i = singleE . AssignVariable i

--------------------------------------------------------------------------------

compileSequential :: CompileExp exp => SequentialCMD exp prog a -> VHDL a
compileSequential (LocalS i k e) =
  do v <- compEM e
     t <- compTM e
     void $ case k of
       M.Constant -> M.constantL i t v
       M.Signal   -> M.signalL   i t v
       M.Variable -> M.variableL i t v
       M.File     -> M.fileL     i t Nothing
compileSequential (AssignSignal i e) =
  do v <- compE e
     M.seqSignalAssignment i v
compileSequential (AssignVariable i e) =
  do v <- compE e
     M.seqVariableAssignment i v

--------------------------------------------------------------------------------
-- **

data ConcurrentCMD exp (prog :: * -> *) a
  where
    LocalC           -- I should merge these into a 'Seq + Conc'-CMD type
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
    imap f (Process l is p) = Process l is $ f p
    imap _ (LocalC  i k e)  = LocalC  i k e

type instance IExp (ConcurrentCMD e)       = e
type instance IExp (ConcurrentCMD e :+: i) = e

--------------------------------------------------------------------------------

constantCL, signalCL, variableCL, fileCL
  :: (ConcurrentCMD (IExp instr) :<: instr, Typeable a)
  => Identifier
  -> Maybe (IExp instr a)
  -> ProgramT instr m ()
constantCL i = singleE . LocalC i M.Constant
signalCL   i = singleE . LocalC i M.Signal
variableCL i = singleE . LocalC i M.Variable
fileCL     i = singleE . LocalC i M.File

process
  :: (ConcurrentCMD (IExp instr) :<: instr)
  => Label
  -> [Identifier]
  -> ProgramT instr m ()
  -> ProgramT instr m ()
process i is = singleE . Process i is

--------------------------------------------------------------------------------

compileConcurrent :: CompileExp exp => ConcurrentCMD exp VHDL a -> VHDL a
compileConcurrent (LocalC i k e) =
  do v <- compEM e
     t <- compTM e
     void $ case k of
       M.Constant -> M.constantL i  t v
       M.Signal   -> M.signalL   i  t v
       M.Variable -> M.variableL i  t v
       M.File     -> M.fileL     i  t Nothing
compileConcurrent (Process l is p) =
  do M.newProcess   l is
     M.enterProcess l
     p
     M.enterGlobal

--------------------------------------------------------------------------------
-- **

data DeclKind = Port | Generic

data HeaderCMD exp (prog :: * -> *) a
  where
    Decl :: Typeable a
         => DeclKind
         -> Identifier
         -> Kind
         -> Mode
         -> Maybe (exp a)
         -> HeaderCMD exp prog Identifier

instance MapInstr (HeaderCMD exp)
  where
    imap _ (Decl d i k m e) = Decl d i k m e

type instance IExp (HeaderCMD e)       = e
type instance IExp (HeaderCMD e :+: i) = e

--------------------------------------------------------------------------------

constant, signal, variable, file
  :: (HeaderCMD (IExp instr) :<: instr, Typeable a)
  => Identifier
  -> Mode
  -> Maybe (IExp instr a)
  -> ProgramT instr m Identifier
constant i m = singleE . Decl Port i M.Constant m
signal   i m = singleE . Decl Port i M.Signal   m
variable i m = singleE . Decl Port i M.Variable m
file     i m = singleE . Decl Port i M.File     m

constantG, signalG, variableG, fileG
  :: (HeaderCMD (IExp instr) :<: instr, Typeable a)
  => Identifier
  -> Mode
  -> Maybe (IExp instr a)
  -> ProgramT instr m Identifier
constantG i m = singleE . Decl Generic i M.Constant m
signalG   i m = singleE . Decl Generic i M.Signal   m
variableG i m = singleE . Decl Generic i M.Variable m
fileG     i m = singleE . Decl Generic i M.File     m

--------------------------------------------------------------------------------

compileHeader :: CompileExp exp => HeaderCMD exp prog a -> VHDL a
compileHeader (Decl Port i k m e) =
  do v <- compEM e
     t <- compTM e
     case k of
       M.Constant -> M.constant i   t v
       M.Signal   -> M.signal   i m t v
       M.Variable -> M.variable i m t v
       M.File     -> M.file     i   t
compileHeader (Decl Generic i k m e) =
  do v <- compEM e
     t <- compTM e
     case k of
       M.Constant -> M.constantG i   t v
       M.Signal   -> M.signalG   i m t v
       M.Variable -> M.variableG i m t v
       M.File     -> M.fileG     i   t

--------------------------------------------------------------------------------
