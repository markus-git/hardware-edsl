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
-- **

data SequentialCMD (exp :: * -> *) (prog :: * -> *) a
  where
    Process :: Label -> prog () -> SequentialCMD exp prog ()

instance MapInstr (SequentialCMD exp)
  where
    imap f (Process l prog) = Process l (f prog)

type instance IExp (SequentialCMD e)       = e
type instance IExp (SequentialCMD e :+: i) = e

--------------------------------------------------------------------------------



--------------------------------------------------------------------------------

compileSequential :: CompileExp exp => SequentialCMD exp prog a -> VHDL a
compileSequential (Process l prg) =
  do undefined

--------------------------------------------------------------------------------
-- **

data ConcurrentCMD exp (prog :: * -> *) a
  where
    Assign
      :: Identifier
      -> exp a
      -> ConcurrentCMD exp prog ()
           
    Local
      :: Typeable a
      => Identifier
      -> Kind
      -> Maybe (exp a)
      -> ConcurrentCMD exp prog ()

instance MapInstr (ConcurrentCMD exp)
  where
    imap _ (Assign r a)   = Assign  r a
    imap _ (Local  i k e) = Local   i k e

type instance IExp (ConcurrentCMD e)       = e
type instance IExp (ConcurrentCMD e :+: i) = e

--------------------------------------------------------------------------------

(<==) :: (ConcurrentCMD (IExp instr) :<: instr) => Identifier -> IExp instr a -> ProgramT instr m ()
(<==) i = singleE . Assign i

constantL, signalL, variableL, fileL
  :: (ConcurrentCMD (IExp instr) :<: instr, Typeable a)
  => Identifier
  -> Maybe (IExp instr a)
  -> ProgramT instr m ()
constantL i = singleE . Local i M.Constant
signalL   i = singleE . Local i M.Signal
variableL i = singleE . Local i M.Variable
fileL     i = singleE . Local i M.File

--------------------------------------------------------------------------------

compileConcurrent :: CompileExp exp => ConcurrentCMD exp prog a -> VHDL a
compileConcurrent (Assign i exp) =
  do v <- compE exp
     M.addSignalAssignment i v
compileConcurrent (Local i k e) =
  do v <- compEM e
     t <- compTM e
     void $ case k of
       M.Constant -> M.constantL i         t v
       M.Signal   -> M.signalL   i V.InOut t v
       M.Variable -> M.variableL i V.InOut t v
       M.File     -> M.fileL     i         t

compEM :: CompileExp exp => Maybe (exp a) -> VHDL (Maybe Expression)
compEM = maybe (return Nothing) (>>= return . Just) . fmap compE

compTM :: forall exp a. (CompileExp exp, Typeable a) => Maybe (exp a) -> VHDL Type
compTM _ = compT (undefined :: exp a)

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
