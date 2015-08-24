{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Embedded.VHDL.Command where

import Control.Monad.Operational.Compositional

import Language.VHDL (Identifier, Expression)
import Language.VHDL as V

import Language.Embedded.VHDL.Monad (VHDL, Kind, Type)
import Language.Embedded.VHDL.Interface
import qualified Language.Embedded.VHDL.Monad as M

import Control.Applicative

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
     show (pp decl) ++ "\n" ++ show (pp body)

--------------------------------------------------------------------------------
-- **

data ConcurrentCMD exp (prog :: * -> *) a
  where
    Assign :: Identifier -> exp a                           -> ConcurrentCMD exp prog ()
    Local  :: Identifier -> Kind  -> Type  -> Maybe (exp a) -> ConcurrentCMD exp prog ()

instance MapInstr (ConcurrentCMD exp)
  where
    imap _ (Assign r a)     = Assign r a
    imap _ (Local  i k e t) = Local  i k e t

type instance IExp (ConcurrentCMD e)       = e
type instance IExp (ConcurrentCMD e :+: i) = e

--------------------------------------------------------------------------------

(<==) :: (ConcurrentCMD (IExp instr) :<: instr) => Identifier -> IExp instr a -> ProgramT instr m ()
(<==) i = singleE . Assign i

constantL, signalL, variableL, fileL
  :: (ConcurrentCMD (IExp instr) :<: instr)
  => Identifier
  -> Type
  -> Maybe (IExp instr a)
  -> ProgramT instr m ()
constantL i t = singleE . Local i M.Constant t
signalL   i t = singleE . Local i M.Signal   t
variableL i t = singleE . Local i M.Variable t
fileL     i t = singleE . Local i M.File     t

--------------------------------------------------------------------------------

compileConcurrent :: CompileExp exp => ConcurrentCMD exp prog a -> VHDL a
compileConcurrent (Assign i exp) =
  do v <- compE exp
     M.addAssignment i v
compileConcurrent (Local i k t e) =
  do v <- compEM e
     void $ case k of
       M.Constant -> M.constantL i          t v
       M.Signal   -> M.signalL   i V.Buffer t v
       M.Variable -> M.variableL i V.Buffer t v
       M.File     -> M.fileL     i          t

compEM :: CompileExp exp => Maybe (exp a) -> VHDL (Maybe Expression)
compEM = maybe (return Nothing) (>>= return . Just) . fmap compE

--------------------------------------------------------------------------------
-- **

data HeaderCMD exp (prog :: * -> *) a
  where
    Port    :: String -> Kind -> Type -> Maybe (exp a) -> HeaderCMD exp prog Identifier
    Generic :: String -> Kind -> Type -> Maybe (exp a) -> HeaderCMD exp prog Identifier

instance MapInstr (HeaderCMD exp)
  where
    imap _ (Port    s k t e) = Port    s k t e
    imap _ (Generic s k t e) = Generic s k t e

type instance IExp (HeaderCMD e)       = e
type instance IExp (HeaderCMD e :+: i) = e

--------------------------------------------------------------------------------

constant, signal, variable, file
  :: (HeaderCMD (IExp instr) :<: instr)
  => String
  -> Type
  -> Maybe (IExp instr a)
  -> ProgramT instr m Identifier
constant s t = singleE . Port s M.Constant t
signal   s t = singleE . Port s M.Signal   t
variable s t = singleE . Port s M.Variable t
file     s t = singleE . Port s M.File     t

constantG, signalG, variableG, fileG
  :: (HeaderCMD (IExp instr) :<: instr)
  => String
  -> Type
  -> Maybe (IExp instr a)
  -> ProgramT instr m Identifier
constantG s t = singleE . Generic s M.Constant t
signalG   s t = singleE . Generic s M.Signal   t
variableG s t = singleE . Generic s M.Variable t
fileG     s t = singleE . Generic s M.File     t

--------------------------------------------------------------------------------

compileHeader :: CompileExp exp => HeaderCMD exp prog a -> VHDL a
compileHeader (Port s k t e) =
  do let i = Ident s
     v <- compEM e
     case k of
       M.Constant -> M.constant i         t v
       M.Signal   -> M.signal   i V.InOut t v
       M.Variable -> M.variable i V.InOut t v
       M.File     -> M.file     i         t
compileHeader (Generic s k t e) =
  do let i = Ident s
     v <- compEM e
     case k of
       M.Constant -> M.constantG i         t v
       M.Signal   -> M.signalG   i V.InOut t v
       M.Variable -> M.variableG i V.InOut t v
       M.File     -> M.fileG     i         t

--------------------------------------------------------------------------------
