{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ConstraintKinds     #-}

{-# LANGUAGE GADTs #-}

module Language.Embedded.Hardware.Command
  (
  -- Regular hardware compilers.
    compile
  , icompile
  , runIO
  -- AXI compilers.
  , compileAXILite
  , icompileAXILite
  --
  , VHDL.Mode(..)
  --
  , module CMD
  , module Language.Embedded.Hardware.Command.CMD
  , module Language.Embedded.Hardware.Command.Frontend
  , module Language.Embedded.Hardware.Command.Backend.VHDL
  ) where

import Language.Embedded.Hardware.Command.CMD as CMD (Signal, Variable, Array)
import Language.Embedded.Hardware.Command.CMD hiding (Signal, Variable, Array)
import Language.Embedded.Hardware.Command.Frontend
import Language.Embedded.Hardware.Command.Backend.VHDL
import Language.Embedded.Hardware.Interface
import Language.Embedded.Hardware.Interface.AXI

import Language.Embedded.VHDL (VHDL, prettyVHDL)

import qualified Language.VHDL          as VHDL -- temp
import qualified Language.Embedded.VHDL as VHDL -- temp

import Control.Monad.Operational.Higher

import Control.Monad.Identity

import qualified GHC.Exts as GHC (Constraint)

--------------------------------------------------------------------------------
-- * Compilation and evaluation.
--------------------------------------------------------------------------------

-- | Compile a program to VHDL code represented as a string.
compile :: forall instr (exp :: * -> *) (pred :: * -> GHC.Constraint) a.
     ( Interp instr VHDLGen (Param2 exp pred)
     , HFunctor instr
     , ProcessCMD :<: instr
     , VHDLCMD :<: instr
     , pred Bool
     )
  => Program instr (Param2 exp pred) ()
  -> String
compile = show
        . VHDL.prettyVHDL
        . VHDL.wrapMain
        . flip runVHDLGen env
        . interpret
        . process []
  where
    env :: SignalEnv
    env  = SignalEnv {
        _clock = SignalC "clock"
      , _reset = SignalC "reset" }    
    -- todo: 'wrapMain' will add these ports.

-- | Compile a program to VHDL code and print it on the screen.
icompile :: forall instr (exp :: * -> *) (pred :: * -> GHC.Constraint) a.
     ( Interp instr VHDLGen (Param2 exp pred)
     , HFunctor instr
     , ProcessCMD :<: instr
     , VHDLCMD :<: instr
     , pred Bool
     )
  => Program instr (Param2 exp pred) ()
  -> IO ()
icompile = putStrLn . compile

-- | Run a program in 'IO'.
runIO :: forall instr (exp :: * -> *) (pred :: * -> GHC.Constraint) a
   . ( InterpBi instr IO (Param1 pred)
     , HBifunctor instr
     , EvaluateExp exp
     )
  => Program instr (Param2 exp pred) a
  -> IO a
runIO = interpretBi (return . evalE)

--------------------------------------------------------------------------------
-- Some extra compilers that might be handy to have.

compileAXILite :: forall instr (exp :: * -> *) (pred :: * -> GHC.Constraint) a .
  ( Interp instr VHDLGen (Param2 exp pred)
  , HFunctor instr
  , AXIPred instr exp pred
  )
  => Sig instr exp pred Identity a
  -> String
compileAXILite sig =
    show
  . VHDL.prettyVHDL
  . flip runVHDLGen env
  . interpret
  $ do a <- component sig
       b <- component (axi_light a)
       return ()
  where
    env :: SignalEnv
    env  = SignalEnv {
        _clock = SignalC "clock"
      , _reset = SignalC "reset" }    
    -- todo: 'wrapMain' will add these ports.

icompileAXILite :: forall instr (exp :: * -> *) (pred :: * -> GHC.Constraint) a .
  ( Interp instr VHDLGen (Param2 exp pred)
  , HFunctor instr
  , AXIPred instr exp pred
  )
  => Sig instr exp pred Identity a
  -> IO ()
icompileAXILite = putStrLn . compileAXILite

--------------------------------------------------------------------------------
