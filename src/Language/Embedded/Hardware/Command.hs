{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ConstraintKinds     #-}

module Language.Embedded.Hardware.Command
  ( compile
  , icompile
  , runIO

  , VHDL.Mode(..)

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

import Language.Embedded.VHDL (VHDL, prettyVHDL)

import qualified Language.VHDL          as VHDL -- temp
import qualified Language.Embedded.VHDL as VHDL -- temp

import Control.Monad.Operational.Higher

import qualified GHC.Exts as GHC (Constraint)

--------------------------------------------------------------------------------
-- * Compilation and evaluation.
--------------------------------------------------------------------------------

-- | Compile a program to VHDL code represented as a string.
compile
  :: forall instr (exp :: * -> *) (pred :: * -> GHC.Constraint) a.
     ( Interp instr VHDL (Param2 exp pred)
     , HFunctor instr
     , pred Bool
     , StructuralCMD :<: instr
     , SignalCMD     :<: instr)
  => Program instr (Param2 exp pred) a
  -> String
compile = show . prettyVHDL . interpret . wrap

-- | Compile a program to VHDL code and print it on the screen.
icompile
  :: forall instr (exp :: * -> *) (pred :: * -> GHC.Constraint) a.
     ( Interp instr VHDL (Param2 exp pred)
     , HFunctor instr
     , pred Bool
     , StructuralCMD :<: instr
     , SignalCMD     :<: instr)
  => Program instr (Param2 exp pred) a
  -> IO ()
icompile = putStrLn . compile

-- | Run a program in 'IO'.
runIO
  :: forall instr (exp :: * -> *) (pred :: * -> GHC.Constraint) a
   . (InterpBi instr IO (Param1 pred), HBifunctor instr, EvaluateExp exp)
  => Program instr (Param2 exp pred) a
  -> IO a
runIO = interpretBi (return . evalE)

--------------------------------------------------------------------------------

-- | Wrap a hardware program in an empty architecture/entity.
wrap
  :: forall instr (exp :: * -> *) (pred :: * -> GHC.Constraint) a.
     ( StructuralCMD :<: instr
     , SignalCMD     :<: instr
     , pred Bool)
  => Program instr (Param2 exp pred) a
  -> Program instr (Param2 exp pred) ()
wrap body = do
  clk <- entity "main" $
    newExactPort "clk" VHDL.In :: Program instr (Param2 exp pred) (Signal Bool)
  architecture "main" "behavioural" $
    process (clk .: []) $
      body >> return ()

--------------------------------------------------------------------------------
