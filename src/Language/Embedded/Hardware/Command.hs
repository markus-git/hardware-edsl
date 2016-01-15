{-# LANGUAGE FlexibleContexts #-}

module Language.Embedded.Hardware.Command
  ( compile
  , icompile
  , runIO

  , module CMD
  , module Language.Embedded.Hardware.Command.CMD
  , module Language.Embedded.Hardware.Command.Frontend
  , module Language.Embedded.Hardware.Command.Backend.VHDL
  ) where

import Language.Embedded.Hardware.Command.CMD as CMD (Signal, Variable, Array)
import Language.Embedded.Hardware.Command.CMD hiding (Signal, Variable, Array)
import Language.Embedded.Hardware.Command.Frontend
import Language.Embedded.Hardware.Command.Backend.VHDL

import Language.Embedded.VHDL (VHDL, prettyVHDL)

import Control.Monad.Operational.Higher

--------------------------------------------------------------------------------
-- * Compilation and evaluation.
--------------------------------------------------------------------------------

-- | Compile a program to VHDL code represented as a string.
compile :: (Interp instr VHDL, HFunctor instr) => Program instr a -> String
compile = show . prettyVHDL . interpret

-- | Compile a program to VHDL code and print it on the screen.
icompile :: (Interp instr VHDL, HFunctor instr) => Program instr a -> IO ()
icompile = putStrLn . compile

-- | Run a program in 'IO'.
runIO :: (Interp instr IO, HFunctor instr) => Program instr a -> IO a
runIO = interpret

--------------------------------------------------------------------------------
