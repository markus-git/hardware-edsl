{-# LANGUAGE FlexibleContexts #-}

module Language.Embedded.Hardware.Command
  ( compile
  , icompile
  , runIO

  , wcompile

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

import qualified Language.VHDL          as VHDL -- temp
import qualified Language.Embedded.VHDL as VHDL -- temp

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

-- | Temp.
wcompile :: (Interp instr VHDL, HFunctor instr) => Program instr () -> IO ()
wcompile = putStrLn . show . prettyVHDL . wrap . interpret
  where
    wrap :: VHDL () -> VHDL ()
    wrap v = do
      VHDL.entity (VHDL.Ident "empty") (return ())
      VHDL.architecture (VHDL.Ident "empty") (VHDL.Ident "behavioural") $
        do l <- VHDL.newLabel
           s <- snd <$> VHDL.inProcess l [] v
           VHDL.addConcurrent $ VHDL.ConProcess s

--------------------------------------------------------------------------------
