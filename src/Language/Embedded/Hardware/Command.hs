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
  -- hardware compilers without process wrapping.
  , compileSig
  , icompileSig
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
        . flip runVHDLGen emptyEnv
        . interpret
        . process []

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

compileSig :: forall instr (exp :: * -> *) (pred :: * -> GHC.Constraint) a .
  ( Interp instr VHDLGen (Param2 exp pred)
  , HFunctor instr
  , ComponentCMD :<: instr
  )
  => Sig instr exp pred Identity a
  -> String
compileSig =
    show
  . VHDL.prettyVHDL
  . flip runVHDLGen emptyEnv
  . interpret
  . component

icompileSig :: forall instr (exp :: * -> *) (pred :: * -> GHC.Constraint) a .
  ( Interp instr VHDLGen (Param2 exp pred)
  , HFunctor instr
  , ComponentCMD :<: instr
  )
  => Sig instr exp pred Identity a
  -> IO ()
icompileSig = putStrLn . compileSig

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
  . flip runVHDLGen emptyEnv
  . interpret
  $ do comp <- component sig
       clockedComponent "AXI" "S_AXI_ACLK" "S_AXI_ARESETN" (axi_light comp)

icompileAXILite :: forall instr (exp :: * -> *) (pred :: * -> GHC.Constraint) a .
  ( Interp instr VHDLGen (Param2 exp pred)
  , HFunctor instr
  , AXIPred instr exp pred
  )
  => Sig instr exp pred Identity a
  -> IO ()
icompileAXILite = putStrLn . compileAXILite

--------------------------------------------------------------------------------
