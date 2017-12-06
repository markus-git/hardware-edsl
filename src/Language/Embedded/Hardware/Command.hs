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
  , compileWrap
  , icompileWrap
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
     (Interp instr VHDL (Param2 exp pred), HFunctor instr)
  => Program instr (Param2 exp pred) a
  -> String
compile = show . VHDL.prettyVHDL . VHDL.wrapMain . interpret

-- | Compile a program to VHDL code and print it on the screen.
icompile :: forall instr (exp :: * -> *) (pred :: * -> GHC.Constraint) a.
     (Interp instr VHDL (Param2 exp pred), HFunctor instr)
  => Program instr (Param2 exp pred) a
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
  ( Interp instr VHDL (Param2 exp pred)
  , HFunctor instr
  , AXIPred instr exp pred
  )
  => Sig instr exp pred Identity (Signal Bool -> Signal Bool -> a)
  -> String
compileAXILite sig = compile $
  do cmp <- component sig
     axi <- component (axi_light cmp)
     return ()

icompileAXILite :: forall instr (exp :: * -> *) (pred :: * -> GHC.Constraint) a .
  ( Interp instr VHDL (Param2 exp pred)
  , HFunctor instr
  , AXIPred instr exp pred
  )
  => Sig instr exp pred Identity (Signal Bool -> Signal Bool -> a)
  -> IO ()
icompileAXILite = putStrLn . compileAXILite

--------------------------------------------------------------------------------

-- | Wraps a program in a clocked process and then compiles it.
compileWrap :: forall instr (exp :: * -> *) (pred :: * -> GHC.Constraint) a .
     ( Interp instr VHDL (Param2 exp pred)
     , HFunctor instr
     , ProcessCMD :<: instr
     , VHDLCMD    :<: instr
     , pred Bool
     )
  => Program instr (Param2 exp pred) ()
  -> String
compileWrap = compile . wrap

-- | Wraps a program in a clocked process and then outputs its code.
icompileWrap :: forall instr (exp :: * -> *) (pred :: * -> GHC.Constraint) a.
     ( Interp instr VHDL (Param2 exp pred)
     , HFunctor instr
     , ProcessCMD :<: instr
     , VHDLCMD    :<: instr
     , pred Bool
     )
  => Program instr (Param2 exp pred) ()
  -> IO ()
icompileWrap = icompile . wrap

-- | Wrap a program in a process.
wrap :: forall instr (exp :: * -> *) (pred :: * -> GHC.Constraint) a .
     ( ProcessCMD :<: instr
     , VHDLCMD    :<: instr
     , pred Bool
     )
  => Program instr (Param2 exp pred) ()
  -> Program instr (Param2 exp pred) ()
wrap body = do
  clk <- newNamedPort "clk" VHDL.In
  process clk [] body

--------------------------------------------------------------------------------
