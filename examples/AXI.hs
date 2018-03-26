{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AXI where

import Language.VHDL (Mode(..))
import Language.Embedded.Hardware

import Language.Embedded.Hardware.Interface.AXI

import Control.Monad.Identity
import Control.Monad.Operational.Higher
import Data.ALaCarte
import Data.Int
import Data.Word
import Text.PrettyPrint

import Prelude hiding (and, or, not)

--------------------------------------------------------------------------------
-- * AXI example.
--------------------------------------------------------------------------------

-- | Command set used for our programs.
type CMD =
      SignalCMD
  :+: VariableCMD
  :+: ArrayCMD
  :+: VArrayCMD
  :+: LoopCMD
  :+: ConditionalCMD
  :+: ComponentCMD
  :+: ProcessCMD
  :+: VHDLCMD

type HProg = Program CMD (Param2 HExp HType)

type HComp = Comp CMD HExp HType Identity

type HSig  = Sig CMD HExp HType Identity

--------------------------------------------------------------------------------

-- Simple adder.
ex ::Signal Word32 -> Signal Word32 -> Signal Word32 -> HProg ()
ex a b c =
  processR []
    (do setSignal c 0)
    (do va <- getSignal a
        vb <- getSignal b
        setSignal c (va + vb))

-- Adder's signature.
ex_sig :: HSig (
     Signal Word32
  -> Signal Word32
  -> Signal Word32
  -> ())
ex_sig =
  namedInput  "a"   $ \a ->
  namedInput  "b"   $ \b ->
  namedOutput "c"   $ \c ->
  ret $ ex a b c

--------------------------------------------------------------------------------

test2 = icompileAXILite ex_sig

--------------------------------------------------------------------------------
