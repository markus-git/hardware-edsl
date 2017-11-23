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
  :+: StructuralCMD
  :+: VHDLCMD

type HProg = Program CMD (Param2 HExp HType)

type HComp = Comp CMD HExp HType Identity

type HSig  = Sig CMD HExp HType Identity

--------------------------------------------------------------------------------
-- ** Example component.

-- Simple adder.
ex :: Signal Word32 -> Signal Word32 -> Signal Word32 -> HProg ()
ex a b c =
  process (a .: b .: []) $ do
    va <- getSignal a
    vb <- getSignal b
    setSignal c (va + vb)

--------------------------------------------------------------------------------

-- Adder's signature.
ex_sig :: HSig (
     Signal Word32
  -> Signal Word32
  -> Signal Word32
  -> ())
ex_sig =
  namedInput  "a" $ \a ->
  namedInput  "b" $ \b ->
  namedOutput "c" $ \c ->
  ret $ ex a b c

--------------------------------------------------------------------------------

-- Adder component.
ex_comp :: HProg (HComp (
     Signal Word32
  -> Signal Word32
  -> Signal Word32
  -> ()))
ex_comp = namedComponent "example" ex_sig

--------------------------------------------------------------------------------

-- Adder component wrapped in an AXI-lite interface.
ex_axi :: HProg ()
ex_axi =
  ex_comp >>= void . namedComponent "axi_wrapper" . axi_light

--------------------------------------------------------------------------------

test = icompile ex_axi

test2 = icompileAXILite ex_sig

--------------------------------------------------------------------------------
