{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AXI where

import Language.VHDL (Mode(..))
import Language.Embedded.Hardware

import Language.Embedded.Hardware.Common.AXI

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

-- Multiplier program.
ex :: Integer -> Array Word8 -> Array Word8 -> Signal Word32 -> HProg ()
ex size a b c =
  process (a .: b .: []) $ do
    -- init. temp. variable.
    sum <- initVariable 0
    -- calc.
    for 0 (value size - 1) (\ix -> do
      va  <- getArray a ix
      vb  <- getArray b ix
      old <- getVariable sum
      setVariable sum (old + toUnsigned (va * vb)))
    -- set output.
    out <- unsafeFreezeVariable sum
    setSignal c out

-- An encoding of the multiplier's signature.
ex_sig :: Integer -> HSig (
     Array  Word8
  -> Array  Word8
  -> Signal Word32
  -> ())
ex_sig size =
  namedInputArr  "a" size $ \a ->
  namedInputArr  "b" size $ \b ->
  namedOutput    "c"      $ \c ->
  ret (ex size a b c)

-- A multiplier component given by its signature.
ex_comp :: Integer -> HProg (HComp (
     Array  Word8
  -> Array  Word8
  -> Signal Word32
  -> ()))
ex_comp size = namedComponent "example" (ex_sig size)

--------------------------------------------------------------------------------
-- ** AXI-lite wrapping of example.

axi comp = namedComponent "axi_wrapper" (axi_light_signature comp)

axi_comp :: HProg ()
axi_comp =
  do c <- ex_comp 2
     a <- axi c
     return ()

--------------------------------------------------------------------------------

test = icompile axi_comp

--------------------------------------------------------------------------------
