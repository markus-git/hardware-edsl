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
-- * ...
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
-- ** Identity.

ident :: Integer -> Signal Bit -> Array Word16 -> Array Word16 -> HProg ()
ident size clk a b =
  process (clk .: []) $
    for 0 (value size - 1) $ \ix ->
      do tmp <- getArray a ix
         setArray b ix tmp

ident_sig :: Integer -> HSig (
     Signal Bit
  -> Array Word16
  -> Array Word16
  -> ())
ident_sig size =
  namedInput     "clk"    $ \clk ->
  namedInputArr  "a" size $ \a ->
  namedOutputArr "b" size $ \b ->
  ret (ident size clk a b)

ident_comp :: Integer -> HProg (HComp (
     Signal Bit
  -> Array Word16
  -> Array Word16
  -> ()))
ident_comp size = namedComponent "ident" (ident_sig size)

--------------------------------------------------------------------------------

test_ident = icompile (ident_comp 2)

--------------------------------------------------------------------------------
-- ** Addition of two 8-bit words.

-- Adder program.
adder :: Signal Bit -> Signal Word8 -> Signal Word8 -> Signal Word8 -> HProg ()
adder clk a b c =
  process (clk .: []) $
    do va <- getSignal a
       vb <- getSignal b
       setSignal c (va + vb)

-- An encoding of the adder's signature.
adder_sig :: HSig (
     Signal Bit
  -> Signal Word8
  -> Signal Word8
  -> Signal Word8
  -> ())
adder_sig =
  namedInput  "clk" $ \clk ->
  namedInput  "a"   $ \a ->
  namedInput  "b"   $ \b ->
  namedOutput "c"   $ \c ->
  ret (adder clk a b c)

-- An adder component given by its signature.
adder_comp :: HProg (HComp (
     Signal Bit
  -> Signal Word8
  -> Signal Word8
  -> Signal Word8
  -> ()))
adder_comp = namedComponent "adder" adder_sig

--------------------------------------------------------------------------------

test_adder = icompile adder_comp

--------------------------------------------------------------------------------
-- ** Point-wise multiplication of arrays over 8-bit words.

-- Multiplier program.
mult :: Integer -> Signal Bit -> Array Word8 -> Array Word8 -> Array Word8 -> HProg ()
mult size clk a b c =
  process (clk .: []) $
    for 0 (value size - 1) $ \ix ->
      do va <- getArray a ix
         vb <- getArray b ix
         setArray c ix (va * vb)

-- An encoding of the multiplier's signature.
mult_sig :: Integer -> HSig (
     Signal Bit
  -> Array Word8
  -> Array Word8
  -> Array Word8
  -> ())
mult_sig size =
  namedInput     "clk"    $ \clk ->
  namedInputArr  "a" size $ \a ->
  namedInputArr  "b" size $ \b ->
  namedOutputArr "c" size $ \c ->
  ret (mult size clk a b c)

-- A multiplier component given by its signature.
mult_comp :: Integer -> HProg (HComp (
     Signal Bit
  -> Array Word8
  -> Array Word8
  -> Array Word8
  -> ()))
mult_comp size = namedComponent "multiplier" (mult_sig size)

--------------------------------------------------------------------------------

test_mult = icompile (mult_comp 10)

--------------------------------------------------------------------------------
