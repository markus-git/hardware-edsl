{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Component where

import Language.VHDL (Mode(..))
import Language.Embedded.Hardware

import Language.Embedded.Hardware.Interface.AXI

import Control.Monad.Identity
import Control.Monad.Operational.Higher
import Data.ALaCarte
import Data.Int
import Data.Word
import Text.PrettyPrint

import Prelude hiding (and, or, not, negate)

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
-- ** Addition of two 8-bit words.

-- Adder program.
adder :: Signal Bit -> Signal Bit -> Signal Word8 -> Signal Word8 -> Signal Word8 -> HProg ()
adder clk rst a b c =
  process clk rst [] (return ()) (do
    va <- getSignal a
    vb <- getSignal b
    setSignal c (va + vb))

-- An encoding of the adder's signature.
adder_sig :: HSig (
     Signal Bit
  -> Signal Bit
  -> Signal Word8
  -> Signal Word8
  -> Signal Word8
  -> ())
adder_sig =
  namedInput  "clk" $ \clk ->
  namedInput  "rst" $ \rst ->
  namedInput  "a"   $ \a ->
  namedInput  "b"   $ \b ->
  namedOutput "c"   $ \c ->
  ret (adder clk rst a b c)

-- An adder component given by its signature.
adder_comp :: HProg (HComp (
     Signal Bit
  -> Signal Bit
  -> Signal Word8
  -> Signal Word8
  -> Signal Word8
  -> ()))
adder_comp = namedComponent "adder" adder_sig

--------------------------------------------------------------------------------

test = icompile adder_comp

--------------------------------------------------------------------------------
