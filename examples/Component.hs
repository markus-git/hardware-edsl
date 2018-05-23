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
  :+: ProcessCMD
  :+: VHDLCMD

type HProg = Program CMD (Param2 HExp HType)

type HComp = Comp CMD HExp HType Identity

type HSig  = Sig CMD HExp HType Identity

--------------------------------------------------------------------------------
-- ** Addition of two 8-bit words.

-- Adder program.
adder :: Signal Word8 -> Signal Word8 -> Signal Word8 -> HProg ()
adder a b c =
  processR []
    (do setSignal c 0)
    (do va <- getSignal a
        vb <- getSignal b
        setSignal c (va + vb))

-- An encoding of the adder's signature.
adder_sig :: HSig (
     Signal Word8
  -> Signal Word8
  -> Signal Word8
  -> ())
adder_sig =
  namedInput  "a" $ \a ->
  namedInput  "b" $ \b ->
  namedOutput "c" $ \c ->
  ret (adder a b c)

--------------------------------------------------------------------------------

test = icompileSig adder_sig

--------------------------------------------------------------------------------

-- An adder component given by its signature.
mult4_sig :: HSig (Signal Word8 -> Signal Word8 -> Signal Word8 -> ())
mult4_sig =
  input  $ \a ->
  input  $ \b ->
  output $ \c ->
  ret $ do
    adder <- namedComponent "adder" adder_sig
    tmp :: Signal Word8 <- initSignal 0
    portmap adder (a   +: b   +: tmp +: nil)
    portmap adder (tmp +: tmp +: tmp +: nil)
    val <- unsafeFreezeSignal tmp
    setSignal c val

--------------------------------------------------------------------------------

test2 = icompileSig mult4_sig

--------------------------------------------------------------------------------
