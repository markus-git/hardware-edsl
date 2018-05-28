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

-- Adder.
add_impl :: Signal Word32 -> Signal Word32 -> Signal Word32 -> HProg ()
add_impl a b c =
  processR []
    (do setSignal c 0)
    (do va <- getSignal a
        vb <- getSignal b
        setSignal c (va + vb))

-- Adder's signature.
add_sig :: HSig (
     Signal Word32
  -> Signal Word32
  -> Signal Word32
  -> ())
add_sig =
  namedInput  "a" $ \a ->
  namedInput  "b" $ \b ->
  namedOutput "c" $ \c ->
  ret $ add_impl a b c

-- Connect the adder's signature to an AXI-lite interface and compile.
test = icompileAXILite add_sig

--------------------------------------------------------------------------------

-- Reverse, assume 10 elements in each array.
rev_impl :: Array Word32 Word32 -> Array Word32 Word32 -> HProg ()
rev_impl a b =
  processR []
    (do resetArray b 0)
    (do for 0 9 $ \ix ->
          do va <- getArray a ix
             setArray b (9 - ix) va)

rev_sig :: HSig (
     Array Word32 Word32
  -> Array Word32 Word32
  -> ())
rev_sig =
  namedInputArray  "a" 10 $ \a ->
  namedOutputArray "b" 10 $ \b ->
  ret $ rev_impl a b

test2 = icompileAXILite rev_sig

--------------------------------------------------------------------------------
