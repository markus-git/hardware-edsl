{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds        #-}

{-# LANGUAGE ScopedTypeVariables #-}

module Bits where

import Language.VHDL (Mode(..))
import Language.Embedded.Hardware

import Control.Monad.Identity
import Control.Monad.Operational.Higher
import Data.ALaCarte
import Data.Int
import Data.Word
import Text.PrettyPrint

import Prelude hiding (and, or, not)

--------------------------------------------------------------------------------
-- * Example of a program that words with variable length bit arrarys.
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

type HProg = Program CMD (Param2 HExp HType)

type HSig  = Sig CMD HExp HType Identity

--------------------------------------------------------------------------------

bits :: Signal Bit -> Signal Bit -> HProg ()
bits clk rst =
  do ----------------------------------------
     -- Bit vectors.
     --
     let zero = litE (bitFromInteger 0) :: HExp (Bits 2)
         one  = litE (bitFromInteger 1) :: HExp (Bits 2)

     a :: Variable (Bits 2) <- initVariable zero
     b :: Variable (Bits 2) <- initVariable one

     u :: HExp (Bits 2) <- getVariable a
     v :: HExp (Bits 2) <- getVariable b

     setVariable a (u + v)
     setVariable b (u - v)

     ----------------------------------------
     -- Bits with an array.
     let two  = litE (bitFromInteger 2) :: HExp (Bits 8)
     
     c :: Variable (Bits 8)  <- initVariable two
     d :: Array    (Bits 32) <- newArray 2

     x <- unsafeFreezeVariable c
     setArray d 0 (fromBits x)

--------------------------------------------------------------------------------

test = icompileWrap bits

--------------------------------------------------------------------------------
