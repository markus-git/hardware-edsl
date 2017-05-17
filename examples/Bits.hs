{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds        #-}

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

bits :: HProg ()
bits =
  do let zero = litE (bitFromInteger 0) :: HExp (Bits 2)
         one  = litE (bitFromInteger 1) :: HExp (Bits 2)
         two  = litE (bitFromInteger 2) :: HExp (Bits 4)

         high = litE True  :: HExp Bit
         low  = litE False :: HExp Bit

     ----------------------------------------
     -- Bit vectors.
     --
     a <- initVariable zero :: HProg (Variable (Bits 2))
     b <- initVariable one  :: HProg (Variable (Bits 2))

     u <- getVariable a :: HProg (HExp (Bits 2))
     v <- getVariable b :: HProg (HExp (Bits 2))

     setVariable a (u + v)
     setVariable b (u - v)

--------------------------------------------------------------------------------

test = icompile bits

--------------------------------------------------------------------------------
