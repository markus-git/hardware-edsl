{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE DataKinds           #-}
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
-- * Example of a program that performs type casting.
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

casting :: HProg ()
casting =
  do a :: Variable Word8 <- initVariable 0
     b :: Variable Int8  <- initVariable 2
     c :: Variable Int16 <- initVariable 200

     av :: HExp Word8 <- getVariable a
     bv :: HExp Int8  <- getVariable b
     cv :: HExp Int16 <- getVariable c

     setVariable b (toSigned av)
     setVariable c (toSigned av)
     setVariable a (toUnsigned cv)

--------------------------------------------------------------------------------

test = icompile casting

--------------------------------------------------------------------------------
