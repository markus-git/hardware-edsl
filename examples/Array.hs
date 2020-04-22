{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Arrays where

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
  :+: ProcessCMD
  :+: VHDLCMD

type HProg = Program CMD (Param2 HExp HType)

type HSig  = Sig CMD HExp HType Identity

--------------------------------------------------------------------------------

arrays :: HProg ()
arrays =
  do a :: Array  Word8 <- newArray  20
     b :: VArray Word8 <- newVArray 10

     setArray  a 0 0
     setVArray b 1 1

     resetArray a 1

     va :: HExp Word8 <- getArray  a 0
     vb :: HExp Word8 <- getVArray b 1

     setArray  a 0 (va + 2 - vb)
     setVArray b 1 (vb + 2 - va)

     copyArray (a, 0) (a, 2) 4

--------------------------------------------------------------------------------

test = icompile arrays

--------------------------------------------------------------------------------
