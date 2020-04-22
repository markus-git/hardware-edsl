{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Int where

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
  :+: ProcessCMD
  :+: VHDLCMD

type HProg = Program CMD (Param2 HExp HType)

type HSig  = Sig CMD HExp HType Identity

--------------------------------------------------------------------------------

integers :: HProg ()
integers =
  do arr :: VArray   Word8 <- initVArray [0..10]
     ref :: Variable Word8 <- initVariable 10

     setVArray arr 0 20
     for 0 10 $ \i ->
       do v <- getVArray arr (10 - i)
          setVArray arr i v

     v0 <- getVArray arr 0
     v1 <- getVArray arr 1

     iff (v0 `lte` 5)
       (setVArray arr 0 5)
       (iff (v1 `lte` 5)
         (setVArray arr 1 5)
         (setVArray arr 1 10))

--------------------------------------------------------------------------------

test = icompile integers

--------------------------------------------------------------------------------
