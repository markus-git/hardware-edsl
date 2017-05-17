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
  :+: StructuralCMD

type HProg = Program CMD (Param2 HExp HType)

type HSig  = Sig CMD HExp HType Identity

--------------------------------------------------------------------------------

integers :: HProg ()
integers =
  do arr :: VArray Word8 <- initVArray [0..10]

     setVArray 0 20 arr
     for (0) (10) $ \i ->
       do v <- getVArray (10 - i) arr
          setVArray i v arr

     v0 <- getVArray 0 arr
     v1 <- getVArray 1 arr

     iff (v0 `lte` 5)
       (setVArray 0 5 arr)
       (iff (v1 `lte` 5)
         (setVArray 1 5  arr)
         (setVArray 1 10 arr))
     
     return ()

--------------------------------------------------------------------------------

test = icompile integers

--------------------------------------------------------------------------------
