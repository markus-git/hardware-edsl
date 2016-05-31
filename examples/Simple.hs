{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds        #-}

module Simple where

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
-- * ...
--------------------------------------------------------------------------------

-- | Command set used for our 'simple' programs.
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
-- ** ...

simple :: HProg ()
simple =
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

     ----------------------------------------
     -- Clock stuff.
     --
     s <- initSignal high :: HProg (Signal Bit)     
     risingEdge s $ setSignal s low

     ----------------------------------------
     -- Ranges.
     --
     d <- initSignal two  :: HProg (Signal (Bits 4))

     let low  = litE 0 :: HExp Integer
         high = litE 1 :: HExp Integer
     
     r <- getRange low high d
     setRange (low + 1) (high + 1) d r
     
     return ()

testSimgle = icompile simple

--------------------------------------------------------------------------------
