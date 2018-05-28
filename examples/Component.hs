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

--
import qualified Language.Embedded.VHDL as VHDL (prettyVHDL)
--

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

plus1_impl :: Signal Word8 -> Signal Word8 -> HProg ()
plus1_impl a b =
  processR []
    (do setSignal b 0)
    (do va <- getSignal a; setSignal b (va + 1))

plus1_sig :: HSig (Signal Word8 -> Signal Word8 -> ())
plus1_sig =
  namedInput  "a" $ \a ->
  namedOutput "b" $ \b ->
  ret $ plus1_impl a b

test = icompileSig plus1_sig

--------------------------------------------------------------------------------

ex1_sig :: HComp (Signal Word8 -> Signal Word8 -> ()) -> HSig ()
ex1_sig comp = ret $
  do a <- initSignal 0
     portmap comp (a +: a +: nil)

test1 =
    putStrLn
  $ show
  $ VHDL.prettyVHDL
  $ flip runVHDLGen emptyEnv
  $ interpret
  $ do comp <- component plus1_sig
       component $ ex1_sig comp

--------------------------------------------------------------------------------

ex2_sig :: HSig ()
ex2_sig = ret $
  do comp <- component plus1_sig
     a    <- initSignal 0
     portmap comp (a +: a +: nil)

test2 = icompileSig ex2_sig

--------------------------------------------------------------------------------
