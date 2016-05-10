{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

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
--  :+: ArrayCMD
  :+: VArrayCMD
  :+: LoopCMD
  :+: ConditionalCMD
  :+: ComponentCMD
  :+: StructuralCMD

type HProg = Program CMD (Param2 HExp HType)

type HSig  = Sig CMD HExp HType Identity

--------------------------------------------------------------------------------
-- ** Example VHDL programs.

tests :: IO ()
tests = do
  putStrLn "\n### Looooop ###\n"
  icompile testLoop
--  putStrLn "\n### Identity ###\n"
--  icompile $ testIdentity
--  putStrLn "\n### Component ###\n"
--  icompile $ testComponent

--------------------------------------------------------------------------------

testLoop :: HProg ()
testLoop = do
  entity "loop" (return ())
  architecture "loop" "behav" $ do
    component looping
    return ()

looping :: HSig ()
looping = ret $ do
  return ()

--------------------------------------------------------------------------------
{-
testIdentity :: HProg ()
testIdentity = do
  (x, y) <- entity "identity" $
    do x <- newPort In  :: HProg (Signal Int8)
       y <- newPort Out :: HProg (Signal Int8)
       return (x, y)
  architecture "identity" "behavioural" $
    process (x .: []) $
      do y <=- x
-}
--------------------------------------------------------------------------------

testComponent :: HProg ()
testComponent = do
  (x, y) <- entity "component" $
    do x <- newPort In  :: HProg (Signal Bool)
       y <- newPort Out :: HProg (Signal Bool)
       return (x, y)
  architecture "component" "behavioural" $
    do c <- component invert
       return ()
       --portmap c (x .> y .> Nill)

invert :: HSig (Signal Bool -> Signal Bool -> ())
invert =
  input  $ \i ->
  output $ \o ->
  ret    $ do
    v <- getSignal i
    setSignal o (not v)

--------------------------------------------------------------------------------
