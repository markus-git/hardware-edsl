{-# LANGUAGE TypeOperators #-}

module Simple where

import Language.Embedded.VHDL
import Control.Monad.Identity
import Control.Monad.Operational.Higher
import Data.ALaCarte
import Text.PrettyPrint

import Prelude hiding (and)

--------------------------------------------------------------------------------
-- * ...
--------------------------------------------------------------------------------

-- | Command set used for our 'simple' programs.
type CMD =
      SignalCMD       VExp
  :+: VariableCMD     VExp
  :+: ArrayCMD        VExp
  :+: EntityCMD       VExp
  :+: ArchitectureCMD VExp
  :+: ProcessCMD      VExp
  :+: ConditionalCMD  VExp

type Prog = Program CMD

--------------------------------------------------------------------------------
-- ** Example VHDL programs.

testSimple :: Prog ()
testSimple = do
  i <- newEntity "simple" $
         do x <- newSignal true :: Prog (Signal Bool)
            y <- newSignal_     :: Prog (Signal Bool)
            return x
  a <- newArchitecture "simple" "behavioural" $
         do i <== (true `and` false)
  return ()

--------------------------------------------------------------------------------

printTests :: IO ()
printTests = do
  putStrLn $ compile $ testSimple

--------------------------------------------------------------------------------
