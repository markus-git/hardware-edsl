{-# LANGUAGE TypeOperators #-}

module Simple where

import Language.Embedded.VHDL
import Control.Monad.Identity
import Control.Monad.Operational.Higher
import Data.ALaCarte
import Data.Int
import Data.Word
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
         do x <- newPort     InOut true :: Prog (Signal Bool)
            y <- newPort_    In         :: Prog (Signal Word8)
            z <- newGeneric_ In         :: Prog (Signal Int16)
            return x
  newArchitecture "simple" "behavioural" $
    newProcess [toX i] $
      i <== (true `and` false)

testArrays :: Prog ()
testArrays = do
  i <- newEntity "arrays" $
         newPort Out true :: Prog (Signal Bool)
  newArchitecture "arrays" "behavioural" $
    newProcess [toX i] $
      do a <- newArray (litE 4) :: Prog (Array Int8 Bool)
         v <- getArray (litE 2) a
         i <== v

--------------------------------------------------------------------------------

printTests :: IO ()
printTests = do
  putStrLn "\n### Simple ###"
  putStrLn $ compile $ testSimple
  putStrLn "\n### Arrays ###"
  putStrLn $ compile $ testArrays

--------------------------------------------------------------------------------
