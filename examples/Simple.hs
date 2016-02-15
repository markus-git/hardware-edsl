{-# LANGUAGE TypeOperators #-}

module Simple where

import Language.VHDL (Mode(..))
import Language.Embedded.Hardware

import Control.Monad.Identity
import Control.Monad.Operational.Higher
import Data.ALaCarte
import Data.Int
import Data.Word
import Text.PrettyPrint

import Prelude hiding (and, or)

--------------------------------------------------------------------------------
-- * ...
--------------------------------------------------------------------------------

-- | Command set used for our 'simple' programs.
type CMD =
      SignalCMD       HExp
  :+: VariableCMD     HExp
  :+: ArrayCMD        HExp
  :+: LoopCMD         HExp
  :+: ConditionalCMD  HExp
  :+: StructuralCMD   HExp

type Prog = Program CMD

--------------------------------------------------------------------------------
-- ** Example VHDL programs.

testSimple :: Prog ()
testSimple = do
  i <- structEntity "simple" $
         do x <- newPort     InOut true :: Prog (Signal Bool)
            y <- newPort_    In         :: Prog (Signal Word8)
            z <- newGeneric_ In         :: Prog (Signal Int16)
            return x
  structArchitecture "simple" "behavioural" $
    structProcess [SignalX i] $
      i <== (true `and` false)

testArrays :: Prog ()
testArrays = do
  i <- structEntity "arrays" $
         newPort Out true :: Prog (Signal Bool)
  structArchitecture "arrays" "behavioural" $
    structProcess [SignalX i] $
      do a <- newArray (litE 4) :: Prog (Array Int8 Bool)
         v <- getArray (litE 2) a
         i <== v

testLoops :: Prog ()
testLoops = do
  (i, o) <- structEntity "loops" $
         do x <- newPort_ In  :: Prog (Signal Word8)
            y <- newPort_ Out :: Prog (Signal Word8)
            return (x, y)
  structArchitecture "loops" "behavioural" $
    structProcess [SignalX i] $ do
      for (litE 3) $ \n ->
        do y <- getSignal i
           o <== (n + y)
      while
        (do y <- getSignal i
            return $ y `gt` 3)
        (do setSignal o 2)

testConditionals :: Prog ()
testConditionals = do
  (i, o) <- structEntity "cond" $
         do x <- newPort_ In  :: Prog (Signal Bool)
            y <- newPort_ Out :: Prog (Signal Bool)
            return (x, y)
  structArchitecture "cond" "behavioural" $
    structProcess [SignalX i] $
      do v <- getSignal i
         iff (v)
           (setSignal o false)
           (setSignal o true)



--------------------------------------------------------------------------------

printTests :: IO ()
printTests = do
  putStrLn "\n### Simple ###\n"
  putStrLn $ compile $ testSimple
  putStrLn "\n### Arrays ###\n"
  putStrLn $ compile $ testArrays
  putStrLn "\n### Loops ###\n"
  putStrLn $ compile $ testLoops
  putStrLn "\n### Conditionals ###\n"
  putStrLn $ compile $ testConditionals

--------------------------------------------------------------------------------
