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
  i <- entity "simple" $
         do x <- newPort     InOut true :: Prog (Signal Bool)
            y <- newPort_    In         :: Prog (Signal Word8)
            z <- newGeneric_ In         :: Prog (Signal Int16)
            return x
  architecture "simple" "behavioural" $
    process [hideSig i] $
      i <== (true `and` false)

testArrays :: Prog ()
testArrays = do
  i <- entity "arrays" $
         newPort Out true :: Prog (Signal Bool)
  architecture "arrays" "behavioural" $
    process [hideSig i] $
      do a <- newArray (litE 4) :: Prog (Array Int8 Bool)
         v <- getArray (litE 2) a
         i <== v

testLoops :: Prog ()
testLoops = do
  (i, o) <- entity "loops" $
         do x <- newPort_ In  :: Prog (Signal Word8)
            y <- newPort_ Out :: Prog (Signal Word8)
            return (x, y)
  architecture "loops" "behavioural" $
    process [hideSig i] $
      for (litE 3) $ \n ->
        do y <- getSignal i
           o <== (n + y) 

testConditionals :: Prog ()
testConditionals = do
  (i, o) <- entity "cond" $
         do x <- newPort_ In  :: Prog (Signal Bool)
            y <- newPort_ Out :: Prog (Signal Bool)
            return (x, y)
  architecture "cond" "behavioural" $
    process [hideSig i] $
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
