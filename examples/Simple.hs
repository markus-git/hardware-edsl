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
  :+: ComponentCMD    HExp
  :+: StructuralCMD   HExp

type Prog = Program CMD

--------------------------------------------------------------------------------
-- ** Example VHDL programs.

testSimple :: Prog ()
testSimple = do
  (o, i1, i2) <- structEntity "simple" $
         do x <- newPort "o"  Out :: Prog (Signal Int16)
            y <- newPort "i" In  :: Prog (Signal Int16)
            z <- newPort "i" In  :: Prog (Signal Int16)
            return (x, y, z)
  structArchitecture "simple" "behavioural" $
    structProcess [SignalX i1, SignalX i2] $
      do e1 <- getSignal i1 :: Prog (HExp Int16)
         e2 <- getSignal i2 :: Prog (HExp Int16)
         setSignal o (e1 + e2)

testArrays :: Prog ()
testArrays = do
  o <- structEntity "arrays" $
         newPort "o" Out :: Prog (Signal Int8)
  structArchitecture "arrays" "behavioural" $
    structProcess [] $
      do a <- newArray (litE 4)  :: Prog (Array Int8 Int8)
         v <- getArray (litE 0) a
         setSignal o v
         

testLoops :: Prog ()
testLoops = do
  (i, o) <- structEntity "loops" $
         do x <- newPort "i" In  :: Prog (Signal Word8)
            y <- newPort "o" Out :: Prog (Signal Word8)
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
         do x <- newPort "i" In  :: Prog (Signal Bool)
            y <- newPort "o" Out :: Prog (Signal Bool)
            return (x, y)
  structArchitecture "cond" "behavioural" $
    structProcess [SignalX i] $
      do v <- getSignal i
         iff (v)
           (setSignal o false)
           (setSignal o true)

testComponents :: Prog ()
testComponents = do
  let -- we define our signal function.
      function :: Signal Bool -> Prog (Signal Bool)
      function inp = do
        out <- newSignal
        y   <- getSignal inp
        setSignal out (y `xor` true)
        return out

      -- translate its signature into a 'pointer-passing' style.
      signature :: Sig HExp Prog (Signal Bool -> Signal Bool -> ())
      signature = Lam In $ \i -> Lam Out $ \o -> Unit $ do
        s <- function i
        v <- unsafeFreezeSignal s
        setSignal o v

  p      <- process signature
  (i, o) <- structEntity "portmap" $
         do x <- newPort "i" In  :: Prog (Signal Bool)
            y <- newPort "o" Out :: Prog (Signal Bool)
            return (x, y)
  structArchitecture "portmap" "structural" $
    portmap p (i :> o :> Nill)

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
  putStrLn "\n### Components ###\n"
  putStrLn $ compile $ testComponents

--------------------------------------------------------------------------------
