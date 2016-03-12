{-# LANGUAGE TypeOperators #-}

module Array where

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
-- *
--------------------------------------------------------------------------------

-- | Command set used for our 'simple' programs.
type CMD =
      SignalCMD       HExp
  :+: VariableCMD     HExp
  :+: ArrayCMD        HExp
  :+: VArrayCMD       HExp
  :+: LoopCMD         HExp
  :+: ConditionalCMD  HExp
  :+: ComponentCMD    HExp
  :+: StructuralCMD   HExp

type Prog = Program CMD

--------------------------------------------------------------------------------

array_simple :: Prog ()
array_simple = do
  (i, o) <- structEntity "simple" $
    do i <- newPort "input"  In    :: Prog (Signal Word2)
       o <- newPort "output" Out   :: Prog (Signal Word4)
       return (i, o)
  structArchitecture "simple" "behavioural" $
    structProcess [] $
      do a <- unpackArray "test" i
         return ()

--------------------------------------------------------------------------------

test :: IO ()
test = do
  putStrLn "\n### Simple ###\n"
  putStrLn $ compile $ array_simple

--------------------------------------------------------------------------------
