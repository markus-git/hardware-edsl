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

import Prelude hiding (and, or)


import Language.Embedded.VHDL (VHDL) -- !!! 

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
--  :+: ComponentCMD
  :+: StructuralCMD

type Prog = Program CMD (Param2 HExp HType)

--------------------------------------------------------------------------------
-- ** Example VHDL programs.

testIdentity :: Prog ()
testIdentity = do
{-
  x <- newPort In  :: Prog (Signal Int8)
  return ()
-}
  (x, y) <- entity "identity" $
    do x <- newPort In  :: Prog (Signal Int8)
       y <- newPort Out :: Prog (Signal Int8)
       return (x, y)
  architecture "identity" "behavioural" $
    process (x .: []) $
      do y <=- x

--------------------------------------------------------------------------------

compileProg
  :: (Interp instr VHDL (Param2 HExp HType), HFunctor instr)
  => Program instr (Param2 HExp HType) a -> String
compileProg = compile

{-
printTests :: IO ()
printTests = do
  putStrLn "\n### Simple ###\n"
  putStrLn $ compile $ testIdentity
-}
--------------------------------------------------------------------------------
