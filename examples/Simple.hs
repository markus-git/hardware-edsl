{-# LANGUAGE TypeOperators #-}

module Simple where

import Language.VHDL
import Language.Embedded.VHDL
import Language.Embedded.VHDL.Monad (prettyVHDL)
import qualified Language.Embedded.VHDL.Monad as M

import Control.Monad.Identity
import Control.Monad.Operational.Higher
import Data.ALaCarte
import Text.PrettyPrint

import Prelude hiding (and)

--------------------------------------------------------------------------------
-- * ...
--------------------------------------------------------------------------------

true, false :: Data Bool
true  = litE True
false = litE False

--------------------------------------------------------------------------------

simple :: Data Bool
simple = true `and` false

--------------------------------------------------------------------------------

type CMD = SequentialCMD Data :+: ConcurrentCMD Data :+: HeaderCMD Data

simpleWrap :: Type a => Data a -> Program CMD ()
simpleWrap var = architecture "test" $
  process "main" [] $
    (Ident "x") <== var

--------------------------------------------------------------------------------
-- ** ...

testSimple :: IO ()
testSimple = putStrLn $ compile $ simpleWrap simple

--------------------------------------------------------------------------------
