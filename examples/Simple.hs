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

tuple  :: (Data Bool, Data Bool)
tuple = (true, false)

--------------------------------------------------------------------------------

type CMD = SequentialCMD Data :+: ConcurrentCMD Data :+: HeaderCMD Data

simpleWrap :: Type a => Data a -> Program CMD ()
simpleWrap var = architecture "simple" $
  process "main" [] $
    (Ident "x") <== var

tupleWrap :: (Type a, Type b) => (Data a, Data b) -> Program CMD ()
tupleWrap var = architecture "tuple" $
  process "main" [] $
    undefined

--------------------------------------------------------------------------------
-- ** ...

simpleTest :: IO ()
simpleTest = putStrLn $ compile $ simpleWrap simple

tupleTest :: IO ()
tupleTest = putStrLn $ compile $ tupleWrap tuple

--------------------------------------------------------------------------------
