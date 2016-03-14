{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}

module Language.Embedded.Hardware.AXI.Controller where

import qualified Language.VHDL as V

import Language.Embedded.Hardware.Expression
import Language.Embedded.Hardware.Expression.Represent
import Language.Embedded.Hardware.Expression.Hoist
import Language.Embedded.Hardware.Command
import Language.Embedded.Hardware.Interface

import Control.Monad.Operational.Higher

import Data.Bits
import Data.Ix (Ix)

import Language.Embedded.VHDL

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

pack
  :: forall instr m a.
     ( FreeExp      (IExp instr)
     , PredicateExp (IExp instr) a
     , Rep a)
  => a
  -> ProgramT instr m (IArray (IExp instr a) Bool)
pack v =
  do let ix = litE v :: IExp instr a

     undefined

{-
pack :: forall a. Rep a => a -> VHDL (IArray a Bool)
pack v = do
  t <- declare (undefined :: proxy a)
  let ix :: V.Expression
      ix = lift $ lit $ sized t
  undefined
-}
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
