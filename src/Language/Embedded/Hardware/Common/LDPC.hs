{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ConstraintKinds     #-}

module Language.Embedded.Hardware.Common.LDPC where

import Language.Embedded.Hardware

import Control.Monad.Identity (Identity)
import Control.Monad.Operational.Higher hiding (when)

--------------------------------------------------------------------------------
-- * LDPC Components.
--------------------------------------------------------------------------------

type Prog instr exp pred = Program instr (Param2 exp pred)

--------------------------------------------------------------------------------

check :: Prog instr exp pred ()
check = undefined

--------------------------------------------------------------------------------
