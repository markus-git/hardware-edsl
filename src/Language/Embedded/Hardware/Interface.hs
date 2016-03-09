{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}

module Language.Embedded.Hardware.Interface where

import Language.VHDL          (Expression)
import Language.Embedded.VHDL (VHDL, Type)
import Data.Constraint

--------------------------------------------------------------------------------
-- * Interface for evaluation and compilation of pure expressions into VHDL.
--------------------------------------------------------------------------------

-- | Variable identifier.
type VarId = String

-- | Expressions that support injection of values and named variables.
class FreeExp exp
  where
    -- | Constraint on the types of variables in a given expression language.
    type PredicateExp exp :: * -> Constraint

    -- | Literal expressions.
    litE :: PredicateExp exp a => a -> exp a

    -- | Variable expressions.
    varE :: PredicateExp exp a => VarId -> exp a

-- | General interface for evaluating expressions.
class FreeExp exp => EvaluateExp exp
  where
    -- | Evaluation of (closed) expressions.
    evalE :: exp a -> a

-- | General interface for compiling expressions.
class FreeExp exp => CompileExp exp
  where
    -- | Compilation of type kind.
    compT :: PredicateExp exp a => exp a -> VHDL Type

    -- | Compilation of expressions.
    compE :: PredicateExp exp a => exp a -> VHDL Expression

--------------------------------------------------------------------------------
