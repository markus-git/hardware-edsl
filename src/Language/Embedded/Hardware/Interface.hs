{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}

module Language.Embedded.Hardware.Interface where

import Language.VHDL          (Expression)
import Language.Embedded.VHDL (VHDL, Type)
import Data.Constraint

--------------------------------------------------------------------------------
-- * Interface for evaluation and compilation of pure expressions into VHDL.
--------------------------------------------------------------------------------

-- | Constraint on the types of variables in a given expression language.
type family PredicateExp (exp :: * -> *) :: * -> Constraint

-- | General interface for evaluating expressions.
class EvaluateExp exp
  where
    -- | Literal expressions.
    litE  :: PredicateExp exp a => a -> exp a

    -- | Evaluation of (closed) expressions.
    evalE :: PredicateExp exp a => exp a -> a

-- | General interface for compiling expressions.
class CompileExp exp
  where
    -- | Variable expressions.
    varE  :: PredicateExp exp a => Integer -> exp a

    -- | Compilation of type kind.
    compT :: PredicateExp exp a => exp a -> VHDL Type

    -- | Compilation of expressions.
    compE :: PredicateExp exp a => exp a -> VHDL Expression

--------------------------------------------------------------------------------
