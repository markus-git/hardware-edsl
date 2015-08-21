{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}

module Language.Embedded.VHDL.Interface where

import Language.VHDL                (Expression, Identifier(..))
import Language.Embedded.VHDL.Monad (VHDL)

import Data.Constraint

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

type family PredicateExp (exp :: * -> *) :: * -> Constraint

-- | General interface for evaluating expressions
class EvaluateExp exp
  where
    -- | Literal expressions
    litE  :: PredicateExp exp a => a -> exp a

    -- | Evaluation of (closed) expressions
    evalE :: exp a -> a


-- | General interface for compiling expressions
class CompileExp exp
  where
    -- | Variable expressions
    varE  :: PredicateExp exp a => Identifier -> exp a

    -- | Compilation of expressions
    compE :: exp a -> VHDL Expression

newVar :: (CompileExp exp, PredicateExp exp a) => Integer -> exp a
newVar = varE . Ident . show

--------------------------------------------------------------------------------
