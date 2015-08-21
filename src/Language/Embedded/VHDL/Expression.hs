{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}

-- these are needed for Hoist/Lift
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Embedded.VHDL.Expression
  ( Expr
  ) where

import Language.VHDL (Expression(..)
                    , Relation(..)
                    , ShiftExpression(..)
                    , SimpleExpression(..)
                    , Term(..)
                    , Factor(..)
                    , Primary(..))
import qualified Language.VHDL as V

import Language.Embedded.VHDL.Interface
import Language.Embedded.VHDL.Monad
import qualified Language.Embedded.VHDL.Monad as M

import Data.Bits
import Data.Dynamic
import Data.Typeable
import Data.TypePredicates

import Prelude
import qualified Prelude as P

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

data Expr a
  where
    Val :: Show a     => a       -> Expr a
    Var :: Typeable a => Integer -> Expr a

    -- expression operators (plus Not)
    Not  :: Expr Bool -> Expr Bool
    And  :: Expr Bool -> Expr Bool -> Expr Bool
    Or   :: Expr Bool -> Expr Bool -> Expr Bool
    Xor  :: Expr Bool -> Expr Bool -> Expr Bool
    Xnor :: Expr Bool -> Expr Bool -> Expr Bool
    Nand :: Expr Bool -> Expr Bool -> Expr Bool
    Nor  :: Expr Bool -> Expr Bool -> Expr Bool

    -- relational operators
    Eq   :: Eq a  => Expr a -> Expr a -> Expr Bool
    Neq  :: Eq a  => Expr a -> Expr a -> Expr Bool
    Lt   :: Ord a => Expr a -> Expr a -> Expr Bool
    Lte  :: Ord a => Expr a -> Expr a -> Expr Bool
    Gt   :: Ord a => Expr a -> Expr a -> Expr Bool
    Gte  :: Ord a => Expr a -> Expr a -> Expr Bool

    -- shift operators
    Sll  :: Bits a => Expr a -> Expr a -> Expr a
    Srl  :: Bits a => Expr a -> Expr a -> Expr a
    Sla  :: Bits a => Expr a -> Expr a -> Expr a
    Sra  :: Bits a => Expr a -> Expr a -> Expr a
    Rol  :: Bits a => Expr a -> Expr a -> Expr a
    Ror  :: Bits a => Expr a -> Expr a -> Expr a

    -- adding operators
    Neg  :: Num a => Expr a -> Expr a
    Add  :: Num a => Expr a -> Expr a -> Expr a
    Sub  :: Num a => Expr a -> Expr a -> Expr a
    Cat  :: Num a => Expr a -> Expr a -> Expr a

    -- multiplyind operators
    Mul  :: Num a      => Expr a -> Expr a -> Expr a
    Div  :: Integral a => Expr a -> Expr a -> Expr a
    Mod  :: Integral a => Expr a -> Expr a -> Expr a
    Rem  :: Integral a => Expr a -> Expr a -> Expr a

    -- misc. operators (minus Not)
    Exp  :: Floating a => Expr a -> Expr a -> Expr a
    Abs  :: Num a      => Expr a -> Expr a

    -- and so on ...

type instance PredicateExp Expr = Typeable :/\: Show

--------------------------------------------------------------------------------
-- ** Evaluation

instance EvaluateExp Expr
  where
    litE  = Val
    evalE = evaluate (error "eval: free variable")

evaluate :: (Integer -> Dynamic) -> Expr a -> a
evaluate env exp = case exp of
    Var v | Just a <- fromDynamic (env v) -> a
    Val v    -> v
    
    Not  x   -> un P.not x
    And  x y -> bin (&&) x y
    Or   x y -> bin (||) x y
    Xor  x y -> bin xor  x y
    Xnor x y -> P.not $ bin xor  x y
    Nand x y -> P.not $ bin (&&) x y
    Nor  x y -> P.not $ bin (||) x y

    Eq   x y -> bin (==) x y
    Neq  x y -> bin (/=) x y
    Lt   x y -> bin (<)  x y
    Lte  x y -> bin (<=) x y
    Gt   x y -> bin (>)  x y
    Gte  x y -> bin (>=) x y

    Sll  x y -> undefined
    Srl  x y -> undefined  
    Sla  x y -> undefined  
    Sra  x y -> undefined  
    Rol  x y -> undefined  
    Ror  x y -> undefined  

    Neg  x   -> undefined   
    Add  x y -> undefined  
    Sub  x y -> undefined  
    Cat  x y -> undefined 

    Mul  x y -> undefined 
    Div  x y -> undefined 
    Mod  x y -> undefined 
    Rem  x y -> undefined 

    Exp  x y -> undefined
    Abs  x   -> undefined
  where
    xor a b = (a || b) && P.not (a && b)
    
    un :: (a -> b) -> Expr a -> b
    un  f x   = f (evaluate env x)

    bin :: (a -> b -> c) -> Expr a -> Expr b -> c
    bin f x y = f (evaluate env x) (evaluate env y)

--------------------------------------------------------------------------------
-- ** Compilation

-- | Lift one level
class Hoist a
  where
    type Next a :: *
    hoist :: a -> Next a

instance Hoist Primary
  where
    type Next Primary = Factor
    hoist p = FacPrim p Nothing

instance Hoist Factor
  where
    type Next Factor = Term
    hoist f = Term f []

instance Hoist Term
  where
    type Next Term = SimpleExpression
    hoist t = SimpleExpression Nothing t []

instance Hoist SimpleExpression
  where
    type Next SimpleExpression = ShiftExpression
    hoist s = ShiftExpression s Nothing

instance Hoist ShiftExpression
  where
    type Next ShiftExpression = Relation
    hoist s = Relation s Nothing

instance Hoist Relation
  where
    type Next Relation = Expression
    hoist r = ENand r Nothing

instance Hoist Expression
  where
    type Next Expression = Primary
    hoist e = PrimExp e

--------------------------------------------------------------------------------

-- | Lift any level
class Lift a b where
  lift :: a -> b

-- | Base case: we are the correct level
instance {-# OVERLAPPING #-} Lift a a where
  lift = id

-- | Step case: we can get to the correct level by using a kind of induction step
instance {-# OVERLAPPABLE #-} (Hoist a, Lift (Next a) b) => Lift a b where
  lift = lift . hoist

--------------------------------------------------------------------------------

instance CompileExp Expr
  where
    varE  = Var
    compE = compile

compile :: Expr a -> VHDL Expression
compile = return . go
  where
    go :: Expr t -> Expression
    go exp = case exp of
      Var v    -> lift $ name ('v' : show v)
      Val v    -> lift $ lit  (show v)
    
      Not  x   -> lift $ M.not  (lift $ go x)
      And  x y -> lift $ M.and  $ (lift $ go x) : (lift $ go y) : []
      Or   x y -> lift $ M.or   $ (lift $ go x) : (lift $ go y) : []
      Xor  x y -> lift $ M.xor  $ (lift $ go x) : (lift $ go y) : []
      Xnor x y -> lift $ M.and  $ (lift $ go x) : (lift $ go y) : []
      Nand x y -> lift $ M.nand (lift $ go x) (lift $ go y)
      Nor  x y -> lift $ M.nor  (lift $ go x) (lift $ go y)

      Eq   x y -> lift $ M.eq  (lift $ go x) (lift $ go y)
      Neq  x y -> lift $ M.neq (lift $ go x) (lift $ go y)
      Lt   x y -> lift $ M.lt  (lift $ go x) (lift $ go y)
      Lte  x y -> lift $ M.lte (lift $ go x) (lift $ go y)
      Gt   x y -> lift $ M.gt  (lift $ go x) (lift $ go y)
      Gte  x y -> lift $ M.gte (lift $ go x) (lift $ go y)
                  
      Sll  x y -> lift $ M.sll (lift $ go x) (lift $ go y)
      Srl  x y -> lift $ M.srl (lift $ go x) (lift $ go y)
      Sla  x y -> lift $ M.sla (lift $ go x) (lift $ go y)
      Sra  x y -> lift $ M.sra (lift $ go x) (lift $ go y)
      Rol  x y -> lift $ M.rol (lift $ go x) (lift $ go y)
      Ror  x y -> lift $ M.ror (lift $ go x) (lift $ go y)

      Neg  x   -> undefined
      Add  x y -> undefined
      Sub  x y -> undefined
      Cat  x y -> undefined

      Mul  x y -> undefined
      Div  x y -> undefined
      Mod  x y -> undefined
      Rem  x y -> undefined

      Exp  x y -> undefined
      Abs  x   -> undefined

--------------------------------------------------------------------------------
-- **



--------------------------------------------------------------------------------
