{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Embedded.VHDL.Expression.Hoist where

import Language.VHDL (
    Expression(..)
  , Relation(..)
  , ShiftExpression(..)
  , SimpleExpression(..)
  , Term(..)
  , Factor(..)
  , Primary(..)
  , Identifier(..)
  )

import Language.Embedded.VHDL.Interface

--------------------------------------------------------------------------------
-- * Lifting / Hoisting of VHDL Expression
--------------------------------------------------------------------------------

-- | A collection type for VHDLs expression types.
--
-- Going from "Expr a -> VHDL.Exp" means recovering the structure of vhdls
-- expressions, since Exp isn't a single type. Hence the need for "Kind".  
data Kind =
    E  Expression
  | R  Relation
  | Sh ShiftExpression
  | Si SimpleExpression
  | T  Term
  | F  Factor
  | P  Primary

--------------------------------------------------------------------------------
-- ** Lift one level

class Hoist a where
  type Next a :: *
  hoist :: a -> Next a

instance Hoist Primary where
  type Next Primary = Factor
  hoist p = FacPrim p Nothing

instance Hoist Factor where
  type Next Factor = Term
  hoist f = Term f []

instance Hoist Term where
  type Next Term = SimpleExpression
  hoist t = SimpleExpression Nothing t []

instance Hoist SimpleExpression where
  type Next SimpleExpression = ShiftExpression
  hoist s = ShiftExpression s Nothing

instance Hoist ShiftExpression where
  type Next ShiftExpression = Relation
  hoist s = Relation s Nothing

instance Hoist Relation where
  type Next Relation = Expression
  hoist r = ENand r Nothing

instance Hoist Expression where
  type Next Expression = Primary
  hoist e = PrimExp e

--------------------------------------------------------------------------------
-- ** Lift any number of levels

class Lift a b where
  lift :: a -> b

-- | Base case: we are the correct level
instance {-# OVERLAPPING #-} Lift a a where
  lift = id

-- | Step case: we can get to the correct level by using a kind of induction step
instance {-# OVERLAPPABLE #-} (Hoist a, Lift (Next a) b) => Lift a b where
  lift = lift . hoist

--------------------------------------------------------------------------------
-- ** Lifting of Kind

instance Lift Kind Expression where
  lift (E e)   = lift e
  lift (R r)   = lift r
  lift (Sh sh) = lift sh
  lift (Si si) = lift si
  lift (T t)   = lift t
  lift (F f)   = lift f
  lift (P p)   = lift p

instance Lift Kind Relation where
  lift (E e)   = lift e
  lift (R r)   = lift r
  lift (Sh sh) = lift sh
  lift (Si si) = lift si
  lift (T t)   = lift t
  lift (F f)   = lift f
  lift (P p)   = lift p

instance Lift Kind ShiftExpression where
  lift (E e)   = lift e
  lift (R r)   = lift r
  lift (Sh sh) = lift sh
  lift (Si si) = lift si
  lift (T t)   = lift t
  lift (F f)   = lift f
  lift (P p)   = lift p

instance Lift Kind SimpleExpression where
  lift (E e)   = lift e
  lift (R r)   = lift r
  lift (Sh sh) = lift sh
  lift (Si si) = lift si
  lift (T t)   = lift t
  lift (F f)   = lift f
  lift (P p)   = lift p

instance Lift Kind Term where
  lift (E e)   = lift e
  lift (R r)   = lift r
  lift (Sh sh) = lift sh
  lift (Si si) = lift si
  lift (T t)   = lift t
  lift (F f)   = lift f
  lift (P p)   = lift p

instance Lift Kind Factor where
  lift (E e)   = lift e
  lift (R r)   = lift r
  lift (Sh sh) = lift sh
  lift (Si si) = lift si
  lift (T t)   = lift t
  lift (F f)   = lift f
  lift (P p)   = lift p

instance Lift Kind Primary where
  lift (E e)   = lift e
  lift (R r)   = lift r
  lift (Sh sh) = lift sh
  lift (Si si) = lift si
  lift (T t)   = lift t
  lift (F f)   = lift f
  lift (P p)   = lift p

--------------------------------------------------------------------------------
