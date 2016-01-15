{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Embedded.Hardware.Expression.Hoist where

import Language.VHDL (
    Expression       (..)
  , Relation         (..)
  , ShiftExpression  (..)
  , SimpleExpression (..)
  , Term             (..)
  , Factor           (..)
  , Primary          (..)
  , Identifier       (..)
  )

--------------------------------------------------------------------------------
-- * Lifting & hoisting of expressions.
--------------------------------------------------------------------------------

-- | Lift an expression into another.
class Lift a b where
  lift :: a -> b

-- base: we are the correct level.
instance {-# OVERLAPPING #-} Lift a a where
  lift = id

-- step: we can get to the correct level by stepping through the types.
instance {-# OVERLAPPABLE #-} (Hoist a, Lift (Next a) b) => Lift a b where
  lift = lift . hoist

--------------------------------------------------------------------------------

-- | Hoist an expression up one level.
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
  hoist si = ShiftExpression si Nothing

instance Hoist ShiftExpression where
  type Next ShiftExpression = Relation
  hoist sh = Relation sh Nothing

instance Hoist Relation where
  type Next Relation = Expression
  hoist r = ENand r Nothing

instance Hoist Expression where
  type Next Expression = Primary
  hoist e = PrimExp e

--------------------------------------------------------------------------------
-- ... I need to replace these ...
--------------------------------------------------------------------------------
  
-- | A collection of hardware expression types.
--
-- Going from "Expr a -> VHDL.Exp" means recovering the structure of VHDL's
-- expressions since VHDL.Exp isn't a single type. Hence the need for "Kind".
data Kind = E Expression | R Relation | Sh ShiftExpression | Si SimpleExpression
          | T Term       | F Factor   | P  Primary

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
