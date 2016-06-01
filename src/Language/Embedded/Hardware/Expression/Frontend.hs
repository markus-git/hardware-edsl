{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}

module Language.Embedded.Hardware.Expression.Frontend where

import qualified Language.VHDL as V

import Language.Embedded.Hardware.Interface
import Language.Embedded.Hardware.Expression.Syntax hiding (Term, Factor, Primary)
import Language.Embedded.Hardware.Expression.Hoist
import Language.Embedded.Hardware.Expression.Represent
import Language.Embedded.Hardware.Expression.Represent.Bit
import qualified Language.Embedded.VHDL.Monad.Expression as V

import Data.Typeable
import qualified Data.Bits as B (Bits)

import Prelude hiding (not, and, or, abs, rem, div, mod, exp)
import qualified Prelude as P

import GHC.TypeLits

--------------------------------------------------------------------------------
-- * ...
--------------------------------------------------------------------------------

-- | ...
name :: HType a => V.Name -> HExp a
name n = sugarT (Name n)

-- | Creates a variable from a string.
var :: HType a => String -> HExp a
var = name . V.NSimple . V.Ident

-- | Lifts a typed value to an expression.
value :: HType a => a -> HExp a
value i = sugarT (Literal i)

-- | Casts an expression using supplied conversion function.
cast  :: (HType a, HType b) => (a -> b) -> HExp a -> HExp b
cast f = sugarT (Conversion f)

--------------------------------------------------------------------------------

type Hardware exp = (Expr exp, Rel exp, Shift exp, Simple exp, Term exp, Factor exp, Primary exp)

-- | Logical operators.
class Expr exp where
  true  :: exp Bool
  false :: exp Bool
  and   :: exp Bool -> exp Bool -> exp Bool
  or    :: exp Bool -> exp Bool -> exp Bool
  xor   :: exp Bool -> exp Bool -> exp Bool
  xnor  :: exp Bool -> exp Bool -> exp Bool
  nand  :: exp Bool -> exp Bool -> exp Bool
  nor   :: exp Bool -> exp Bool -> exp Bool

instance Expr HExp where
  true  = value True
  false = value False
  and   = sugarT And
  or    = sugarT Or
  xor   = sugarT Xor
  xnor  = sugarT Xnor
  nand  = sugarT Nand
  nor   = sugarT Nor

-- | Relational operators.
class Rel exp where
  eq  :: (HType a, Eq a) => exp a -> exp a -> exp Bool
  neq :: (HType a, Eq a) => exp a -> exp a -> exp Bool
  lt  :: (HType a, Ord a) => exp a -> exp a -> exp Bool
  lte :: (HType a, Ord a) => exp a -> exp a -> exp Bool
  gt  :: (HType a, Ord a) => exp a -> exp a -> exp Bool
  gte :: (HType a, Ord a) => exp a -> exp a -> exp Bool

instance Rel HExp where
  eq  = sugarT Eq
  neq = sugarT Neq
  lt  = sugarT Lt
  lte = sugarT Lte
  gt  = sugarT Gt
  gte = sugarT Gte

-- | Shift operators.
class Shift exp where
  sll :: (HType a, B.Bits a, HType b, Integral b) => exp a -> exp b -> exp a
  srl :: (HType a, B.Bits a, HType b, Integral b) => exp a -> exp b -> exp a
  sla :: (HType a, B.Bits a, HType b, Integral b) => exp a -> exp b -> exp a
  sra :: (HType a, B.Bits a, HType b, Integral b) => exp a -> exp b -> exp a
  rol :: (HType a, B.Bits a, HType b, Integral b) => exp a -> exp b -> exp a
  ror :: (HType a, B.Bits a, HType b, Integral b) => exp a -> exp b -> exp a

instance Shift HExp where
  sll = sugarT Sll
  srl = sugarT Srl
  sla = sugarT Sla
  sra = sugarT Sra
  rol = sugarT Rol
  ror = sugarT Ror

-- | Adding operators.
class Simple exp where
  neg :: (HType a, Num a) => exp a -> exp a
  add :: (HType a, Num a) => exp a -> exp a -> exp a
  sub :: (HType a, Num a) => exp a -> exp a -> exp a
  cat :: ( KnownNat n, KnownNat m, KnownNat (n + m), Typeable (n + m))
      => exp (Bits n) -> exp (Bits m) -> exp (Bits (n + m))

instance Simple HExp where
  neg = sugarT Neg
  add = sugarT Add
  sub = sugarT Sub
  cat = sugarT Cat

-- | Multiplying operators.
class Term exp where
  mul :: (HType a, Num a)      => exp a -> exp a -> exp a
  div :: (HType a, Integral a) => exp a -> exp a -> exp a
  mod :: (HType a, Integral a) => exp a -> exp a -> exp a
  rem :: (HType a, Integral a) => exp a -> exp a -> exp a

instance Term HExp where
  mul = sugarT Mul
  div = sugarT Div
  mod = sugarT Mod
  rem = sugarT Rem

-- | Miscellaneous operators.
class Factor exp where
  exp :: (HType a, Num a, HType b, Integral b) => exp a -> exp b -> exp a
  abs :: (HType a, Num a) => exp a -> exp a
  not :: exp Bool -> exp Bool

instance Factor HExp where
  exp = sugarT Exp
  abs = sugarT Abs
  not = sugarT Not

-- | ...
class Primary exp where
  risingEdge :: exp a -> exp Bool

instance Primary HExp where
  risingEdge = sugarT (Function "rising_edge" $ \_ -> error "vhdl-todo: cannot evaluate 'risingEdge'")

--------------------------------------------------------------------------------
-- These are a bit strange. Wonder when they'll add Typeable for type literals.

others :: (KnownNat n, Typeable n) => HExp Bit -> HExp (Bits n)
others = sugarT Others

slice :: (KnownNat n, Typeable n) => HExp (Bits n) -> (Integer, Integer) -> HExp (Bits m)
slice = undefined

--------------------------------------------------------------------------------

instance (HType a, Eq a) => Eq (HExp a)
  where
    (==) = error "VHDL: equality checking is not supported"

instance (HType a, Ord a) => Ord (HExp a)
  where
    compare = error "VHDL: compare is not supported"
    max     = error "VHDL: max is not supported"
    min     = error "VHDL: min is not supported"

instance (HType a, Bounded a) => Bounded (HExp a)
  where
    minBound = value minBound
    maxBound = value maxBound

instance (HType a, Enum a) => Enum (HExp a)
  where
    toEnum   = error "VHDL: toEnum is not supported"
    fromEnum = error "VHDL: fromEnum is not supported"

instance (HType a, Real a) => Real (HExp a)
  where
    toRational = error "VHDL: toRational is not supported"

instance (HType a, Num a) => Num (HExp a)
  where
    fromInteger = value . fromInteger
    (+)         = add
    (-)         = undefined --sub
    (*)         = mul
    abs         = abs
    signum      = error "VHDL: signum is not supported"

instance (HType a, Integral a) => Integral (HExp a)
  where
    quot         = error "VHDL: quotient is not supported"
    rem          = rem
    div          = div
    mod          = mod
    quotRem  a b = (quot a b, rem a b)
    divMod   a b = (div  a b, mod a b)
    toInteger    = error "VHDL: toInteger is not supported"

instance (HType a, Fractional a) => Fractional (HExp a)
  where
    (/)          = error "VHDL: floating point division is not _yet_ supported"
    recip        = (/) (value 1)
    fromRational = error "VHDL: fromRational is not supported"    

--------------------------------------------------------------------------------
