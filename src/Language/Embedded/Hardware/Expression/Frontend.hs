module Language.Embedded.Hardware.Expression.Frontend where

import qualified Language.VHDL as V

import Language.Embedded.Hardware.Interface (VarId(..))
import Language.Embedded.Hardware.Expression.Syntax

import Data.Bits (Bits)

import Prelude hiding (not, and, or, abs, rem, div, mod, exp)
import qualified Prelude as P

--------------------------------------------------------------------------------
-- * ...
--------------------------------------------------------------------------------

-- | ...
name :: HType a => V.Name -> HExp a
name n = sugarT (Name n)

-- | Lifts a typed value to an expression.
value :: HType a => a -> HExp a
value i = sugarT (Literal i)

-- | Creates a variable from a string.
variable :: HType a => VarId -> HExp a
variable = name . V.NSimple . V.Ident . id
  where
    id :: VarId -> String
    id (Unique i) = i
    id (Base   i) = i

-- | Casts an expression using supplied conversion function.
cast  :: (HType a, HType b) => (a -> b) -> HExp a -> HExp b
cast f = sugarT (Conversion f)

--------------------------------------------------------------------------------

true, false :: HExp Bool
true  = value True
false = value False

-- logical operators
and, or, xor, xnor, nand, nor :: HExp Bool -> HExp Bool -> HExp Bool
and  = sugarT And
or   = sugarT Or
xor  = sugarT Xor
xnor = sugarT Xnor
nand = sugarT Nand
nor  = sugarT Nor

-- relational operators
eq, neq :: (HType a, Eq a) => HExp a -> HExp a -> HExp Bool
eq  = sugarT Eq
neq = sugarT Neq

lt, lte, gt, gte :: (HType a, Ord a) => HExp a -> HExp a -> HExp Bool
lt  = sugarT Lt
lte = sugarT Lte
gt  = sugarT Gt
gte = sugarT Gte

-- shift operators
sll, srl, sla, sra, rol, ror :: (HType a, Bits a, HType b, Integral b) => HExp a -> HExp b -> HExp a
sll = sugarT Sll
srl = sugarT Srl
sla = sugarT Sla
sra = sugarT Sra
rol = sugarT Rol
ror = sugarT Ror

-- adding operators
add, sub :: (HType a, Num a) => HExp a -> HExp a -> HExp a
add = sugarT Add
sub = sugarT Sub

cat :: (HType a, Read a, Show a) => HExp a -> HExp a -> HExp a
cat = sugarT Cat

-- multiplying operators
mul :: (HType a, Num a) => HExp a -> HExp a -> HExp a
mul = sugarT Mul

div, mod, rem :: (HType a, Integral a) => HExp a -> HExp a -> HExp a
div = sugarT Div
mod = sugarT Mod
rem = sugarT Rem

-- miscellaneous operators
exp :: (HType a, Num a, HType b, Integral b) => HExp a -> HExp b -> HExp a
exp = sugarT Exp

abs :: (HType a, Num a) => HExp a -> HExp a
abs = sugarT Abs

not :: HExp Bool -> HExp Bool
not = sugarT Not

--------------------------------------------------------------------------------

-- *** temp
event :: HType a => HExp a -> HExp Bool
event = sugarT (Attribute "EVENT")

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
    (-)         = sub
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
