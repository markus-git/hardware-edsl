{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Embedded.Hardware.Expression.Syntax where

import Language.Syntactic
import Language.Syntactic.Functional (Denotation, Eval(..), EvalEnv)

import qualified Language.VHDL as V (Name, Aggregate)

import Language.Embedded.Hardware.Command
import Language.Embedded.Hardware.Interface
import Language.Embedded.Hardware.Expression.Represent

import Data.Bits     (Bits)
import Data.Typeable (Typeable)
import qualified Data.Bits as Bits

--------------------------------------------------------------------------------
-- * Syntax of hardware expressions.
--------------------------------------------------------------------------------

-- | Collection of required classes for hardware expressions.
class    (Typeable a, Rep a, Eq a) => HType a
instance (Typeable a, Rep a, Eq a) => HType a

-- | Domain of expressions.
type Dom =
      Expression
  :+: Relational
  :+: Shift
  :+: Simple
  :+: Term
  :+: Factor
  :+: Primary

-- | Typed expressions.
data T sig
  where
    T :: HType (DenResult sig) => { unT :: Dom sig } -> T sig

-- | Specialized sugarSym for T.
sugarT
  :: ( Signature (SmartSig fi), sub :<: Dom
     , T :<: SmartSym fi
     , SyntacticN f fi
     , SmartFun (SmartSym fi) (SmartSig fi) ~ fi
     , HType (DenResult (SmartSig fi)))
  => sub (SmartSig fi)
  -> f
sugarT sym = sugarSym (T $ inj sym)

-- | Hardware expressions.
newtype HExp a = HExp { unHExp :: ASTF T a }

instance Syntactic (HExp a)
  where
    type Domain   (HExp a) = T
    type Internal (HExp a) = a

    desugar = unHExp
    sugar   = HExp 

--------------------------------------------------------------------------------
-- ** Syntax.

-- | Logical expressions.
data Expression sig
  where
    And  :: Expression (Bool :-> Bool :-> Full Bool)
    Or   :: Expression (Bool :-> Bool :-> Full Bool)
    Xor  :: Expression (Bool :-> Bool :-> Full Bool)
    Xnor :: Expression (Bool :-> Bool :-> Full Bool)
    Nand :: Expression (Bool :-> Bool :-> Full Bool)
    Nor  :: Expression (Bool :-> Bool :-> Full Bool)

-- | Relational expressions.
data Relational sig
  where
    Eq   :: (HType a)        => Relational (a :-> a :-> Full Bool)
    Neq  :: (HType a)        => Relational (a :-> a :-> Full Bool)
    Lt   :: (HType a, Ord a) => Relational (a :-> a :-> Full Bool)
    Lte  :: (HType a, Ord a) => Relational (a :-> a :-> Full Bool)
    Gt   :: (HType a, Ord a) => Relational (a :-> a :-> Full Bool)
    Gte  :: (HType a, Ord a) => Relational (a :-> a :-> Full Bool)

-- | Bit vector expressions.
data Shift sig
  where
    Sll :: (HType a, Bits a, HType b, Integral b) => Shift (a :-> b :-> Full a)
    Srl :: (HType a, Bits a, HType b, Integral b) => Shift (a :-> b :-> Full a)
    Sla :: (HType a, Bits a, HType b, Integral b) => Shift (a :-> b :-> Full a)
    Sra :: (HType a, Bits a, HType b, Integral b) => Shift (a :-> b :-> Full a)
    Rol :: (HType a, Bits a, HType b, Integral b) => Shift (a :-> b :-> Full a)
    Ror :: (HType a, Bits a, HType b, Integral b) => Shift (a :-> b :-> Full a)

-- | Numerical expressions.
data Simple sig
  where
    Neg :: (HType a, Num a) => Simple (a :->       Full a)
    Pos :: (HType a, Num a) => Simple (a :->       Full a)
    Add :: (HType a, Num a) => Simple (a :-> a :-> Full a)
    Sub :: (HType a, Num a) => Simple (a :-> a :-> Full a)
    Cat :: (HType a, HType b, Show a, Read b) => Simple (a :-> a :-> Full b)

-- | Integral expressions.
data Term sig
  where
    Mul :: (HType a, Num a)      => Term (a :-> a :-> Full a)
    Div :: (HType a, Integral a) => Term (a :-> a :-> Full a)
    Mod :: (HType a, Integral a) => Term (a :-> a :-> Full a)
    Rem :: (HType a, Integral a) => Term (a :-> a :-> Full a)

-- | ...
data Factor sig
  where
    Exp :: (HType a, Num a, HType b, Integral b) => Factor (a :-> b :-> Full a)
    Abs :: (HType a, Num a) => Factor (a :-> Full a)
    Not :: Factor (Bool :-> Full Bool)

-- | ...
data Primary sig
  where
    Name       :: (HType a) => V.Name      -> Primary (Full a)
    Literal    :: (HType a) => a           -> Primary (Full a)
    Aggregate  :: (HType a) => V.Aggregate -> Primary (Full a)
    Function   :: (Signature sig) => String -> Denotation sig -> Primary sig
    Qualified  :: (HType a, HType b) => b        -> Primary (a :-> Full a)
    Conversion :: (HType a, HType b) => (a -> b) -> Primary (a :-> Full b)
    Allocator  :: (HType a) => Primary (Full a)
    
--------------------------------------------------------------------------------
-- ** Syntactic instances.

instance CompArrayIx HExp

instance Equality T
  where
    equal (T s) (T t) = equal s t
    hash  (T s)       = hash s

instance StringTree T
  where
    stringTreeSym as (T s) = stringTreeSym as (T s)

instance Symbol T
  where
    symSig (T s) = symSig s

instance Render T
  where
    renderSym     (T s) = renderSym s
    renderArgs as (T s) = renderArgs as s

instance Equality   Expression
instance StringTree Expression

instance Symbol Expression
  where
    symSig And  = signature
    symSig Or   = signature
    symSig Xor  = signature
    symSig Xnor = signature
    symSig Nand = signature
    symSig Nor  = signature

instance Render Expression
  where
    renderSym And  = "and"
    renderSym Or   = "or"
    renderSym Xor  = "xor"
    renderSym Xnor = "xnor"
    renderSym Nand = "nand"
    renderSym Nor  = "nor"

instance Eval Expression
  where
    evalSym And  = (&&)
    evalSym Or   = (||)
    evalSym Xor  = \x y -> (x && Prelude.not y) || (Prelude.not x && y)
    evalSym Xnor = \x y -> (Prelude.not x && Prelude.not y) || (x && y)
    evalSym Nand = \x y -> Prelude.not (x && y)
    evalSym Nor  = \x y -> Prelude.not (x || y)

instance EvalEnv Expression env

instance Equality   Relational
instance StringTree Relational

instance Symbol Relational
  where
    symSig Eq  = signature
    symSig Neq = signature
    symSig Lt  = signature
    symSig Lte = signature
    symSig Gt  = signature
    symSig Gte = signature

instance Render Relational
  where
    renderSym Eq  = "(==)"
    renderSym Neq = "(/=)"
    renderSym Lt  = "(<)"
    renderSym Lte = "(<=)"
    renderSym Gt  = "(>)"
    renderSym Gte = "(>=)"

instance Eval Relational
  where
    evalSym Eq  = (==)
    evalSym Neq = (/=)
    evalSym Lt  = (<)
    evalSym Lte = (<=)
    evalSym Gt  = (>)
    evalSym Gte = (>=)

instance EvalEnv Relational env

instance Equality   Shift
instance StringTree Shift

instance Symbol Shift
  where
    symSig Sll = signature
    symSig Srl = signature
    symSig Sla = signature
    symSig Sra = signature
    symSig Rol = signature
    symSig Ror = signature

instance Render Shift
  where
    renderSym Sll = "sll"
    renderSym Srl = "srl"
    renderSym Sla = "sla"
    renderSym Sra = "sra"
    renderSym Rol = "rol"
    renderSym Ror = "ror"

instance Eval Shift
  where
    evalSym Sll = \x i -> Bits.shiftL x (fromIntegral i)
    evalSym Srl = \x i -> shiftLR     x (fromIntegral i)
      where
        shiftLR :: Bits a => a -> Int -> a
        shiftLR x n = let y = Bits.shiftR x n in
          case Bits.bitSizeMaybe x of
            Just i  -> foldr (flip Bits.clearBit) y [i - n `Prelude.mod` i .. i]
            Nothing -> y
    evalSym Sla = \x i -> Bits.shiftL x (fromIntegral i)
    evalSym Sra = \x i -> Bits.shiftR x (fromIntegral i)
    evalSym Rol = \x i -> Bits.rotateL x (fromIntegral i)
    evalSym Ror = \x i -> Bits.rotateR x (fromIntegral i)

instance EvalEnv Shift env

instance Equality   Simple
instance StringTree Simple

instance Symbol Simple
  where
    symSig Neg = signature
    symSig Pos = signature
    symSig Add = signature
    symSig Sub = signature
    symSig Cat = signature

instance Render Simple
  where
    renderSym Neg = "(-)"
    renderSym Pos = "id"
    renderSym Add = "(+)"
    renderSym Sub = "(-)"
    renderSym Cat = "(&)"

instance Eval Simple
  where
    evalSym Neg = negate
    evalSym Pos = id
    evalSym Add = (+)
    evalSym Sub = (-)
    evalSym Cat = \x y -> read (show x ++ show y)

instance EvalEnv Simple env

instance Equality   Term
instance StringTree Term

instance Symbol Term
  where
    symSig Mul = signature
    symSig Div = signature
    symSig Mod = signature
    symSig Rem = signature

instance Render Term
  where
    renderSym Mul = "(*)"
    renderSym Div = "(/)"
    renderSym Mod = "(%)"
    renderSym Rem = "rem"

instance Eval Term
  where
    evalSym Mul = (*)
    evalSym Div = Prelude.div
    evalSym Mod = Prelude.mod
    evalSym Rem = Prelude.rem

instance EvalEnv Term env

instance Equality   Factor
instance StringTree Factor

instance Symbol Factor
  where
    symSig Exp = signature
    symSig Abs = signature
    symSig Not = signature

instance Render Factor
  where
    renderSym Exp = "(**)"
    renderSym Abs = "abs"
    renderSym Not = "not"

instance Eval Factor
  where
    evalSym Exp = (^)
    evalSym Abs = Prelude.abs
    evalSym Not = Prelude.not

instance EvalEnv Factor env

instance Equality   Primary
instance StringTree Primary

instance Symbol Primary
  where
    symSig (Name _)       = signature
    symSig (Literal _)    = signature
    symSig (Aggregate _)  = signature
    symSig (Function _ _) = signature
    symSig (Qualified _)  = signature
    symSig (Conversion _) = signature
    symSig (Allocator)    = signature

instance Render Primary
  where
    renderSym (Name _)       = "name"
    renderSym (Literal _)    = "lit"
    renderSym (Aggregate _)  = "agg"
    renderSym (Function _ _) = "fun"
    renderSym (Qualified _)  = "qual"
    renderSym (Conversion _) = "conv"
    renderSym (Allocator)    = "alloc"

instance Eval Primary
  where
    evalSym (Name _)       = error "cannot eval open names!"
    evalSym (Literal i)    = i
    evalSym (Aggregate _)  = error "todo: eval aggregate names."
    evalSym (Function _ f) = f
    evalSym (Qualified _)  = error "todo: eval qualified names."
    evalSym (Conversion f) = f
    evalSym (Allocator)    = error "todo: eval allocator"

instance EvalEnv Primary env

--------------------------------------------------------------------------------
