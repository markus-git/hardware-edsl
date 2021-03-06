{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ConstraintKinds       #-}

module Language.Embedded.Hardware.Expression.Syntax where

import Language.Syntactic
import Language.Syntactic.Functional (Denotation, Eval(..), EvalEnv)

import qualified Language.VHDL as V (Name, Aggregate)

--import Language.Embedded.Hardware.Command (CompArrayIx)
--instance CompArrayIx HExp

import Language.Embedded.Hardware.Interface
import Language.Embedded.Hardware.Expression.Represent
import Language.Embedded.Hardware.Expression.Represent.Bit

import Data.Typeable (Typeable)
import qualified Data.Bits as B

import GHC.TypeLits hiding (Symbol)

--------------------------------------------------------------------------------
-- * Syntax of hardware expressions.
--------------------------------------------------------------------------------

-- | Domain of expressions.
type Dom =
      Expression
  :+: Relational
  :+: ShiftExpression
  :+: SimpleExpression
  :+: Term
  :+: Factor
  :+: Primary

-- | Typed expressions.
data T sig
  where
    T :: PrimType (DenResult sig) => { unT :: Dom sig } -> T sig

-- | Specialized sugarSym for T.
sugarT
  :: ( Signature (SmartSig fi), sub :<: Dom
     , T :<: SmartSym fi
     , SyntacticN f fi
     , SmartFun (SmartSym fi) (SmartSig fi) ~ fi
     , PrimType (DenResult (SmartSig fi)))
  => sub (SmartSig fi)
  -> f
sugarT sym = sugarSym (T $ inj sym)

-- | Hardware primitive types.
type HType = PrimType

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
    Eq   :: (PrimType a)        => Relational (a :-> a :-> Full Bool)
    Neq  :: (PrimType a)        => Relational (a :-> a :-> Full Bool)
    Lt   :: (PrimType a, Ord a) => Relational (a :-> a :-> Full Bool)
    Lte  :: (PrimType a, Ord a) => Relational (a :-> a :-> Full Bool)
    Gt   :: (PrimType a, Ord a) => Relational (a :-> a :-> Full Bool)
    Gte  :: (PrimType a, Ord a) => Relational (a :-> a :-> Full Bool)

-- | Bit vector expressions.
data ShiftExpression sig
  where
    Sll :: (PrimType a, B.Bits a) => ShiftExpression (a :-> Integer :-> Full a)
    Srl :: (PrimType a, B.Bits a) => ShiftExpression (a :-> Integer :-> Full a)
    Sla :: (PrimType a, B.Bits a) => ShiftExpression (a :-> Integer :-> Full a)
    Sra :: (PrimType a, B.Bits a) => ShiftExpression (a :-> Integer :-> Full a)
    Rol :: (PrimType a, B.Bits a) => ShiftExpression (a :-> Integer :-> Full a)
    Ror :: (PrimType a, B.Bits a) => ShiftExpression (a :-> Integer :-> Full a)

-- | Numerical expressions.
data SimpleExpression sig
  where
    Neg :: (PrimType a, Num a) => SimpleExpression (a :->       Full a)
    Pos :: (PrimType a, Num a) => SimpleExpression (a :->       Full a)
    Add :: (PrimType a, Num a) => SimpleExpression (a :-> a :-> Full a)
    Sub :: (PrimType a, Num a) => SimpleExpression (a :-> a :-> Full a)
    Cat :: (KnownNat n, KnownNat m)
        => SimpleExpression (Bits n :-> Bits m :-> Full (Bits (n + m)))

-- | Integral expressions.
data Term sig
  where
    Mul :: (PrimType a, Num a)      => Term (a :-> a :-> Full a)
    Div :: (PrimType a, Integral a) => Term (a :-> a :-> Full a)
    Mod :: (PrimType a, Integral a) => Term (a :-> a :-> Full a)
    Rem :: (PrimType a, Integral a) => Term (a :-> a :-> Full a)

-- | ...
data Factor sig
  where
    Exp :: (PrimType a, Num a, PrimType b, Integral b)
        => Factor (a :-> b :-> Full a)
    Abs :: (PrimType a, Num a) => Factor (a :-> Full a)
    Not :: Factor (Bool :-> Full Bool)

-- | ...
data Primary sig
  where
    Name       :: (PrimType a) => V.Name -> Primary (Full a)
    Literal    :: (PrimType a) => a -> Primary (Full a)
    Aggregate  :: (PrimType a) => V.Aggregate -> Primary (Full a)
    Function   :: (Signature sig) => String -> Denotation sig -> Primary sig
    Qualified  :: (PrimType a, PrimType b) => b -> Primary (a :-> Full a)
    Conversion :: (PrimType a, PrimType b) => (a -> b) -> Primary (a :-> Full b)
    Allocator  :: (PrimType a) => Primary (Full a)
    -- *** todo: expanded aggregate
    Others     :: Primary (Bit :-> Full (Bits n))
    
--------------------------------------------------------------------------------
-- ** Syntactic instances.

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

instance Equality   ShiftExpression
instance StringTree ShiftExpression

instance Symbol ShiftExpression
  where
    symSig Sll = signature
    symSig Srl = signature
    symSig Sla = signature
    symSig Sra = signature
    symSig Rol = signature
    symSig Ror = signature

instance Render ShiftExpression
  where
    renderSym Sll = "sll"
    renderSym Srl = "srl"
    renderSym Sla = "sla"
    renderSym Sra = "sra"
    renderSym Rol = "rol"
    renderSym Ror = "ror"

instance Eval ShiftExpression
  where
    evalSym Sll = \x i -> B.shiftL x (fromIntegral i)
    evalSym Srl = \x i -> shiftLR     x (fromIntegral i)
      where
        shiftLR :: B.Bits a => a -> Int -> a
        shiftLR x n = let y = B.shiftR x n in
          case B.bitSizeMaybe x of
            Just i  -> foldr (flip B.clearBit) y [i - n `Prelude.mod` i .. i]
            Nothing -> y
    evalSym Sla = \x i -> B.shiftL x (fromIntegral i)
    evalSym Sra = \x i -> B.shiftR x (fromIntegral i)
    evalSym Rol = \x i -> B.rotateL x (fromIntegral i)
    evalSym Ror = \x i -> B.rotateR x (fromIntegral i)

instance EvalEnv ShiftExpression env

instance Equality   SimpleExpression
instance StringTree SimpleExpression

instance Symbol SimpleExpression
  where
    symSig Neg = signature
    symSig Pos = signature
    symSig Add = signature
    symSig Sub = signature
    symSig Cat = signature

instance Render SimpleExpression
  where
    renderSym Neg = "(-)"
    renderSym Pos = "id"
    renderSym Add = "(+)"
    renderSym Sub = "(-)"
    renderSym Cat = "(&)"

instance Eval SimpleExpression
  where
    evalSym Neg = negate
    evalSym Pos = id
    evalSym Add = (+)
    evalSym Sub = (-)
    evalSym Cat = bitJoin

instance EvalEnv SimpleExpression env

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
    symSig (Others)       = signature
    symSig (Function _ _) = signature
    symSig (Qualified _)  = signature
    symSig (Conversion _) = signature
    symSig (Allocator)    = signature

instance Render Primary
  where
    renderSym (Name _)       = "name"
    renderSym (Literal _)    = "lit"
    renderSym (Aggregate _)  = "agg"
    renderSym (Others)       = "others"
    renderSym (Function _ _) = "fun"
    renderSym (Qualified _)  = "qual"
    renderSym (Conversion _) = "conv"
    renderSym (Allocator)    = "alloc"

instance Eval Primary
  where
    evalSym (Name _)       = error "cannot eval open names!"
    evalSym (Literal i)    = i
    evalSym (Aggregate _)  = error "primary-todo: eval aggregate names."
    evalSym (Others)       = error "primary-todo: eval others"
    evalSym (Function _ f) = f
    evalSym (Qualified _)  = error "primary-todo: eval qualified names."
    evalSym (Conversion f) = f
    evalSym (Allocator)    = error "primary-todo: eval allocator"

instance EvalEnv Primary env

--------------------------------------------------------------------------------
-- *** Temporary fix until GHC fixes their class resolution for DTC ***
--------------------------------------------------------------------------------

instance {-# OVERLAPPING #-} Project sub Dom => Project sub (AST T)
  where
    prj (Sym s) = prj s

instance {-# OVERLAPPING #-} Project sub Dom => Project sub T
  where
    prj (T a) = prj a

instance {-# OVERLAPPING #-} Project Expression Dom
  where
    prj (InjL a) = Just a
    prj _ = Nothing

instance {-# OVERLAPPING #-} Project Relational Dom
  where
    prj (InjR (InjL a)) = Just a
    prj _ = Nothing

instance {-# OVERLAPPING #-} Project ShiftExpression Dom
  where
    prj (InjR (InjR (InjL a))) = Just a
    prj _ = Nothing

instance {-# OVERLAPPING #-} Project SimpleExpression Dom
  where
    prj (InjR (InjR (InjR (InjL a)))) = Just a
    prj _ = Nothing

instance {-# OVERLAPPING #-} Project Term Dom
  where
    prj (InjR (InjR (InjR (InjR (InjL a))))) = Just a
    prj _ = Nothing

instance {-# OVERLAPPING #-} Project Factor Dom
  where
    prj (InjR (InjR (InjR (InjR (InjR (InjL a)))))) = Just a
    prj _ = Nothing

instance {-# OVERLAPPING #-} Project Primary Dom
  where
    prj ((InjR (InjR (InjR (InjR (InjR (InjR a))))))) = Just a
    prj _ = Nothing

--------------------------------------------------------------------------------

