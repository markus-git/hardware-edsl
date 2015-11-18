{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Language.Embedded.VHDL.Expression where
{-
  ( Expr
  , pair, first, second
  , not, and, or, xor, xnor, nand, nor
  , eq, neq
  , lt, lte, gt, gte
  , sll, srl, sla, sra, rol, ror
  , add, sub, cat
  , mul
  , div, mod, rem
  , neg
  , exp, abs
  , name, lit
  ) 
-}

import Language.VHDL (Identifier(..))

import Language.Embedded.VHDL.Interface
import Language.Embedded.VHDL.Monad           (VHDL, TypeRep(..))
import qualified Language.Embedded.VHDL.Monad as M
import Language.Embedded.VHDL.Expression.Hoist
import Language.Embedded.VHDL.Expression.Format
import qualified Language.Embedded.VHDL.Expression.Type as T

import Language.Syntactic hiding (fold, printExpr, showAST, drawAST, writeHtmlAST)
import qualified Language.Syntactic as Syntactic
import Language.Syntactic.Functional hiding (Name)
import Language.Syntactic.Functional.Sharing
import Language.Syntactic.Functional.Tuple
import Language.Syntactic.Sugar.BindingT ()
import Language.Syntactic.Sugar.TupleT ()

import Data.Bits     (Bits)
import qualified Data.Bits as Bits
import Data.Maybe    (fromJust)
import Data.Typeable (Typeable)

import Prelude hiding (not, and, or, abs, rem, div, mod)
import qualified Prelude

--------------------------------------------------------------------------------
-- * ...
--------------------------------------------------------------------------------

class    (Typeable a, Rep a, Eq a) => Type a
instance (Typeable a, Rep a, Eq a) => Type a

--------------------------------------------------------------------------------
-- ** ...

data Expression sig
  where
    And  :: Expression (Bool :-> Bool :-> Full Bool)
    Or   :: Expression (Bool :-> Bool :-> Full Bool)
    Xor  :: Expression (Bool :-> Bool :-> Full Bool)
    Xnor :: Expression (Bool :-> Bool :-> Full Bool)
    Nand :: Expression (Bool :-> Bool :-> Full Bool)
    Nor  :: Expression (Bool :-> Bool :-> Full Bool)

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

--------------------------------------------------------------------------------
-- ** ...

data Relational sig
  where
    Eq   :: Type a => Relational (a :-> a :-> Full Bool)
    Neq  :: Type a => Relational (a :-> a :-> Full Bool)
    Lt   :: (Type a, Ord a) => Relational (a :-> a :-> Full Bool)
    Lte  :: (Type a, Ord a) => Relational (a :-> a :-> Full Bool)
    Gt   :: (Type a, Ord a) => Relational (a :-> a :-> Full Bool)
    Gte  :: (Type a, Ord a) => Relational (a :-> a :-> Full Bool)

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

--------------------------------------------------------------------------------
-- ** ...

data Shift sig
  where
    Sll :: (Type a, Bits a, Type b, Integral b) => Shift (a :-> b :-> Full a)
    Srl :: (Type a, Bits a, Type b, Integral b) => Shift (a :-> b :-> Full a)
    Sla :: (Type a, Bits a, Type b, Integral b) => Shift (a :-> b :-> Full a)
    Sra :: (Type a, Bits a, Type b, Integral b) => Shift (a :-> b :-> Full a)
    Rol :: (Type a, Bits a, Type b, Integral b) => Shift (a :-> b :-> Full a)
    Ror :: (Type a, Bits a, Type b, Integral b) => Shift (a :-> b :-> Full a)

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
    evalSym Sla = \x i -> Bits.shiftL x (fromIntegral i)
    evalSym Sra = \x i -> Bits.shiftR x (fromIntegral i)
    evalSym Rol = \x i -> Bits.rotateL x (fromIntegral i)
    evalSym Ror = \x i -> Bits.rotateR x (fromIntegral i)

shiftLR :: Bits a => a -> Int -> a
shiftLR x n =
  let y = Bits.shiftR x n
  in case Bits.bitSizeMaybe x of
    Just i  -> foldr (flip Bits.clearBit) y [i - n `Prelude.mod` i .. i]
    Nothing -> y

instance EvalEnv Shift env

--------------------------------------------------------------------------------
-- ** ...

data Simple sig
  where
    Neg :: (Type a, Num a) => Simple (a :->       Full a)
    Pos :: (Type a, Num a) => Simple (a :->       Full a)
    Add :: (Type a, Num a) => Simple (a :-> a :-> Full a)
    Sub :: (Type a, Num a) => Simple (a :-> a :-> Full a)
    Cat :: (Type a, Show a, Read a) => Simple (a :-> a :-> Full a)

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

--------------------------------------------------------------------------------
-- ** ...

data Term sig
  where
    Mul :: (Type a, Num a)      => Term (a :-> a :-> Full a)
    Div :: (Type a, Integral a) => Term (a :-> a :-> Full a)
    Mod :: (Type a, Integral a) => Term (a :-> a :-> Full a)
    Rem :: (Type a, Integral a) => Term (a :-> a :-> Full a)

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

--------------------------------------------------------------------------------
-- ** ...

data Factor sig
  where
    Exp :: (Type a, Num a, Type b, Integral b) => Factor (a :-> b :-> Full a)
    Abs :: (Type a, Num a) => Factor (a :-> Full a)
    Not :: Factor (Bool :-> Full Bool)

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

--------------------------------------------------------------------------------
-- ** ...

data Primary sig
  where
    Lit :: Type a => a -> Primary (Full a)

instance Equality   Primary
instance StringTree Primary

instance Symbol Primary
  where
    symSig (Lit _) = signature

instance Render Primary
  where
    renderSym (Lit _) = "lit"

instance Eval Primary
  where
    evalSym (Lit i) = i

instance EvalEnv Primary env

--------------------------------------------------------------------------------

type VHDLDomain = Typed
  (   BindingT
  :+: Let
  :+: Tuple
  :+: Expression
  :+: Relational
  :+: Shift
  :+: Simple
  :+: Term
  :+: Factor
  :+: Primary
  )

newtype Data a = Data { unData :: ASTF VHDLDomain a }

instance Type a => Syntactic (Data a)
  where
    type Domain   (Data a) = VHDLDomain
    type Internal (Data a) = a

    desugar = unData
    sugar   = Data

class    (Syntactic a, Domain a ~ VHDLDomain, Type (Internal a)) => Syntax a
instance (Syntactic a, Domain a ~ VHDLDomain, Type (Internal a)) => Syntax a

type instance PredicateExp Data = Type

--------------------------------------------------------------------------------
-- * Backend
--------------------------------------------------------------------------------

codeMotionInterface :: CodeMotionInterface VHDLDomain
codeMotionInterface = defaultInterfaceT sharable (const True)
  where
    sharable :: ASTF VHDLDomain a -> ASTF VHDLDomain b -> Bool
    sharable (Sym _) _ = False
    sharable (lam :$ _) _
      | Just _ <- prLam lam = False
    sharable _ (lam :$ _)
      | Just _ <- prLam lam = False
    sharable (sel :$ _) _
      | Just Sel1 <- prj sel = False
      | Just Sel2 <- prj sel = False
      | Just Sel3 <- prj sel = False
      | Just Sel4 <- prj sel = False
    sharable _ _ = True

showExpr :: (Syntactic a, Domain a ~ VHDLDomain) => a -> String
showExpr = render . codeMotion codeMotionInterface . desugar

showAST :: (Syntactic a, Domain a ~ VHDLDomain) => a -> String
showAST = Syntactic.showAST . codeMotion codeMotionInterface . desugar

eval :: (Syntactic a, Domain a ~ VHDLDomain) => a -> Internal a
eval = evalClosed . desugar

--------------------------------------------------------------------------------
-- * Frontend
--------------------------------------------------------------------------------

value :: Syntax a => Internal a -> a
value = sugar . injT . Lit

force :: Syntax a => a -> a
force = resugar

share :: (Syntax a, Syntax b) => a -> (a -> b) -> b
share = sugarSymT Let

--------------------------------------------------------------------------------
-- ** ...

-- logical operators
and, or, xor, xnor, nand, nor :: Data Bool -> Data Bool -> Data Bool
and  = sugarSymT And
or   = sugarSymT Or
xor  = sugarSymT Xor
xnor = sugarSymT Xnor
nand = sugarSymT Nand
nor  = sugarSymT Nor

-- relational operators
eq, neq :: (Type a, Eq a) => Data a -> Data a -> Data Bool
eq  = sugarSymT Eq
neq = sugarSymT Neq

lt, lte, gt, gte :: (Type a, Ord a) => Data a -> Data a -> Data Bool
lt  = sugarSymT Lt
lte = sugarSymT Lte
gt  = sugarSymT Gt
gte = sugarSymT Gte

-- shift operators
sll, srl, sla, sra, rol, ror :: (Type a, Bits a, Type b, Integral b) => Data a -> Data b -> Data a
sll = sugarSymT Sll
srl = sugarSymT Srl
sla = sugarSymT Sla
sra = sugarSymT Sra
rol = sugarSymT Rol
ror = sugarSymT Ror

-- adding operators
add, sub :: (Type a, Num a) => Data a -> Data a -> Data a
add = sugarSymT Add
sub = sugarSymT Sub

cat :: (Type a, Read a, Show a) => Data a -> Data a -> Data a
cat = sugarSymT Cat

-- multiplying operators
mul :: (Type a, Num a) => Data a -> Data a -> Data a
mul = sugarSymT Mul

div, mod, rem :: (Type a, Integral a) => Data a -> Data a -> Data a
div = sugarSymT Div
mod = sugarSymT Mod
rem = sugarSymT Rem

-- miscellaneous operators
exp :: (Type a, Num a, Type b, Integral b) => Data a -> Data b -> Data a
exp = sugarSymT Exp

abs :: (Type a, Num a) => Data a -> Data a
abs = sugarSymT Abs

not :: Data Bool -> Data Bool
not = sugarSymT Not

--------------------------------------------------------------------------------
-- ** ...

instance (Type a, Eq a) => Eq (Data a)
  where
    (==) = error "VHDL: equality checking is not supported"

instance (Type a, Ord a) => Ord (Data a)
  where
    compare = error "VHDL: compare is not supported"
    max     = error "VHDL: max is not supported"
    min     = error "VHDL: min is not supported"

instance (Type a, Bounded a) => Bounded (Data a)
  where
    minBound = value minBound
    maxBound = value maxBound

instance (Type a, Enum a) => Enum (Data a)
  where
    toEnum   = error "VHDL: toEnum is not supported"
    fromEnum = error "VHDL: fromEnum is not supported"

instance (Type a, Real a) => Real (Data a)
  where
    toRational = error "VHDL: toRational is not supported"

instance (Type a, Num a) => Num (Data a)
  where
    fromInteger = value . fromInteger
    (+) = add
    (-) = sub
    (*) = mul
    abs = abs
    signum = error "VHDL: signum is not supported"

instance (Type a, Integral a) => Integral (Data a)
  where
    quot = error "VHDL: quotient is not supported"
    rem  = rem
    div  = div
    mod  = mod
    quotRem = undefined
    divMod  = undefined
    toInteger = error "VHDL: toInteger is not supported"

instance (Type a, Fractional a) => Fractional (Data a)
  where
    (/)   = error "VHDL: floating point division is not yet supported"
    recip = (/) (value 1)
    fromRational = error "VHDL: fromRational is not supported"

--------------------------------------------------------------------------------
-- * Evaluation of Expressions
--------------------------------------------------------------------------------

instance EvaluateExp Data
  where
    litE  = value
    evalE = eval

--------------------------------------------------------------------------------
-- * Compilation of expressions
--------------------------------------------------------------------------------

instance CompileExp Data
  where
    varE  = undefined
    compT = undefined
    compE = undefined

--------------------------------------------------------------------------------
    
{-
compileT :: forall a. Rep a => Expr a -> VHDL Type
compileT _ = M.addType $ unTag $ (typed :: Tagged a TypeRep)

compileE :: Expr a -> VHDL Expression
compileE = return . lift . go
  where
    go :: forall a. Expr a -> Kind
    go exp = case exp of
      Var (Ident i) -> P $ M.name i
      Val v         -> P $ M.string $ format v

      -- ^ Tuples
      Pair l r -> P $ M.aggregate [(Nothing, lift (go l)), (Nothing, lift (go r))]
      Fst  p   -> P $ M.selected undefined undefined
      Snd  p   -> P $ M.selected undefined undefined

      -- ^ VHDL constructs
      And  x y -> E $ M.and  [lift (go x),  lift (go y)]
      Or   x y -> E $ M.or   [lift (go x),  lift (go y)]
      Xor  x y -> E $ M.xor  [lift (go x),  lift (go y)]
      Xnor x y -> E $ M.xnor [lift (go x),  lift (go y)]
      Nand x y -> E $ M.nand (lift (go x)) (lift (go y))
      Nor  x y -> E $ M.nor  (lift (go x)) (lift (go y))

      Eq   x y -> R $ M.eq   (lift (go x)) (lift (go y))
      Neq  x y -> R $ M.neq  (lift (go x)) (lift (go y))
      Lt   x y -> R $ M.lt   (lift (go x)) (lift (go y))
      Lte  x y -> R $ M.lte  (lift (go x)) (lift (go y))
      Gt   x y -> R $ M.gt   (lift (go x)) (lift (go y))
      Gte  x y -> R $ M.gte  (lift (go x)) (lift (go y))

      Sll  x y -> Sh $ M.sll (lift (go x)) (lift (go y))
      Srl  x y -> Sh $ M.srl (lift (go x)) (lift (go y))
      Sla  x y -> Sh $ M.sla (lift (go x)) (lift (go y))
      Sra  x y -> Sh $ M.sra (lift (go x)) (lift (go y))
      Rol  x y -> Sh $ M.rol (lift (go x)) (lift (go y))
      Ror  x y -> Sh $ M.ror (lift (go x)) (lift (go y))

      Neg  x   -> Si $ M.neg (lift (go x))
      Add  x y -> Si $ M.add [lift (go x), lift (go y)]
      Sub  x y -> Si $ M.sub [lift (go x), lift (go y)]
      Cat  x y -> Si $ M.cat [lift (go x), lift (go y)]

      Mul  x y -> P $ resize (unTag (width :: Tagged a Int))
                    $ M.mul  [lift (go x), lift (go y)]
      Dif  x y -> error "compilation of floating point division is not yet supported"
      Div  x y -> T $ M.div  [lift (go x), lift (go y)]
      Mod  x y -> T $ M.mod  [lift (go x), lift (go y)]
      Rem  x y -> T $ M.rem  [lift (go x), lift (go y)]

      Exp  x y -> F $ M.exp  (lift (go x)) (lift (go y))
      Abs  x   -> F $ M.abs  (lift (go x))
      Not  x   -> F $ M.not  (lift (go x))
-}
--------------------------------------------------------------------------------
