{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}

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
import qualified Language.VHDL as V

import Language.Embedded.VHDL.Interface
import Language.Embedded.VHDL.Monad           (VHDL, TypeRep(..))
import qualified Language.Embedded.VHDL.Monad as M
import Language.Embedded.VHDL.Expression.Hoist
import qualified Language.Embedded.VHDL.Expression.Hoist as Hoist
import Language.Embedded.VHDL.Expression.Format
import qualified Language.Embedded.VHDL.Expression.Type as T

import Language.Syntactic hiding (fold, printExpr, showAST, drawAST, writeHtmlAST)
import qualified Language.Syntactic as Syntactic
import Language.Syntactic.Functional
import Language.Syntactic.Functional.Sharing
import Language.Syntactic.Functional.Tuple
import Language.Syntactic.Sugar.BindingT ()
import Language.Syntactic.Sugar.TupleT ()

import Data.Bits     (Bits)
import qualified Data.Bits as Bits
import Data.Maybe    (fromJust)
import Data.Typeable (Typeable)
import Data.Word     (Word8)

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

type VHDLDomain = Typed (
  -- Syntactic
      BindingT
  :+: Let
  :+: Tuple
  :+: Construct
  -- VHDL constructs
  :+: Expression
  :+: Relational
  :+: Shift
  :+: Simple
  :+: Term
  :+: Factor
  :+: Primary)

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
    (/)   = error "VHDL: floating point division is not _yet_ supported"
    recip = (/) (value 1)
    fromRational = error "VHDL: fromRational is not supported"

--------------------------------------------------------------------------------
-- ** ...

type Length = Word8
type Index  = Word8

-- ...
data Vector a where
  Indexed :: Data Length -> (Data Index -> a) -> Vector a

-- ...
type Matrix a = Vector (Vector (Data a))

instance (Rep (Internal a), Syntax a) => Syntactic (Vector a)
  where
    type Domain (Vector a)   = VHDLDomain
    type Internal (Vector a) = [Internal a]
    desugar = desugar . freezeVector . mapVector resugar
    sugar   = mapVector resugar . thawVector . sugar

instance Rep a => Rep [a]
  where
    format = error "format not supported for vectors"
    width  = error "width not supported for vectors yet"
    typed  = error "typed not supported for vectors yet"

infixl 9 !

length :: Vector a -> Data Length
length (Indexed len _) = len

indexed :: Data Length -> (Data Index -> a) -> Vector a
indexed = Indexed

index :: Vector a -> Data Index -> a
index (Indexed _ ixf) = ixf

(!) :: Vector a -> Data Index -> a
(!) (Indexed _ ixf) i = ixf i

freezeVector :: Type a => Vector (Data a) -> Data [a]
freezeVector vec = undefined

thawVector :: Type a => Data [a] -> Vector (Data a)
thawVector arr = undefined

mapVector :: (a -> b) -> Vector a -> Vector b
mapVector f (Indexed len ixf) = Indexed len (f . ixf)

--------------------------------------------------------------------------------

-- | Get the length of an array
arrLength :: Type a => Data [a] -> Data Length
arrLength = sugarSymT $ Construct "arrLength" eval
  where
    eval []     = 0
    eval (a:as) = 1 + eval as

-- | Index into an array
arrIx :: (Type a, Rep a) => Data [a] -> Data Index -> Data a
arrIx = sugarSymT $ Construct "arrIndex" eval
  where
    eval (a:as) 0 = a
    eval (a:as) n = eval as (n - 1)
    
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
    varE  i = undefined
    compT t = undefined
    compE e = undefined

--------------------------------------------------------------------------------
-- ** ...

compileT :: forall a. Rep a => ASTF VHDLDomain a -> VHDL T.Type
compileT _ = M.addType (unTag (typed :: Tagged a TypeRep))

instance (Rep a, Rep b) => Rep (a, b) where
  format = error "format not supported for tuples"
  width  =
    let l = unTag (width :: Tagged a Int)
        r = unTag (width :: Tagged b Int)
     in Tag (l + r)
  typed  =
    let l = unTag (typed :: Tagged a TypeRep)
        r = unTag (typed :: Tagged b TypeRep)
     in Tag (Composite l r)

--------------------------------------------------------------------------------
-- ** ...

vars :: Name -> String
vars v = 'v' : show v

compileE :: ASTF VHDLDomain a -> VHDL Kind
compileE var
  | Just (Var v) <- prj var = return $ P $ M.name $ vars v
compileE val
  | Just (Lit v) <- prj val = return $ P $ M.string $ format v
compileE (lets :$ a :$ (lam :$ body))
  | Just (Let)    <- prj lets
  , Just (LamT v) <- prj lam
  = do let v' = Ident $ vars v
       a' <- lift <$> compileE a
       M.addSequential $ M.assignVariable v' a'
       compileE body
     -- that we pick variable assignment might be a problem
compileE (expr :$ x :$ y)
  | Just And  <- prj expr = go $ \a b -> M.and  [a, b]
  | Just Or   <- prj expr = go $ \a b -> M.or   [a, b]
  | Just Xor  <- prj expr = go $ \a b -> M.xor  [a, b]
  | Just Xnor <- prj expr = go $ \a b -> M.xnor [a, b]
  | Just Nand <- prj expr = go $ M.nand
  | Just Nor  <- prj expr = go $ M.nor
  where
    go :: (V.Relation -> V.Relation -> V.Expression) -> VHDL Kind
    go f = bin (\a b -> Hoist.E $ f (lift a) (lift b)) x y
compileE (relate :$ x :$ y)
  | Just Eq  <- prj relate = go $ M.eq
  | Just Neq <- prj relate = go $ M.neq
  | Just Lt  <- prj relate = go $ M.lt
  | Just Lte <- prj relate = go $ M.lte
  | Just Gt  <- prj relate = go $ M.gt
  | Just Gte <- prj relate = go $ M.gte
  where
    go :: (V.ShiftExpression -> V.ShiftExpression -> V.Relation) -> VHDL Kind
    go f = bin (\a b -> Hoist.R $ f (lift a) (lift b)) x y
compileE (shift :$ x :$ y)
  | Just Sll <- prj shift = go $ M.sll
  | Just Srl <- prj shift = go $ M.srl
  | Just Sla <- prj shift = go $ M.sla
  | Just Sra <- prj shift = go $ M.sra
  | Just Rol <- prj shift = go $ M.rol
  | Just Ror <- prj shift = go $ M.ror
  where
    go :: (V.SimpleExpression -> V.SimpleExpression -> V.ShiftExpression) -> VHDL Kind
    go f = bin (\a b -> Hoist.Sh $ f (lift a) (lift b)) x y
compileE (simple :$ x)
  | Just Neg <- prj simple = go $ M.neg
  | Just Pos <- prj simple = go $ lift
  where
    go :: (V.Term -> V.SimpleExpression) -> VHDL Kind
    go f = un (\a -> Hoist.Si $ f (lift a)) x
compileE (simple :$ x :$ y)
  | Just Add <- prj simple = go $ \a b -> M.add [a, b]
  | Just Sub <- prj simple = go $ \a b -> M.sub [a, b]
  | Just Cat <- prj simple = go $ \a b -> M.cat [a, b]
  where
    go :: (V.Term -> V.Term -> V.SimpleExpression) -> VHDL Kind
    go f = bin (\a b -> Hoist.Si $ f (lift a) (lift b)) x y
compileE (term :$ x :$ y)
  | Just Mul <- prj term = go $ \a b -> M.mul [a, b]
  | Just Div <- prj term = go $ \a b -> M.div [a, b]
  | Just Mod <- prj term = go $ \a b -> M.mod [a, b]
  | Just Rem <- prj term = go $ \a b -> M.rem [a, b]
  where
    go :: (V.Factor -> V.Factor -> V.Term) -> VHDL Kind
    go f = bin (\a b -> Hoist.T $ f (lift a) (lift b)) x y
compileE (factor :$ x :$ y)
  | Just Exp <- prj factor = go $ M.exp
  where
    go :: (V.Primary -> V.Primary -> V.Factor) -> VHDL Kind
    go f = bin (\a b -> Hoist.F $ f (lift a) (lift b)) x y
compileE (factor :$ x)
  | Just Abs <- prj factor = go $ M.abs
  | Just Not <- prj factor = go $ M.not
  where
    go :: (V.Primary -> V.Factor) -> VHDL Kind
    go f = un (\a -> Hoist.F $ f (lift a)) x
compileE (primary)
  | Just (Lit i) <- prj primary = return $ Hoist.P $ M.lit $ format i

-- ...
un :: (Kind -> Kind) -> ASTF VHDLDomain a -> VHDL Kind
un f x = compileE x >>= return . f

-- ...
bin
  :: (Kind -> Kind -> Kind)
  -> ASTF VHDLDomain a
  -> ASTF VHDLDomain b
  -> VHDL Kind
bin f x y = do
  x' <- compileE x
  y' <- compileE y
  return $ f x' y'

--------------------------------------------------------------------------------
