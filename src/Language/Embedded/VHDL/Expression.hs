{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Language.Embedded.VHDL.Expression where

import Language.VHDL (Identifier(..))
import qualified Language.VHDL as V

import Language.Embedded.VHDL.Interface
import Language.Embedded.VHDL.Monad (VHDL)
import Language.Embedded.VHDL.Command (CompArrayIx)
import Language.Embedded.VHDL.Expression.Hoist (Hoist, Kind)
import Language.Embedded.VHDL.Expression.Represent
import qualified Language.Embedded.VHDL.Monad            as M
import qualified Language.Embedded.VHDL.Expression.Hoist as Hoist

import Language.Embedded.VHDL.Monad.Type (
    std_logic
  , signed, usigned
  , signed8, signed16, signed32, signed64
  , usigned8, usigned16, usigned32, usigned64
  )
import qualified Language.Embedded.VHDL.Monad.Type as T

import Language.Syntactic hiding (fold, printExpr, showAST, drawAST, writeHtmlAST)
import Language.Syntactic.Functional hiding (Literal, Name)
import qualified Language.Syntactic as Syntactic
import qualified Language.Syntactic.Functional as Syntactic

import Control.Arrow
import Control.Applicative (liftA)

import Data.Bits     (Bits)
import Data.Maybe    (fromJust)
import Data.Typeable (Typeable)
import Data.Word     (Word8)
import qualified Data.Bits as Bits

import Prelude hiding (not, and, or, abs, rem, div, mod, exp)
import qualified Prelude

--------------------------------------------------------------------------------
-- * ...
--------------------------------------------------------------------------------

-- | Collection of commonly required classes required by VHDL expressions.
class    (Typeable a, Rep a, Eq a) => VType a
instance (Typeable a, Rep a, Eq a) => VType a

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
    Eq   :: VType a => Relational (a :-> a :-> Full Bool)
    Neq  :: VType a => Relational (a :-> a :-> Full Bool)
    Lt   :: (VType a, Ord a) => Relational (a :-> a :-> Full Bool)
    Lte  :: (VType a, Ord a) => Relational (a :-> a :-> Full Bool)
    Gt   :: (VType a, Ord a) => Relational (a :-> a :-> Full Bool)
    Gte  :: (VType a, Ord a) => Relational (a :-> a :-> Full Bool)

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
    Sll :: (VType a, Bits a, VType b, Integral b) => Shift (a :-> b :-> Full a)
    Srl :: (VType a, Bits a, VType b, Integral b) => Shift (a :-> b :-> Full a)
    Sla :: (VType a, Bits a, VType b, Integral b) => Shift (a :-> b :-> Full a)
    Sra :: (VType a, Bits a, VType b, Integral b) => Shift (a :-> b :-> Full a)
    Rol :: (VType a, Bits a, VType b, Integral b) => Shift (a :-> b :-> Full a)
    Ror :: (VType a, Bits a, VType b, Integral b) => Shift (a :-> b :-> Full a)

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

--------------------------------------------------------------------------------
-- ** ...

data Simple sig
  where
    Neg :: (VType a, Num a) => Simple (a :->       Full a)
    Pos :: (VType a, Num a) => Simple (a :->       Full a)
    Add :: (VType a, Num a) => Simple (a :-> a :-> Full a)
    Sub :: (VType a, Num a) => Simple (a :-> a :-> Full a)
    Cat :: (VType a, Show a, Read a) => Simple (a :-> a :-> Full a)

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
    Mul :: (VType a, Num a)      => Term (a :-> a :-> Full a)
    Div :: (VType a, Integral a) => Term (a :-> a :-> Full a)
    Mod :: (VType a, Integral a) => Term (a :-> a :-> Full a)
    Rem :: (VType a, Integral a) => Term (a :-> a :-> Full a)

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
    Exp :: (VType a, Num a, VType b, Integral b) => Factor (a :-> b :-> Full a)
    Abs :: (VType a, Num a)                      => Factor (a :-> Full a)
    Not ::                                          Factor (Bool :-> Full Bool)

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
    Name       :: (VType a)          => V.Name   -> Primary (Full a)
    Literal    :: (VType a)          => a        -> Primary (Full a)
    Aggregate  :: (VType a)          => [a]      -> Primary (Full [a])
    Function   :: (Signature sig)    => String   -> Denotation sig -> Primary sig
    Qualified  :: (VType a, VType b) => b        -> Primary (a :-> Full a)
    Conversion :: (VType a, VType b) => (a -> b) -> Primary (a :-> Full b)
    Allocator  :: (VType a)          =>             Primary (Full a)

instance Equality   Primary
instance StringTree Primary

instance Symbol Primary
  where
    symSig (Name _)       = signature
    symSig (Literal _)    = signature
    symSig (Aggregate _)  = undefined
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
    evalSym (Name _)       = error "VHDL: cannot eval open name!"
    evalSym (Literal i)    = i
    evalSym (Aggregate xs) = xs
    evalSym (Function _ f) = f
    evalSym (Qualified _)  = error "?"
    evalSym (Conversion f) = f
    evalSym (Allocator)    = undefined

instance EvalEnv Primary env

--------------------------------------------------------------------------------
-- ** ...

-- | Domain of VHDL expressions.
type Dom =
      Expression
  :+: Relational
  :+: Shift
  :+: Simple
  :+: Term
  :+: Factor
  :+: Primary

-- | Typed VHDL expressions.
data T sig
  where
    T :: VType (DenResult sig) => { unT :: Dom sig } -> T sig

-- | VHDL Expressions.
newtype VExp a = VExp { unVExp :: ASTF T a }

instance Syntactic (VExp a)
  where
    type Domain   (VExp a) = T
    type Internal (VExp a) = a

    desugar = unVExp
    sugar   = VExp

instance CompArrayIx VExp

type instance PredicateExp VExp = VType

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- * Frontend
--------------------------------------------------------------------------------

-- | Specialized sugarSym for T.
sugarT
  :: ( Signature (SmartSig fi), sub :<: Dom
     , T :<: SmartSym fi
     , SyntacticN f fi
     , SmartFun (SmartSym fi) (SmartSig fi) ~ fi
     , VType (DenResult (SmartSig fi)))
  => sub (SmartSig fi)
  -> f
sugarT sym = sugarSym (T $ inj sym)

-- | ...
value :: VType a => a -> VExp a
value i = sugarT (Literal i)

-- | ...
variable :: VType a => Integer -> VExp a
variable v = variable' $ 'v' : show v

-- | ...
variable' :: VType a => String -> VExp a
variable' = sugarT . Name . V.NSimple . V.Ident

-- | ...
cast  :: (VType a, VType b) => (a -> b) -> VExp a -> VExp b
cast f = sugarT (Conversion f)

--------------------------------------------------------------------------------

true, false :: VExp Bool
true  = value True
false = value False

-- logical operators
and, or, xor, xnor, nand, nor :: VExp Bool -> VExp Bool -> VExp Bool
and  = sugarT And
or   = sugarT Or
xor  = sugarT Xor
xnor = sugarT Xnor
nand = sugarT Nand
nor  = sugarT Nor

-- relational operators
eq, neq :: (VType a, Eq a) => VExp a -> VExp a -> VExp Bool
eq  = sugarT Eq
neq = sugarT Neq

lt, lte, gt, gte :: (VType a, Ord a) => VExp a -> VExp a -> VExp Bool
lt  = sugarT Lt
lte = sugarT Lte
gt  = sugarT Gt
gte = sugarT Gte

-- shift operators
sll, srl, sla, sra, rol, ror :: (VType a, Bits a, VType b, Integral b) => VExp a -> VExp b -> VExp a
sll = sugarT Sll
srl = sugarT Srl
sla = sugarT Sla
sra = sugarT Sra
rol = sugarT Rol
ror = sugarT Ror

-- adding operators
add, sub :: (VType a, Num a) => VExp a -> VExp a -> VExp a
add = sugarT Add
sub = sugarT Sub

cat :: (VType a, Read a, Show a) => VExp a -> VExp a -> VExp a
cat = sugarT Cat

-- multiplying operators
mul :: (VType a, Num a) => VExp a -> VExp a -> VExp a
mul = sugarT Mul

div, mod, rem :: (VType a, Integral a) => VExp a -> VExp a -> VExp a
div = sugarT Div
mod = sugarT Mod
rem = sugarT Rem

-- miscellaneous operators
exp :: (VType a, Num a, VType b, Integral b) => VExp a -> VExp b -> VExp a
exp = sugarT Exp

abs :: (VType a, Num a) => VExp a -> VExp a
abs = sugarT Abs

not :: VExp Bool -> VExp Bool
not = sugarT Not

--------------------------------------------------------------------------------
-- ** ...

instance (VType a, Eq a) => Eq (VExp a)
  where
    (==) = error "VHDL: equality checking is not supported"

instance (VType a, Ord a) => Ord (VExp a)
  where
    compare = error "VHDL: compare is not supported"
    max     = error "VHDL: max is not supported"
    min     = error "VHDL: min is not supported"

instance (VType a, Bounded a) => Bounded (VExp a)
  where
    minBound = value minBound
    maxBound = value maxBound

instance (VType a, Enum a) => Enum (VExp a)
  where
    toEnum   = error "VHDL: toEnum is not supported"
    fromEnum = error "VHDL: fromEnum is not supported"

instance (VType a, Real a) => Real (VExp a)
  where
    toRational = error "VHDL: toRational is not supported"

instance (VType a, Num a) => Num (VExp a)
  where
    fromInteger = value . fromInteger
    (+)         = add
    (-)         = sub
    (*)         = mul
    abs         = abs
    signum      = error "VHDL: signum is not supported"

instance (VType a, Integral a) => Integral (VExp a)
  where
    quot         = error "VHDL: quotient is not supported"
    rem          = rem
    div          = div
    mod          = mod
    quotRem  a b = (quot a b, rem a b)
    divMod   a b = (div  a b, mod a b)
    toInteger    = error "VHDL: toInteger is not supported"

instance (VType a, Fractional a) => Fractional (VExp a)
  where
    (/)          = error "VHDL: floating point division is not _yet_ supported"
    recip        = (/) (value 1)
    fromRational = error "VHDL: fromRational is not supported"    

--------------------------------------------------------------------------------
-- * Evaluation of Expressions
--------------------------------------------------------------------------------

instance EvaluateExp VExp
  where
    litE  = value
    evalE = evalVExp

--------------------------------------------------------------------------------

evalVExp :: VExp a -> a
evalVExp = go . unVExp
  where
    go :: AST T sig -> Denotation sig
    go (Sym (T s)) = evalSym s
    go (f :$ a)    = go f $ go a

--------------------------------------------------------------------------------
-- * Compilation of expressions
--------------------------------------------------------------------------------

instance CompileExp VExp
  where
    varE  = variable
    
    compT = compVType

    compE = liftA Hoist.lift . compVExp

--------------------------------------------------------------------------------

compVType :: forall a. VType a => VExp a -> VHDL T.Type
compVType _ =
  do let t = unTag (typed :: Tagged a T.Type)
     declare (undefined :: a)
     return t

compVExp :: VExp a -> VHDL Kind
compVExp = simpleMatch (\(T s) -> compVDom s) . unVExp
  where
    compVExp' :: ASTF T a -> VHDL Kind
    compVExp' = compVExp . VExp

    compVDom :: forall sig. VType (DenResult sig) => Dom sig -> Args (AST T) sig -> VHDL Kind
    compVDom var  args
      | Just (Var v) <- prj var = return $ Hoist.P $ M.name $ 'v' : show v
    compVDom expr (x :* y :* _)
      | Just And  <- prj expr = go $ \a b -> M.and  [a, b]
      | Just Or   <- prj expr = go $ \a b -> M.or   [a, b]
      | Just Xor  <- prj expr = go $ \a b -> M.xor  [a, b]
      | Just Xnor <- prj expr = go $ \a b -> M.xnor [a, b]
      | Just Nand <- prj expr = go M.nand
      | Just Nor  <- prj expr = go M.nor
      where
        go :: (V.Relation -> V.Relation -> V.Expression) -> VHDL Kind
        go f = do
          x' <- Hoist.lift <$> compVExp' x
          y' <- Hoist.lift <$> compVExp' y
          return $ Hoist.E $ f x' y'
    compVDom relate (x :* y :* _)
      | Just Eq  <- prj relate = go M.eq
      | Just Neq <- prj relate = go M.neq
      | Just Lt  <- prj relate = go M.lt
      | Just Lte <- prj relate = go M.lte
      | Just Gt  <- prj relate = go M.gt
      | Just Gte <- prj relate = go M.gte
      where
        go :: (V.ShiftExpression -> V.ShiftExpression -> V.Relation) -> VHDL Kind
        go f = do
          x' <- Hoist.lift <$> compVExp' x
          y' <- Hoist.lift <$> compVExp' y
          return $ Hoist.R $ f x' y'
    compVDom shift (x :* y :* _)
      | Just Sll <- prj shift = go $ M.sll
      | Just Srl <- prj shift = go $ M.srl
      | Just Sla <- prj shift = go $ M.sla
      | Just Sra <- prj shift = go $ M.sra
      | Just Rol <- prj shift = go $ M.rol
      | Just Ror <- prj shift = go $ M.ror
      where
        go :: (V.SimpleExpression -> V.SimpleExpression -> V.ShiftExpression) -> VHDL Kind
        go f = do
          x' <- Hoist.lift <$> compVExp' x
          y' <- Hoist.lift <$> compVExp' y
          return $ Hoist.Sh $ f x' y'
    compVDom simple (x :* y :* _)
      | Just Add <- prj simple = go M.add
      | Just Sub <- prj simple = go M.sub
      | Just Cat <- prj simple = go M.cat
      where
        go :: ([V.Term] -> V.SimpleExpression) -> VHDL Kind
        go f = do
          x' <- Hoist.lift <$> compVExp' x
          y' <- Hoist.lift <$> compVExp' y
          return $ Hoist.Si $ f [x', y']
    compVDom simple (x :* _)
      | Just Neg <- prj simple = do
          x' <- Hoist.lift <$> compVExp' x
          return $ Hoist.Si $ M.neg x'
      | Just Pos <- prj simple = do
          x' <- Hoist.lift <$> compVExp' x
          return $ Hoist.Si x'
    compVDom term (x :* y :* _)
      | Just Mul <- prj term = go M.mul
      | Just Div <- prj term = go M.div
      | Just Mod <- prj term = go M.mod
      | Just Rem <- prj term = go M.rem
      where
        go :: ([V.Factor] -> V.Term) -> VHDL Kind
        go f = do
          x' <- Hoist.lift <$> compVExp' x
          y' <- Hoist.lift <$> compVExp' y
          return $ Hoist.T $ f [x', y']
    compVDom factor (x :* y :* _)
      | Just Exp <- prj factor = do
          x' <- Hoist.lift <$> compVExp' x
          y' <- Hoist.lift <$> compVExp' y
          return $ Hoist.F $ M.exp x' y'
    compVDom factor (x :* _)
      | Just Abs <- prj factor = do
          x' <- Hoist.lift <$> compVExp' x
          return $ Hoist.F $ M.abs x'
      | Just Not <- prj factor = do
          x' <- Hoist.lift <$> compVExp' x
          return $ Hoist.F $ M.not x'
    compVDom primary (x :* Nil)
      | Just (Qualified t)  <- prj primary = do
          f  <- compVType (undefined :: VExp (DenResult sig))
          x' <- Hoist.lift <$> compVExp' x
          return $ Hoist.P $ M.qualified f x'
      | Just (Conversion f) <- prj primary = do
          t  <- compVType (undefined :: VExp (DenResult sig))
          x' <- Hoist.lift <$> compVExp' x
          return $ Hoist.P $ M.cast t x'
    compVDom primary args
      | Just (Name n)       <- prj primary = case n of
          (V.NSimple (V.Ident i)) -> return $ Hoist.P $ M.name i
      | Just (Literal i)    <- prj primary = return $ Hoist.P $ M.lit $ format i
      | Just (Aggregate xs) <- prj primary = do
          return $ Hoist.P $ M.aggregate $ fmap (Hoist.lift . M.lit . format) xs
      | Just (Function f _) <- prj primary = do
          as <- sequence $ listArgs compVExp' args
          return $ Hoist.P $ M.function (V.Ident f) (fmap Hoist.lift as)
      | Just (Allocator)    <- prj primary = undefined

--------------------------------------------------------------------------------
-- *** temporary random shit becuase Cabal fails to install a package ***

temporaryGetName :: Primary sig -> String
temporaryGetName (Name (V.NSimple (V.Ident i))) = i

--------------------------------------------------------------------------------
