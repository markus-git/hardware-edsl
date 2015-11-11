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

import Data.Bits     (Bits)
import qualified Data.Bits as Bits
import Data.Maybe    (fromJust)
import Data.Typeable (Typeable)
import Data.Syntactic hiding (fold, printExpr, showAST, drawAST, writeHtmlAST)
import qualified Data.Syntactic as Syntactic
import Data.Syntactic.Functional
import Data.Syntactic.Sugar.BindingT ()

import Prelude hiding (and, or)
import qualified Prelude

--------------------------------------------------------------------------------
-- * ...
--------------------------------------------------------------------------------

class    (Typeable a, Rep a, Eq a, Ord a) => Type a
instance (Typeable a, Rep a, Eq a, Ord a) => Type a

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

interpretationInstances ''Expression

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
    evalSym Xor  = \x y -> (x && not y) || (not x && y)
    evalSym Xnor = \x y -> (not x && not y) || (x && y)
    evalSym Nand = \x y -> not (x && y)
    evalSym Nor  = \x y -> not (x || y)

instance EvalEnv Expression env

--------------------------------------------------------------------------------
-- ** ...

data Relational sig
  where
    Eq   :: Type a => Relational (a :-> a :-> Full Bool)
    Neq  :: Type a => Relational (a :-> a :-> Full Bool)
    Lt   :: Type a => Relational (a :-> a :-> Full Bool)
    Lte  :: Type a => Relational (a :-> a :-> Full Bool)
    Gt   :: Type a => Relational (a :-> a :-> Full Bool)
    Gte  :: Type a => Relational (a :-> a :-> Full Bool)

interpretationInstances ''Relational

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


--------------------------------------------------------------------------------


{-
data Expr a
  where
    -- ^ ...
    Val :: Rep a => a          -> Expr a
    Var :: Rep a => Identifier -> Expr a

    -- ^ VHDL Records
    Pair :: Expr a -> Expr b -> Expr (a, b)
    Fst  :: Expr (a, b) -> Expr a
    Snd  :: Expr (a, b) -> Expr b

    -- ^ VHDL Arrays
    -- ...

    -- ^ VHDL Expressions
    
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
    Sll  :: (Bits a, Integral b) => Expr a -> Expr b -> Expr a
    Srl  :: (Bits a, Integral b) => Expr a -> Expr b -> Expr a
    Sla  :: (Bits a, Integral b) => Expr a -> Expr b -> Expr a
    Sra  :: (Bits a, Integral b) => Expr a -> Expr b -> Expr a
    Rol  :: (Bits a, Integral b) => Expr a -> Expr b -> Expr a
    Ror  :: (Bits a, Integral b) => Expr a -> Expr b -> Expr a

    -- adding operators
    Neg  :: Num a => Expr a -> Expr a
    Add  :: Num a => Expr a -> Expr a -> Expr a
    Sub  :: Num a => Expr a -> Expr a -> Expr a
    Cat  :: Num a => Expr a -> Expr a -> Expr a

    -- multiplying operators
    Mul  :: (Rep a, Num a) => Expr a -> Expr a -> Expr a
    Dif  :: Fractional a   => Expr a -> Expr a -> Expr a -- floating point division
    Div  :: Integral a     => Expr a -> Expr a -> Expr a -- integer division
    Mod  :: Integral a     => Expr a -> Expr a -> Expr a
    Rem  :: Integral a     => Expr a -> Expr a -> Expr a

    -- misc. operators (minus Not)
    Exp  :: Floating a => Expr a -> Expr a -> Expr a
    Abs  :: Num a      => Expr a -> Expr a

type instance PredicateExp Expr = Rep
-}

--------------------------------------------------------------------------------
-- ** Useful Expr Instances
{-
instance (Rep a, Eq a) => Eq (Expr a)
  where
    (==) = error "equality checking not supported"

instance (Rep a, Ord a) => Ord (Expr a)
  where
    compare = error "compare not supported"
    max     = error "max not supported"
    min     = error "min not supported"

instance (Rep a, Bounded a) => Bounded (Expr a)
  where
    minBound = Val minBound
    maxBound = Val maxBound

instance (Rep a, Enum a) => Enum (Expr a) -- needed for integral
  where
    toEnum   = error "toEnum not supported"
    fromEnum = error "fromEnum not supported"

instance (Rep a, Real a) => Real (Expr a) -- needed for integral
  where
    toRational = error "toRational not supported"

instance (Rep a, Num a) => Num (Expr a)
  where
    fromInteger  = Val . fromInteger
    (+)          = Add
    (-)          = Sub
    (*)          = Mul
    abs          = Abs
    signum       = error "signum not implemented for Expr"

instance (Rep a, Integral a) => Integral (Expr a)
  where
    quot      = error "quotient not supported"
    rem       = Rem
    div       = Div
    mod       = Mod
    quotRem a b = (quot a b, rem a b)
    divMod  a b = (div  a b, mod a b)
    toInteger = error "toInteger not supported"

instance (Rep a, Fractional a) => Fractional (Expr a)
  where
    (/)   = Dif
    recip = Dif (Val 1)
    fromRational = error "fromRational not supported"
-}
--------------------------------------------------------------------------------
-- * User interface
--------------------------------------------------------------------------------
{-
pair :: Expr a -> Expr b -> Expr (a, b)
pair = Pair

first :: Expr (a, b) -> Expr a
first = Fst

second :: Expr (a, b) -> Expr b
second = Snd
-}
--------------------------------------------------------------------------------
-- ** Logical operators
{-
and, or, xor, xnor, nand, nor :: Expr Bool -> Expr Bool -> Expr Bool

and  = And
or   = Or
xor  = Xor
xnor = Xnor
nand = Nand
nor  = Nor
-}
--------------------------------------------------------------------------------
-- ** Relational operators
{-
eq, neq          :: Eq a  => Expr a -> Expr a -> Expr Bool
lt, lte, gt, gte :: Ord a => Expr a -> Expr a -> Expr Bool

eq  = Eq
neq = Neq
lt  = Lt
lte = Lte
gt  = Gt
gte = Gte
-}
--------------------------------------------------------------------------------
-- ** Shift operators
{-
sll, srl, sra, sla, rol, ror :: (Bits a, Integral b) => Expr a -> Expr b -> Expr a

sll = Sll
srl = Srl
sra = Sra
sla = Sla
rol = Rol
ror = Ror
-}
--------------------------------------------------------------------------------
-- ** Adding operators
{-
add, sub, cat :: Num a => Expr a -> Expr a -> Expr a

add = Add
sub = Sub
cat = Cat
-}
--------------------------------------------------------------------------------
-- ** Multiplying operators
{-
mul           :: (Num a, Rep a) => Expr a -> Expr a -> Expr a
div, mod, rem :: Integral a     => Expr a -> Expr a -> Expr a

mod = Mod
rem = Rem
mul = Mul
div = Div
-}
--------------------------------------------------------------------------------
-- ** Sign operators
{-
neg :: Num a => Expr a -> Expr a
neg = Neg
-}
--------------------------------------------------------------------------------
-- ** Miscellaneous operators
{-
not :: Expr Bool -> Expr Bool
not = Not

exp :: Floating a => Expr a -> Expr a -> Expr a
exp = Exp

abs :: Num a => Expr a -> Expr a
abs = Abs
-}
--------------------------------------------------------------------------------
-- ** Naming operators
{-
name :: Rep a => Identifier -> Expr a
name = Var

lit :: Rep a => a -> Expr a
lit = Val
-}
--------------------------------------------------------------------------------
-- * Compilation of expressions
--------------------------------------------------------------------------------
{-
instance CompileExp Expr
  where
    varE  = Var
    compT = compileT
    compE = compileE

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
-- * Evaluation of Expressions
--------------------------------------------------------------------------------
{-
instance EvaluateExp Expr
  where
    litE  = Val
    evalE = evaluate

evaluate :: Expr a -> a
evaluate exp = case exp of
    Var  v -> error "eval: free variable"
    Val  v -> v
    
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

    Sll  x y -> bin (\a b -> shiftL a (fromIntegral b) `clearBit` msb a) x y
    Srl  x y -> bin (\a b -> shiftR a (fromIntegral b) `clearBit` msb a) x y  
    Sla  x y -> bin (\a -> shiftL a . fromIntegral) x y
    Sra  x y -> bin (\a -> shiftR a . fromIntegral) x y
    Rol  x y -> bin (\a -> rotateL a . fromIntegral) x y
    Ror  x y -> bin (\a -> rotateR a . fromIntegral) x y

    Neg  x   -> un negate x
    Add  x y -> bin (+) x y
    Sub  x y -> bin (-) x y
    Cat  x y -> error "evaluation of bit concatenation not yet implemented" 

    Mul  x y -> bin (*)   x y
    Dif  x y -> bin (/)   x y
    Div  x y -> bin P.div x y
    Mod  x y -> bin P.mod x y
    Rem  x y -> bin P.rem x y

    Exp  x y -> bin (**) x y
    Abs  x   -> un P.abs x
  where
    xor a b = (a || b) && P.not (a && b)
    
    un :: (a -> b) -> Expr a -> b
    un  f x   = f (evaluate x)

    bin :: (a -> b -> c) -> Expr a -> Expr b -> c
    bin f x y = f (evaluate x) (evaluate y)

    msb :: Bits a => a -> Int
    msb = fromJust . bitSizeMaybe
-}
--------------------------------------------------------------------------------
