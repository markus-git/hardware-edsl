{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- these are needed for Hoist
{-# LANGUAGE TypeFamilies #-}

module Language.Embedded.VHDL.Expression
  ( Expr
  , not                             -- :: Expr Bool -> Expr Bool
  , and, or, xor, xnor, nand, nor   -- :: Expr Bool -> Expr Bool -> Expr Bool
  , eq, neq                         -- :: Eq a       => Expr a -> Expr a -> Expr Bool
  , lt, lte, gt, gte                -- :: Ord a      => Expr a -> Expr a -> Expr Bool
  , sll, srl, sla, sra, rol, ror    -- :: (Bits a, Integral b) => Expr a -> Expr b -> Expr a
  , add, sub, cat                   -- :: Num a      => Expr a -> Expr a -> Expr a
  , mul                             -- :: (Num a, Rep a) => Expr a -> Expr a -> Expr a
  , div, mod, rem                   -- :: Integral a => Expr a -> Expr a -> Expr a
  , neg                             -- :: Num a      => Expr a -> Expr a
  , exp                             -- :: Floating a => Expr a -> Expr a -> Expr a
  , abs                             -- :: Num a      => Expr a -> Expr a
  , name
  , lit
  ) where

import Language.VHDL (Identifier(..), Expression)

import Language.Embedded.VHDL.Interface
import Language.Embedded.VHDL.Monad             (VHDL)
import Language.Embedded.VHDL.Expression.Hoist
import Language.Embedded.VHDL.Expression.Format
import Language.Embedded.VHDL.Expression.Type hiding (Kind)
import qualified Language.Embedded.VHDL.Monad as M

import Data.Bits           hiding (xor)
import Data.Dynamic
import Data.Maybe                 (fromJust)
import Data.Typeable
import Data.TypePredicates hiding (sub)

import Prelude hiding (not, and, or, div, mod, rem, exp, abs, null)
import qualified Prelude as P

--------------------------------------------------------------------------------
-- * VHDL Expression Type - a union of VHDLs' types + variables
--------------------------------------------------------------------------------

data Expr a
  where
    Val :: Rep a => a          -> Expr a
    Var :: Rep a => Identifier -> Expr a

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
    Div  :: Integral a   => Expr a -> Expr a -> Expr a -- integer division
    Dif  :: Fractional a => Expr a -> Expr a -> Expr a -- floating point division
    Mod  :: Integral a   => Expr a -> Expr a -> Expr a
    Rem  :: Integral a   => Expr a -> Expr a -> Expr a

    -- misc. operators (minus Not)
    Exp  :: Floating a => Expr a -> Expr a -> Expr a
    Abs  :: Num a      => Expr a -> Expr a

type instance PredicateExp Expr = Rep

--------------------------------------------------------------------------------
-- ** Useful Instances

instance (Rep a, Bounded a) => Bounded (Expr a)
  where
    minBound = Val minBound
    maxBound = Val maxBound

instance (Rep a, Enum a) => Enum (Expr a) -- needed for integral
  where
    toEnum   = error "toEnum not supported"
    fromEnum = error "fromEnum not supported"

instance (Rep a, Num a) => Num (Expr a)
  where
    fromInteger  = Val . fromInteger
    (+)          = Add
    (-)          = Sub
    (*)          = Mul
    abs          = Abs
    signum       = error "signum not implemented for Expr"

--------------------------------------------------------------------------------
-- ** Logical operators

and, or, xor, xnor, nand, nor :: Expr Bool -> Expr Bool -> Expr Bool

and  = And
or   = Or
xor  = Xor
xnor = Xnor
nand = Nand
nor  = Nor

--------------------------------------------------------------------------------
-- ** Relational operators

eq, neq          :: Eq a  => Expr a -> Expr a -> Expr Bool
lt, lte, gt, gte :: Ord a => Expr a -> Expr a -> Expr Bool

eq  = Eq
neq = Neq
lt  = Lt
lte = Lte
gt  = Gt
gte = Gte

--------------------------------------------------------------------------------
-- ** Shift operators

sll, srl, sra, sla, rol, ror :: (Bits a, Integral b) => Expr a -> Expr b -> Expr a

sll = Sll
srl = Srl
sra = Sra
sla = Sla
rol = Rol
ror = Ror

--------------------------------------------------------------------------------
-- ** Adding operators

add, sub, cat :: Num a => Expr a -> Expr a -> Expr a

add = Add
sub = Sub
cat = Cat

--------------------------------------------------------------------------------
-- ** Multiplying operators

mul           :: (Num a, Rep a) => Expr a -> Expr a -> Expr a
div, mod, rem :: Integral a => Expr a -> Expr a -> Expr a

mod = Mod
rem = Rem
mul = Mul
div = Div

--------------------------------------------------------------------------------
-- ** Sign operators

neg :: Num a => Expr a -> Expr a
neg = Neg

--------------------------------------------------------------------------------
-- ** Miscellaneous operators

not :: Expr Bool -> Expr Bool
not = Not

exp :: Floating a => Expr a -> Expr a -> Expr a
exp = Exp

abs :: Num a => Expr a -> Expr a
abs = Abs

--------------------------------------------------------------------------------
-- ** Naming operators

name :: Rep a => Identifier -> Expr a
name = Var

lit :: Rep a => a -> Expr a
lit = Val

--------------------------------------------------------------------------------
-- ** Formal operators
--
-- These are dangerous... as in no type difference between formal/actual names.
-- Do not use with 'lit', only 'name'.

risingEdge :: Identifier -> Expr Bool
risingEdge (Ident formal) = name . Ident $ "rising_edge(" ++ formal ++ ")"

-- ...

--------------------------------------------------------------------------------
-- * Compilation of expressions
--------------------------------------------------------------------------------

instance CompileExp Expr
  where
    varE  = Var
    compT = compileT
    compE = compileE

compileT :: forall a. Rep a => Expr a -> VHDL Type
compileT _ = return $ unTag $ (typed :: Tagged a Type)

compileE :: Expr a -> VHDL Expression
compileE = return . lift . go
  where
    go :: forall a. Expr a -> Kind
    go exp = case exp of
      Var v -> P $ M.name   $ show   v
      Val v -> P $ M.string $ format v

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

      Mul  x y -> P $ resize (unTag (width :: Tagged a Int)) $ M.mul  [lift (go x), lift (go y)]
      Dif  x y -> error "compilation of floating point division is not yet supported"
      Div  x y -> T $ M.div  [lift (go x), lift (go y)]
      Mod  x y -> T $ M.mod  [lift (go x), lift (go y)]
      Rem  x y -> T $ M.rem  [lift (go x), lift (go y)]

      Exp  x y -> F $ M.exp  (lift (go x)) (lift (go y))
      Abs  x   -> F $ M.abs  (lift (go x))
      Not  x   -> F $ M.not  (lift (go x))

--------------------------------------------------------------------------------
-- * Evaluation of Expressions
--------------------------------------------------------------------------------

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
    Sla  x y -> bin (\a -> shiftL a  . fromIntegral) x y
    Sra  x y -> bin (\a -> shiftR a  . fromIntegral) x y
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

--------------------------------------------------------------------------------
