{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- these are needed for Hoist
{-# LANGUAGE TypeFamilies #-}

module Language.Embedded.VHDL.Expression
  ( Expr
  , and, or, xor, xnor
  , eq, neq, lt, lte, gt, gte
  , nand, nor
  , sll, srl, sla, sra, rol, ror
  , add, sub, cat
  , neg
  , mul, div, mod, rem
  , exp
  , abs, not
  , name
  , lit
  ) where

import Language.VHDL (Identifier(..), Expression)

import Language.Embedded.VHDL.Interface
import Language.Embedded.VHDL.Monad             (VHDL, Type)
import Language.Embedded.VHDL.Expression.Hoist
import Language.Embedded.VHDL.Expression.Format
import qualified Language.Embedded.VHDL.Monad as M

import Data.Bits hiding (xor)
import Data.Dynamic
import Data.Typeable
import Data.TypePredicates hiding (sub)

import Prelude hiding (not, and, or, div, mod, rem, exp, abs, null)
import qualified Prelude as P

--------------------------------------------------------------------------------
-- * VHDL Expression Type - a union of VHDLs' types + variables
--------------------------------------------------------------------------------

data Expr a
  where
    Val :: Kludge a   => a          -> Expr a
    Var :: Typeable a => Identifier -> Expr a

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
    Sll  :: Bits a => Expr a -> Expr a -> Expr a
    Srl  :: Bits a => Expr a -> Expr a -> Expr a
    Sla  :: Bits a => Expr a -> Expr a -> Expr a
    Sra  :: Bits a => Expr a -> Expr a -> Expr a
    Rol  :: Bits a => Expr a -> Expr a -> Expr a
    Ror  :: Bits a => Expr a -> Expr a -> Expr a

    -- adding operators
    Neg  :: Num a => Expr a -> Expr a
    Add  :: Num a => Expr a -> Expr a -> Expr a
    Sub  :: Num a => Expr a -> Expr a -> Expr a
    Cat  :: Num a => Expr a -> Expr a -> Expr a

    -- multiplying operators
    Mul  :: Num a        => Expr a -> Expr a -> Expr a
    Div  :: Integral a   => Expr a -> Expr a -> Expr a
    Dif  :: Fractional a => Expr a -> Expr a -> Expr a
    Mod  :: Integral a   => Expr a -> Expr a -> Expr a
    Rem  :: Integral a   => Expr a -> Expr a -> Expr a

    -- misc. operators (minus Not)
    Exp  :: Floating a => Expr a -> Expr a -> Expr a
    Abs  :: Num a      => Expr a -> Expr a

type instance PredicateExp Expr = Typeable :/\: Show :/\: Kludge

--------------------------------------------------------------------------------
-- ** Useful Instances

instance (Kludge a, Num a, Eq a) => Num (Expr a)
  where
    fromInteger   = Val . fromInteger
    Val a + Val b = Val (a+b)
    Val 0 + b     = b
    a     + Val 0 = a
    a     + b     = Add a b
    Val a - Val b = Val (a-b)
    Val 0 - b     = b
    a     - Val 0 = a
    a     - b     = Sub a b
    Val a * Val b = Val (a*b)
    Val 0 * b     = Val 0
    a     * Val 0 = Val 0
    Val 1 * b     = b
    a     * Val 1 = a
    a     * b     = Mul a b

    abs    = error "abs not implemented for Expr"
    signum = error "signum not implemented for Expr"

--------------------------------------------------------------------------------
-- ** Evaluation

instance EvaluateExp Expr
  where
    litE  = Val
    evalE = evaluate (error "eval: free variable")

evaluate :: (Identifier -> Dynamic) -> Expr a -> a
evaluate env exp = case exp of
    Var  v | Just a <- fromDynamic (env v) -> a
    Val  v                                 -> v
    
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

    Sll  x y -> error "evaluation of Sll not yet implemented for Expr"
    Srl  x y -> error "evaluation of Srl not yet implemented for Expr"  
    Sla  x y -> error "evaluation of Sla not yet implemented for Expr"  
    Sra  x y -> error "evaluation of Sra not yet implemented for Expr"  
    Rol  x y -> error "evaluation of Rol not yet implemented for Expr"  
    Ror  x y -> error "evaluation of Ror not yet implemented for Expr"  

    Neg  x   -> error "evaluation of Neg not yet implemented for Expr"   
    Add  x y -> error "evaluation of Add not yet implemented for Expr"  
    Sub  x y -> error "evaluation of Sub not yet implemented for Expr"  
    Cat  x y -> error "evaluation of Cat not yet implemented for Expr" 

    Mul  x y -> error "evaluation of Mul not yet implemented for Expr" 
    Div  x y -> error "evaluation of Div not yet implemented for Expr"
    Dif  x y -> error "evaluation of Dif not yet implemented for Expr"
    Mod  x y -> error "evaluation of Mod not yet implemented for Expr" 
    Rem  x y -> error "evaluation of Rem not yet implemented for Expr" 

    Exp  x y -> error "evaluation of Exp not yet implemented for Expr"
    Abs  x   -> error "evaluation of Abs not yet implemented for Expr"
  where
    xor a b = (a || b) && P.not (a && b)
    
    un :: (a -> b) -> Expr a -> b
    un  f x   = f (evaluate env x)

    bin :: (a -> b -> c) -> Expr a -> Expr b -> c
    bin f x y = f (evaluate env x) (evaluate env y)

--------------------------------------------------------------------------------
-- ** Compilation

instance CompileExp Expr
  where
    varE  = Var
    compE = compileE
    compT = compileT

compileE :: Expr a -> VHDL Expression
compileE = return . lift . go
  where
    go :: Expr e -> Kind
    go exp = case exp of
      Var v -> P $ M.name $ showI v
      Val v -> P $ M.lit  $ format v

      And  x y -> E $ M.and  [lift (go x), lift (go y)]
      Or   x y -> E $ M.or   [lift (go x), lift (go y)]
      Xor  x y -> E $ M.xor  [lift (go x), lift (go y)]
      Xnor x y -> E $ M.xnor [lift (go x), lift (go y)]
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

      Mul  x y -> T $ M.mul  [lift (go x), lift (go y)]
      Div  x y -> T $ M.div  [lift (go x), lift (go y)]
      Dif  x y -> error "compilation of Dif not yet implemented for Expr"
      Mod  x y -> T $ M.mod  [lift (go x), lift (go y)]
      Rem  x y -> T $ M.rem  [lift (go x), lift (go y)]

      Exp  x y -> F $ M.exp  (lift (go x)) (lift (go y))
      Abs  x   -> F $ M.abs  (lift (go x))
      Not  x   -> F $ M.not  (lift (go x))

showI :: Identifier -> String
showI (Ident s) = s

-- *** add 'use' statements
compileT :: forall a. Typeable a => Expr a -> VHDL Type
compileT _ = return $ case show (typeOf (undefined :: a)) of
  "Bool" -> M.std_logic
  'I':'n':'t':xs -> case xs of
    "8"  -> M.signed8
    "16" -> M.signed16
    "32" -> M.signed32
    "64" -> M.signed64
  'W':'o':'r':'d':xs -> case xs of
    "8"  -> M.usigned8
    "16" -> M.usigned16
    "32" -> M.usigned32
    "64" -> M.usigned64
  _ -> error "compT: type not supported"

--------------------------------------------------------------------------------
-- * User Interface
--------------------------------------------------------------------------------

not  :: Expr Bool -> Expr Bool
not  = Not

and, or, xor, xnor, nand, nor :: Expr Bool -> Expr Bool -> Expr Bool
and  = And
or   = Or
xor  = Xor
xnor = Xnor
nand = Nand
nor  = Nor

eq, neq :: Eq a => Expr a -> Expr a -> Expr Bool
eq  = Eq
neq = Neq

lt, lte, gt, gte :: Ord a => Expr a -> Expr a -> Expr Bool
lt  = Lt
lte = Lte
gt  = Gt
gte = Gte

sll, srl, sra, sla, rol, ror :: Bits a => Expr a -> Expr a -> Expr a
sll = Sll
srl = Srl
sra = Sra
sla = Sla
rol = Rol
ror = Ror

add, sub, cat :: Num a => Expr a -> Expr a -> Expr a
add = Add
sub = Sub
cat = Cat

neg :: Num a => Expr a -> Expr a
neg = Neg

mul :: Num a => Expr a -> Expr a -> Expr a
mul = Mul

div :: Integral a => Expr a -> Expr a -> Expr a
div = Div

mod, rem :: Integral a => Expr a -> Expr a -> Expr a
mod = Mod
rem = Rem

exp :: Floating a => Expr a -> Expr a -> Expr a
exp = Exp

abs :: Num a => Expr a -> Expr a
abs = Abs

name :: Typeable a => Identifier -> Expr a
name = Var

lit :: Kludge a => a -> Expr a
lit = Val

--------------------------------------------------------------------------------
