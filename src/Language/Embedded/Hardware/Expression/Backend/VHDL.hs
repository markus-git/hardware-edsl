{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Embedded.Hardware.Expression.Backend.VHDL where

import Language.Syntactic
import Language.Syntactic.Functional (Denotation, evalSym)

import Language.Embedded.Hardware.Expression.Syntax
import Language.Embedded.Hardware.Expression.Frontend (value, var)
import Language.Embedded.Hardware.Expression.Represent
import Language.Embedded.Hardware.Expression.Hoist (Kind)
import Language.Embedded.Hardware.Interface
import qualified Language.Embedded.Hardware.Expression.Hoist as Hoist

import Language.Embedded.VHDL (VHDL)
import qualified Language.VHDL          as VHDL
import qualified Language.Embedded.VHDL as VHDL

import Control.Applicative

--------------------------------------------------------------------------------
-- * Compilation and evaluation of hardware expressions for VHDL.
--------------------------------------------------------------------------------

instance FreeExp HExp
  where
    type PredicateExp HExp = HType
    litE = value
    varE = var

--------------------------------------------------------------------------------

instance EvaluateExp HExp
  where
    evalE = evalHExp

evalHExp :: HExp a -> a
evalHExp = go . unHExp
  where
    go :: AST T sig -> Denotation sig
    go (Sym (T s)) = evalSym s
    go (f :$ a)    = go f $ go a

--------------------------------------------------------------------------------

instance CompileExp HExp
  where
    compE  = compHExp

compHType :: forall a. HType a => HExp a -> VHDL VHDL.Type
compHType _ = declare (undefined :: proxy a)

compHExp  :: forall a. HExp a -> VHDL VHDL.Expression
compHExp  e = Hoist.lift <$> compSimple e
  where
    compSimple :: HExp b -> VHDL Kind
    compSimple = simpleMatch (\(T s) -> compDomain s) . unHExp

    compLoop   :: ASTF T b -> VHDL Kind
    compLoop   = compSimple . HExp

    compDomain :: forall sig. HType (DenResult sig) => Dom sig -> Args (AST T) sig -> VHDL Kind
    compDomain expr (x :* y :* _)
      | Just And  <- prj expr = go $ \a b -> VHDL.and  [a, b]
      | Just Or   <- prj expr = go $ \a b -> VHDL.or   [a, b]
      | Just Xor  <- prj expr = go $ \a b -> VHDL.xor  [a, b]
      | Just Xnor <- prj expr = go $ \a b -> VHDL.xnor [a, b]
      | Just Nand <- prj expr = go VHDL.nand
      | Just Nor  <- prj expr = go VHDL.nor
      where
        go :: (VHDL.Relation -> VHDL.Relation -> VHDL.Expression) -> VHDL Kind
        go f = do
          x' <- Hoist.lift <$> compLoop x
          y' <- Hoist.lift <$> compLoop y
          return $ Hoist.E $ f x' y'
    compDomain relate (x :* y :* _)
      | Just Eq  <- prj relate = go VHDL.eq
      | Just Neq <- prj relate = go VHDL.neq
      | Just Lt  <- prj relate = go VHDL.lt
      | Just Lte <- prj relate = go VHDL.lte
      | Just Gt  <- prj relate = go VHDL.gt
      | Just Gte <- prj relate = go VHDL.gte
      where
        go :: (VHDL.ShiftExpression -> VHDL.ShiftExpression -> VHDL.Relation) -> VHDL Kind
        go f = do
          x' <- Hoist.lift <$> compLoop x
          y' <- Hoist.lift <$> compLoop y
          return $ Hoist.R $ f x' y'
    compDomain shift (x :* y :* _)
      | Just Sll <- prj shift = go $ VHDL.sll
      | Just Srl <- prj shift = go $ VHDL.srl
      | Just Sla <- prj shift = go $ VHDL.sla
      | Just Sra <- prj shift = go $ VHDL.sra
      | Just Rol <- prj shift = go $ VHDL.rol
      | Just Ror <- prj shift = go $ VHDL.ror
      where
        go :: (VHDL.SimpleExpression -> VHDL.SimpleExpression -> VHDL.ShiftExpression) -> VHDL Kind
        go f = do
          x' <- Hoist.lift <$> compLoop x
          y' <- Hoist.lift <$> compLoop y
          return $ Hoist.Sh $ f x' y'
    compDomain simple (x :* y :* _)
      | Just Add <- prj simple = go VHDL.add
      | Just Sub <- prj simple = go VHDL.sub
      | Just Cat <- prj simple = go VHDL.cat
      where
        go :: ([VHDL.Term] -> VHDL.SimpleExpression) -> VHDL Kind
        go f = do
          x' <- Hoist.lift <$> compLoop x
          y' <- Hoist.lift <$> compLoop y
          return $ Hoist.Si $ f [x', y']
    compDomain simple (x :* _)
      | Just Neg <- prj simple = do
          x' <- Hoist.lift <$> compLoop x
          return $ Hoist.Si $ VHDL.neg x'
      | Just Pos <- prj simple = do
          x' <- Hoist.lift <$> compLoop x
          return $ Hoist.Si x'
    compDomain term (x :* y :* _)
      | Just Mul <- prj term = go VHDL.mul
      | Just Div <- prj term = go VHDL.div
      | Just Mod <- prj term = go VHDL.mod
      | Just Rem <- prj term = go VHDL.rem
      where
        go :: ([VHDL.Factor] -> VHDL.Term) -> VHDL Kind
        go f = do
          x' <- Hoist.lift <$> compLoop x
          y' <- Hoist.lift <$> compLoop y
          return $ Hoist.T $ f [x', y']
    compDomain factor (x :* y :* _)
      | Just Exp <- prj factor = do
          x' <- Hoist.lift <$> compLoop x
          y' <- Hoist.lift <$> compLoop y
          return $ Hoist.F $ VHDL.exp x' y'
    compDomain factor (x :* _)
      | Just Abs <- prj factor = do
          x' <- Hoist.lift <$> compLoop x
          return $ Hoist.F $ VHDL.abs x'
      | Just Not <- prj factor = do
          x' <- Hoist.lift <$> compLoop x
          return $ Hoist.F $ VHDL.not x'

    compDomain primary (x :* Nil)
      | Just (Qualified t)  <- prj primary = do
          f  <- compHType (undefined :: HExp (DenResult sig))
          x' <- Hoist.lift <$> compLoop x
          return $ Hoist.P $ VHDL.qualified f x'
      | Just (Conversion f) <- prj primary = do
          t  <- compHType (undefined :: HExp (DenResult sig))
          x' <- Hoist.lift <$> compLoop x
          return $ x'
          --return $ Hoist.P $ VHDL.cast t x' *** Apply cast ***
      | Just (Others) <- prj primary = do
          x' <- Hoist.lift <$> compLoop x
          return $ Hoist.P $ VHDL.aggregate $ VHDL.others x'
    compDomain primary args
      | Just (Name n)       <- prj primary = return $ Hoist.P $ VHDL.name n
      | Just (Literal i)    <- prj primary = return $ Hoist.P $ VHDL.lit $ format i
      | Just (Aggregate a)  <- prj primary = return $ Hoist.P $ VHDL.aggregate a
      | Just (Function f _) <- prj primary = do
          as <- sequence $ listArgs compLoop args
          return $ Hoist.P $ VHDL.function (VHDL.Ident f) (fmap Hoist.lift as)
      | Just (Allocator)    <- prj primary = error "expression-backend: todo"

--------------------------------------------------------------------------------
