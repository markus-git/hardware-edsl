{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Language.Embedded.Hardware.Command.Backend.VHDL () where

import Control.Monad.Operational.Higher

import Language.Embedded.Hardware.Interface
import Language.Embedded.Hardware.Expression.Hoist
import Language.Embedded.Hardware.Command.CMD

import Language.Embedded.VHDL (VHDL)
import qualified Language.VHDL          as V
import qualified Language.Embedded.VHDL as V

import qualified Data.IORef    as IR
import qualified Data.Array.IO as IA

--------------------------------------------------------------------------------
-- * Translation of hardware commands into VHDL.
--------------------------------------------------------------------------------

class ToIdent a
  where
    toIdent :: a -> V.Identifier

instance ToIdent String       where toIdent               = V.Ident
instance ToIdent (Signal a)   where toIdent (SignalC i)   = V.Ident $ 's' : show i
instance ToIdent (Variable a) where toIdent (VariableC i) = V.Ident $ 'v' : show i
instance ToIdent (Array i a)  where toIdent (ArrayC i)    = V.Ident $ 'a' : show i

compEM :: forall exp a. (PredicateExp exp a, CompileExp exp) => Maybe (exp a) -> VHDL (Maybe V.Expression)
compEM e = maybe (return Nothing) (>>= return . Just) $ fmap compE e

compTM :: forall exp a. (PredicateExp exp a, CompileExp exp) => Maybe (exp a) -> VHDL V.Type
compTM _ = compT (undefined :: exp a)

evalEM :: forall exp a. (PredicateExp exp a, EvaluateExp exp) => Maybe (exp a) -> a
evalEM e = maybe (error "empty value") (id) $ fmap evalE e

freshVar :: forall exp a. (CompileExp exp, PredicateExp exp a) => VHDL (exp a, V.Identifier)
freshVar = do
  i <- varE <$> V.freshUnique :: VHDL (exp a)
  n <- dig  <$> compE i
  t <- compT (undefined :: exp a)
  V.addLocal $ V.declareVariable n t Nothing
  return (i, n)
  where
    -- diggity dig!
    dig :: V.Expression -> V.Identifier
    dig (V.ENand (V.Relation (V.ShiftExpression (V.SimpleExpression _ (V.Term (V.FacPrim (V.PrimName (V.NSimple i)) _)_)_)_)_)_) = i

--------------------------------------------------------------------------------
-- ** Signals.

instance CompileExp exp => Interp (SignalCMD exp) VHDL
  where
    interp = compileSignal

instance EvaluateExp exp => Interp (SignalCMD exp) IO
  where
    interp = runSignal

compileSignal :: forall exp a. CompileExp exp => SignalCMD exp VHDL a -> VHDL a
compileSignal (NewSignal clause scope mode exp) =
  do v <- compEM exp
     t <- compTM exp
     i <- SignalC <$> V.freshUnique
     let block     = V.declareSignal   (toIdent i)      t v
         interface = V.interfaceSignal (toIdent i) mode t v
     case scope of
       SProcess      -> V.addLocal  block
       SArchitecture -> V.addGlobal block
       SEntity       -> case clause of
         Port    -> V.addPort    interface
         Generic -> V.addGeneric interface
     return i
compileSignal (GetSignal s) =
  do (v, i) <- freshVar :: VHDL (a, V.Identifier)
     e <- compE v
     V.addSequential $ V.assignVariable i (lift $ V.PrimName $ V.NSimple $ toIdent s)
     return v
compileSignal (SetSignal s exp) =
  do V.addSequential =<< V.assignSignalS (toIdent s) <$> compE exp
compileSignal (UnsafeFreezeSignal (SignalC s)) =
  do return $ varE s

runSignal :: forall exp prog a. EvaluateExp exp => SignalCMD exp prog a -> IO a
runSignal (NewSignal _ _ _ exp)        = fmap SignalE $ IR.newIORef $ evalEM exp
runSignal (GetSignal (SignalE r))      = fmap litE $ IR.readIORef r
runSignal (SetSignal (SignalE r) exp)  = IR.writeIORef r $ evalE exp
runSignal (UnsafeFreezeSignal r)       = runSignal (GetSignal r)

--------------------------------------------------------------------------------
-- ** Variables.

instance CompileExp exp => Interp (VariableCMD exp) VHDL
  where
    interp = compileVariable

instance EvaluateExp exp => Interp (VariableCMD exp) IO
  where
    interp = runVariable

compileVariable :: forall exp a. CompileExp exp => VariableCMD exp VHDL a -> VHDL a
compileVariable (NewVariable exp) =
  do v <- compEM exp
     t <- compTM exp
     i <- VariableC <$> V.freshUnique
     V.addLocal $ V.declareVariable (toIdent i) t v
     return i
compileVariable (GetVariable var) =
  do (v, i) <- freshVar :: VHDL (a, V.Identifier)
     e <- compE v
     V.addSequential $ V.assignVariable i (lift $ V.PrimName $ V.NSimple $ toIdent var)
     return v
compileVariable (SetVariable var exp) =
  do V.addSequential =<< V.assignVariable (toIdent var) <$> compE exp
compileVariable (UnsafeFreezeVariable (VariableC v)) =
  do return $ varE v

runVariable :: forall exp prog a. EvaluateExp exp => VariableCMD exp prog a -> IO a
runVariable (NewVariable exp)               = fmap VariableE $ IR.newIORef $ evalEM exp
runVariable (GetVariable (VariableE v))     = fmap litE $ IR.readIORef v
runVariable (SetVariable (VariableE v) exp) = IR.writeIORef v $ evalE exp
runVariable (UnsafeFreezeVariable v)        = runVariable (GetVariable v)

--------------------------------------------------------------------------------
-- ** Arrays.

instance (CompileExp exp, EvaluateExp exp, CompArrayIx exp) => Interp (ArrayCMD exp) VHDL
  where
    interp = compileArray

instance EvaluateExp exp => Interp (ArrayCMD exp) IO
  where
    interp = runArray

-- *** Signal commands can be both sequential and parallel and I shouldn't
--     depend on them always being sequential.
compileArray :: forall exp a. (CompileExp exp, EvaluateExp exp, CompArrayIx exp) => ArrayCMD exp VHDL a -> VHDL a
compileArray (NewArray len) =
  do n <- compE  len
     t <- compTA len (undefined :: a)
     a <- freshA
     i <- ArrayC <$> V.freshUnique
     let range = V.range (lift n) V.downto zero
         array = V.constrainedArray a t range
     V.addType array
     V.addLocal $ V.declareVariable (toIdent i) (typed array) Nothing
     return i
compileArray (InitArray is) =
  do t <- compTA (undefined :: exp i) (undefined :: a)
     a <- freshA
     i <- ArrayC <$> V.freshUnique
     x <- sequence [compE (litE a :: exp b) | (a :: b) <- is]
     let len   = V.lit (length is)
         range = V.range (lift len) V.downto zero
         array = V.constrainedArray a t range
     V.addType array
     V.addLocal $ V.declareVariable (toIdent i) (typed array) (Just $ lift $ V.aggregate x)
     return i
compileArray (GetArray ix arr) =
  do (v, i) <- freshVar :: VHDL (a, V.Identifier)
     e <- compE ix
     V.addSequential $ V.assignVariable i (lift $ V.PrimName $ V.indexed (toIdent arr) e)
     return v
compileArray (SetArray i e arr) =
  do i' <- compE i
     e' <- compE e
     -- this could be concurrent as well.
     V.addSequential $ V.assignArray (V.indexed (toIdent arr) i') e'
compileArray (UnsafeGetArray ix arr) =
  case compArrayIx ix arr of
      Just e  -> return e
      Nothing -> do
        (v, i) <- freshVar :: VHDL (a, V.Identifier)
        e <- compE ix
        V.addSequential $ V.assignVariable i (lift $ V.PrimName $ V.indexed (toIdent arr) e)
        return v

runArray :: forall exp prog a. EvaluateExp exp => ArrayCMD exp prog a -> IO a
runArray (NewArray len)            = fmap ArrayE $ IA.newArray_ $ (,) 0 $ evalE len
runArray (InitArray is)            = fmap ArrayE $ IA.newListArray (0, fromIntegral $ length is) is
runArray (GetArray i (ArrayE a))   = fmap litE $ IA.readArray a $ evalE i
runArray (SetArray i e (ArrayE a)) = IA.writeArray a (evalE i) (evalE e)
runArray (UnsafeGetArray i a)      = runArray (GetArray i a)

--------------------------------------------------------------------------------

-- | Fresh array type identifier
freshA :: VHDL V.Identifier
freshA = toIdent . ('t' :) . show <$> V.freshUnique

-- | Compile type of array.
compTA :: forall exp i a. (PredicateExp exp a, CompileExp exp) => exp i -> Array i a -> VHDL V.Type
compTA _ _ = compT (undefined :: exp a)

zero :: V.SimpleExpression
zero = lift (V.lit 0)

typed :: V.TypeDeclaration -> V.SubtypeIndication
typed (V.TDFull    (V.FullTypeDeclaration i _))     = named i
typed (V.TDPartial (V.IncompleteTypeDeclaration i)) = named i

named :: V.Identifier -> V.SubtypeIndication
named i = V.SubtypeIndication Nothing (V.TMType (V.NSimple i)) Nothing

--------------------------------------------------------------------------------
-- ** Loops.

instance CompileExp exp => Interp (LoopCMD exp) VHDL
  where
    interp = compileLoop

instance EvaluateExp exp => Interp (LoopCMD exp) IO
  where
    interp = runLoop

compileLoop :: forall exp a. CompileExp exp => LoopCMD exp VHDL a -> VHDL a
compileLoop (For r step) =
  do hi     <- compE r
     (v, i) <- freshVar
     loop   <- V.inFor i (V.range zero V.to (lift hi)) (step v)
     V.addSequential $ V.SLoop $ loop
compileLoop (While b step) =
  do error "todo: compile while loops"

runLoop :: forall exp prog a. EvaluateExp exp => LoopCMD exp IO a -> IO a
runLoop (For r step) = loop (evalE r)
  where
    loop i | i > 0     = step (litE i) >> loop (i - 1)
           | otherwise = return ()
runLoop (While b step) = loop
  where
    loop = b >>= flip when (step >> loop) . evalE

--------------------------------------------------------------------------------
-- ** Conditional.

instance CompileExp exp => Interp (ConditionalCMD exp) VHDL
  where
    interp = compileConditional

instance EvaluateExp exp => Interp (ConditionalCMD exp) IO
  where
    interp = runConditional

compileConditional :: forall exp a. CompileExp exp => ConditionalCMD exp VHDL a -> VHDL a
compileConditional (If (a, b) cs em) =
  do let (es, ds) = unzip cs
         el       = maybe (return ()) id em
     ae  <- compE a
     ese <- mapM compE es
     s   <- V.inConditional (ae, b) (zip ese ds) el
     V.addSequential $ V.SIf s

runConditional :: forall exp a. EvaluateExp exp => ConditionalCMD exp IO a -> IO a
runConditional (If (a, b) cs em) = if (evalE a) then b else loop cs
  where
    loop []          = maybe (return ()) id em
    loop ((c, p):xs) = if (evalE c) then p else (loop xs)

--------------------------------------------------------------------------------
-- ** Structural.

instance CompileExp exp => Interp (StructuralCMD exp) VHDL
  where
    interp = compileStructural

instance EvaluateExp exp => Interp (StructuralCMD exp) IO
  where
    interp = runStructural

compileStructural :: forall exp a. CompileExp exp => StructuralCMD exp VHDL a -> VHDL a
compileStructural (Entity e prog)         = V.entity (toIdent e) prog
compileStructural (Architecture e a prog) = V.architecture (toIdent e) (toIdent a) prog
compileStructural (Process xs prog)       =
  do label  <- V.newLabel
     (a, c) <- V.inProcess label (fmap reveal xs) prog
     V.addConcurrent (V.ConProcess c)
     return a
  where
    reveal :: SignalX -> V.Identifier
    reveal (SignalX s) = toIdent s

runStructural :: forall exp a. EvaluateExp exp => StructuralCMD exp IO a -> IO a
runStructural (Entity _ prog)         = prog
runStructural (Architecture _ _ prog) = prog
runStructural (Process xs prog)       = error "todo: run process"

--------------------------------------------------------------------------------
