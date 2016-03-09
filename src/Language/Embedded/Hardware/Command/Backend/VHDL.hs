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

import Data.Array.IO (freeze)
import Data.List     (genericTake)
import Data.Word     (Word8)
import qualified Data.IORef    as IR
import qualified Data.Array.IO as IA

--------------------------------------------------------------------------------
-- * Translation of hardware commands into VHDL.
--------------------------------------------------------------------------------

ident :: VarId -> V.Identifier
ident = V.Ident

compEM :: forall exp a. (PredicateExp exp a, CompileExp exp) => Maybe (exp a) -> VHDL (Maybe V.Expression)
compEM e = maybe (return Nothing) (>>= return . Just) $ fmap compE e

compTM :: forall exp a. (PredicateExp exp a, CompileExp exp) => Maybe (exp a) -> VHDL V.Type
compTM _ = compT (undefined :: exp a)

evalEM :: forall exp a. (PredicateExp exp a, EvaluateExp exp) => Maybe (exp a) -> a
evalEM e = maybe (error "empty value") (id) $ fmap evalE e

freshVar :: forall exp a. (CompileExp exp, PredicateExp exp a) => String -> VHDL (exp a, V.Identifier)
freshVar prefix = do
  i <- var <$> V.freshUnique
  n <- dig <$> compE i
  t <- compT (undefined :: exp a)
  V.addLocal $ V.declareVariable n t Nothing
  return (i, n)
  where
    var :: Integer -> exp a
    var v = varE $ prefix ++ show v
    
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
compileSignal (NewSignal base clause scope mode exp) =
  do v <- compEM exp
     t <- compTM exp
     i <- V.newSym base
     let block     = V.declareSignal   (ident i)      t v
         interface = V.interfaceSignal (ident i) mode t v
     case scope of
       SProcess      -> V.addLocal  block
       SArchitecture -> V.addGlobal block
       SEntity       -> case clause of
         Port    -> V.addPort    interface
         Generic -> V.addGeneric interface
     return (SignalC i)
compileSignal (GetSignal (SignalC s)) =
  do (v, i) <- freshVar "s" :: VHDL (a, V.Identifier)
     e <- compE v
     V.addSequential $ V.assignVariable i (lift $ V.PrimName $ V.NSimple $ ident s)
     return v
compileSignal (SetSignal (SignalC s) exp) =
  do V.addSequential =<< V.assignSignalS (ident s) <$> compE exp
compileSignal (UnsafeFreezeSignal (SignalC s)) =
  do return $ varE s

runSignal :: forall exp prog a. EvaluateExp exp => SignalCMD exp prog a -> IO a
runSignal (NewSignal _ _ _ _ exp)     = fmap SignalE $ IR.newIORef $ evalEM exp
runSignal (GetSignal (SignalE r))     = fmap litE $ IR.readIORef r
runSignal (SetSignal (SignalE r) exp) = IR.writeIORef r $ evalE exp
runSignal (UnsafeFreezeSignal r)      = runSignal (GetSignal r)

--------------------------------------------------------------------------------
-- ** Variables.

instance CompileExp exp => Interp (VariableCMD exp) VHDL
  where
    interp = compileVariable

instance EvaluateExp exp => Interp (VariableCMD exp) IO
  where
    interp = runVariable

compileVariable :: forall exp a. CompileExp exp => VariableCMD exp VHDL a -> VHDL a
compileVariable (NewVariable base exp) =
  do v <- compEM exp
     t <- compTM exp
     i <- V.newSym base
     V.addLocal $ V.declareVariable (ident i) t v
     return (VariableC i)
compileVariable (GetVariable (VariableC var)) =
  do (v, i) <- freshVar "v" :: VHDL (a, V.Identifier)
     e <- compE v
     V.addSequential $ V.assignVariable i (lift $ V.PrimName $ V.NSimple $ ident var)
     return v
compileVariable (SetVariable (VariableC var) exp) =
  do V.addSequential =<< V.assignVariable (ident var) <$> compE exp
compileVariable (UnsafeFreezeVariable (VariableC v)) =
  do return $ varE v

runVariable :: forall exp prog a. EvaluateExp exp => VariableCMD exp prog a -> IO a
runVariable (NewVariable _ exp)             = fmap VariableE $ IR.newIORef $ evalEM exp
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
compileArray (NewArray base len) =
  do n <- compE  len
     t <- compTA len (undefined :: a)
     a <- freshA   base
     i <- V.newSym base
     let range = V.range (lift n) V.downto zero
         array = V.constrainedArray a t range
     V.addType array
     V.addLocal $ V.declareVariable (ident i) (typed array) Nothing
     return (ArrayC i)
compileArray (InitArray base is) =
  do t <- compTA (undefined :: exp i) (undefined :: a)
     a <- freshA   base
     i <- V.newSym base
     x <- sequence [compE (litE a :: exp b) | (a :: b) <- is]
     let len   = V.lit (length is)
         range = V.range (lift len) V.downto zero
         array = V.constrainedArray a t range
     V.addType array
     V.addLocal $ V.declareVariable (ident i) (typed array) (Just $ lift $ V.aggregate x)
     return (ArrayC i)
compileArray (GetArray ix (ArrayC arr)) =
  do (v, i) <- freshVar "a" :: VHDL (a, V.Identifier)
     e <- compE ix
     V.addSequential $ V.assignVariable i (lift $ V.PrimName $ V.indexed (ident arr) e)
     return v
compileArray (SetArray i e (ArrayC arr)) =
  do i' <- compE i
     e' <- compE e
     -- this could be concurrent as well.
     V.addSequential $ V.assignArray (V.indexed (ident arr) i') e'
compileArray (CopyArray (ArrayC a) (ArrayC b) l) =
  do len <- compE l
     let slice = (lift (V.lit (0 :: Word8)), lift len)
         dest  = V.slice (ident a) slice
         src   = V.slice (ident b) slice
     V.addSequential $ V.assignArray src (lift $ V.PrimName dest)
compileArray (UnsafeFreezeArray (ArrayC a)) = return $ IArrayC a

runArray :: forall exp prog a. EvaluateExp exp => ArrayCMD exp prog a -> IO a
runArray (NewArray _ len)          = fmap ArrayE . IR.newIORef =<< IA.newArray_ (0, evalE len)
runArray (InitArray _ is)          = fmap ArrayE . IR.newIORef =<< IA.newListArray (0, fromIntegral $ length is) is
runArray (GetArray i (ArrayE a))   = do r <- IR.readIORef a; fmap litE $ IA.readArray r (evalE i)
runArray (SetArray i e (ArrayE a)) = do r <- IR.readIORef a; IA.writeArray r (evalE i) (evalE e)
runArray (UnsafeFreezeArray (ArrayE a))      = fmap IArrayE . freeze =<< IR.readIORef a
runArray (CopyArray (ArrayE a) (ArrayE b) l) =
  do arr <- IR.readIORef a
     brr <- IR.readIORef b
     sequence_
       [ IA.readArray brr i >>= IA.writeArray arr i
       | i <- genericTake (evalE l) [0..]
       ]

--------------------------------------------------------------------------------

-- | Fresh array type identifier
freshA :: String -> VHDL V.Identifier
freshA prefix = ident . ('t' :) . show <$> V.newSym prefix

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
     (v, i) <- freshVar "l"
     loop   <- V.inFor i (V.range zero V.to (lift hi)) (step v)
     V.addSequential $ V.SLoop $ loop
compileLoop (While cont step) =
  do l    <- V.newLabel
     loop <- V.inWhile l (Nothing) $
       do b    <- cont
          exit <- compE b
          V.exit l exit
          step
     V.addSequential $ V.SLoop $ loop

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
-- ** Components.

instance CompileExp exp => Interp (ComponentCMD exp) VHDL
  where
    interp = compileComponent

instance EvaluateExp exp => Interp (ComponentCMD exp) IO
  where
    interp = runComponent

compileComponent :: forall exp a. CompileExp exp => ComponentCMD exp VHDL a -> VHDL a
compileComponent (Component sig) =
  do u <- V.newSym "c"
     let ename = ident $ "e_" ++ u
         aname = ident $ "a_" ++ u
         pname = ident $ "p_" ++ u
     V.component $
       do (is, m) <- V.entity ename $ declarations [] sig
          V.architecture ename aname $
            do (_, p) <- V.inProcess pname is m
               V.addConcurrent (V.ConProcess p)
     return (Just u)
  where
    -- go over the sig and declare expected signals, keep inputs and program.
    declarations :: [V.Identifier] -> Sig exp VHDL b -> VHDL ([V.Identifier], VHDL ())
    declarations is (Unit m) = return (is, m)
    declarations is (Lam  m (f :: Signal c -> Sig exp VHDL d)) =
      do t <- compT (undefined :: exp c)
         i <- V.newSym "s"
         V.addPort $ V.interfaceSignal (ident i) m t Nothing
         case m of
           V.In -> declarations (ident i : is) $ f (SignalC i)
           _    -> declarations is $ f (SignalC i)
compileComponent (PortMap (Process (Just name) sig) as) =
  do let comp = V.Ident name
     l  <- V.newSym "s"
     is <- V.declareComponent comp $ applications [] sig as
     V.addConcurrent $ V.portMap (ident l) comp (reverse is)
  where
    -- go over the sig and apply each argument, saving their idents.
    applications :: [V.Identifier] -> Sig exp VHDL b -> Arg b -> VHDL [V.Identifier]
    applications is (Unit  _) (Nill)   = return is
    applications is (Lam m (f :: Signal c -> Sig exp VHDL d)) (s@(SignalC n) :> g) = do
      t <- compT (undefined :: exp c)
      i <- V.newSym "s"
      V.addPort $ V.interfaceSignal (ident i) m t Nothing
      applications (ident n : is) (f s) g

runComponent :: forall exp a. EvaluateExp exp => ComponentCMD exp IO a -> IO a
runComponent (Component _)                  = return Nothing
runComponent (PortMap (Process _ sig) args) =
  do error "hardware-edsl-todo: figure out how to simulate processes in Haskell."

--------------------------------------------------------------------------------
-- ** Structural.

instance CompileExp exp => Interp (StructuralCMD exp) VHDL
  where
    interp = compileStructural

instance EvaluateExp exp => Interp (StructuralCMD exp) IO
  where
    interp = runStructural

compileStructural :: forall exp a. CompileExp exp => StructuralCMD exp VHDL a -> VHDL a
compileStructural (StructEntity e prog)         = V.entity (ident e) prog
compileStructural (StructArchitecture e a prog) = V.architecture (ident e) (ident a) prog
compileStructural (StructProcess xs prog)       =
  do label  <- V.newLabel
     (a, c) <- V.inProcess label (fmap reveal xs) prog
     V.addConcurrent (V.ConProcess c)
     return a
  where
    reveal :: SignalX -> V.Identifier
    reveal (SignalX (SignalC s)) = ident s

runStructural :: forall exp a. EvaluateExp exp => StructuralCMD exp IO a -> IO a
runStructural (StructEntity _ prog)         = prog
runStructural (StructArchitecture _ _ prog) = prog
runStructural (StructProcess xs prog)       =
  do error "hardware-edsl-todo: figure out how to simulate processes in Haskell."

--------------------------------------------------------------------------------
