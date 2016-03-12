{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Language.Embedded.Hardware.Command.Backend.VHDL () where

import Control.Monad.Operational.Higher

import Language.Embedded.Hardware.Interface
import Language.Embedded.Hardware.Expression.Hoist
import Language.Embedded.Hardware.Expression.Represent
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

evalEM :: forall exp a. (PredicateExp exp a, EvaluateExp exp) => Maybe (exp a) -> a
evalEM e = maybe (error "empty value") (id) $ fmap evalE e

compEM :: forall exp a. (PredicateExp exp a, CompileExp exp) => Maybe (exp a) -> VHDL (Maybe V.Expression)
compEM e = maybe (return Nothing) (>>= return . Just) $ fmap compE e

compTM :: forall exp a. (PredicateExp exp a, CompileExp exp) => Maybe (exp a) -> VHDL V.Type
compTM _ = compT (undefined :: exp a)

compTA
  :: forall array exp i a.
     ( PredicateExp exp a
     , PredicateExp exp i
     , CompileExp   exp
     , EvaluateExp  exp
     )
  => V.Range -> exp i -> array i a -> VHDL V.Type
compTA r _ _ =
  do typ  <- compT (undefined :: exp a)
     name <- ident <$> newSym (Base "type")
     let array = V.constrainedArray name typ r
     V.addType array
     return (typed array)
  where
    typed :: V.TypeDeclaration -> V.SubtypeIndication
    typed (V.TDFull    (V.FullTypeDeclaration i _))     = named i
    typed (V.TDPartial (V.IncompleteTypeDeclaration i)) = named i

    named :: V.Identifier -> V.SubtypeIndication
    named i = V.SubtypeIndication Nothing (V.TMType (V.NSimple i)) Nothing

compFT :: forall exp m a b. (CompileExp exp, PredicateExp exp a) => (Signal a -> Sig exp m b) -> VHDL V.Type
compFT _ = compT (undefined :: exp a)

--------------------------------------------------------------------------------

newSym :: VarId -> VHDL VarId
newSym (Unique n) = Unique <$> return n
newSym (Base   n) = Base   <$> V.newSym n

freshVar :: forall exp a. (CompileExp exp, PredicateExp exp a) => VarId -> VHDL (exp a, V.Identifier)
freshVar prefix =
  do i <- varE <$> newSym prefix
     p <- dig  <$> compE i
     t <- compT (undefined :: exp a)
     V.variable p t Nothing
     return (i, p)
  where
    -- diggity dig!
    dig :: V.Expression -> V.Identifier
    dig (V.ENand (V.Relation (V.ShiftExpression (V.SimpleExpression _ (V.Term (V.FacPrim (V.PrimName (V.NSimple i)) _)_)_)_)_)_) = i

range :: Integral a => a -> V.Direction -> a -> V.Range
range a d b = V.range (V.point $ toInteger a) d (V.point $ toInteger b)

ident :: VarId -> V.Identifier
ident (Unique n) = V.Ident n
ident (Base   n) = V.Ident n

name  :: VarId -> V.Primary
name (Unique n) = V.name n
name (Base   n) = V.name n

--------------------------------------------------------------------------------
-- ** Signals.

instance CompileExp exp => Interp (SignalCMD exp) VHDL
  where
    interp = compileSignal

instance EvaluateExp exp => Interp (SignalCMD exp) IO
  where
    interp = runSignal

compileSignal :: forall exp a. CompileExp exp => SignalCMD exp VHDL a -> VHDL a
compileSignal (NewSignal base mode exp) =
  do v <- compEM exp
     t <- compTM exp
     i <- newSym base
     V.signal (ident i) mode t v
     return (SignalC i)
compileSignal (GetSignal (SignalC s)) =
  do (v, i) <- freshVar "s" :: VHDL (a, V.Identifier)
     e      <- compE v
     V.assignVariable i (lift $ name s)
     return v
compileSignal (SetSignal (SignalC s) exp) =
  do V.assignSignal (ident s) =<< compE exp
compileSignal (UnsafeFreezeSignal (SignalC s)) =
  do return $ varE s
compileSignal (PackSignal base (ArrayC n :: Array i Bool)) =
  do t <- compT (undefined :: exp i)
     i <- newSym base
     V.signal       (ident i) V.Out t Nothing
     V.assignSignal (ident i) (lift $ V.cast t $ lift $ name n)
     return (SignalC i)

runSignal :: forall exp prog a. EvaluateExp exp => SignalCMD exp prog a -> IO a
runSignal (NewSignal _ _ exp)         = fmap SignalE $ IR.newIORef $ evalEM exp
runSignal (GetSignal (SignalE r))     = fmap litE $ IR.readIORef r
runSignal (SetSignal (SignalE r) exp) = IR.writeIORef r $ evalE exp
runSignal (UnsafeFreezeSignal r)      = runSignal (GetSignal r)
runSignal (PackSignal _ array)        =
  error "runSignal-todo: run PackSignal"

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
     i <- newSym base
     V.variable (ident i) t v
     return (VariableC i)
compileVariable (GetVariable (VariableC var)) =
  do (v, i) <- freshVar "v" :: VHDL (a, V.Identifier)
     e <- compE v
     V.assignVariable i (lift $ V.PrimName $ V.NSimple $ ident var)
     return v
compileVariable (SetVariable (VariableC var) exp) =
  do V.assignVariable (ident var) =<< compE exp
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

compileArray
  :: forall exp a.
     ( CompileExp  exp
     , EvaluateExp exp
     , CompArrayIx exp)
  => ArrayCMD exp VHDL a
  -> VHDL a
compileArray (NewArray base (len :: exp i)) =
  do a <- compTA (range (evalE len) V.downto 0) len (undefined :: a)
     i <- newSym base
     V.signal (ident i) V.Out a Nothing
     return (ArrayC i)
compileArray (UnpackArray base (SignalC n :: Signal i)) =
  do t <- compT (undefined :: exp i)
     i <- newSym base
     let typ = V.std_logic_vector (V.width t)
     V.signal       (ident i) V.Out typ Nothing
     V.assignSignal (ident i) (lift $ V.cast typ $ lift $ name n)
     return (ArrayC i)

runArray :: forall exp prog a. EvaluateExp exp => ArrayCMD exp prog a -> IO a
runArray = error "runArray-todo"

--------------------------------------------------------------------------------
-- ** Virtual Arrays.

instance (CompileExp exp, EvaluateExp exp, CompArrayIx exp) => Interp (VArrayCMD exp) VHDL
  where
    interp = compileVArray

instance EvaluateExp exp => Interp (VArrayCMD exp) IO
  where
    interp = runVArray

compileVArray
  :: forall exp a.
     ( CompileExp exp
     , EvaluateExp exp
     , CompArrayIx exp)
  => VArrayCMD exp VHDL a
  -> VHDL a
compileVArray (NewVArray base len) =
  do a <- compTA (range (evalE len) V.downto 0) len (undefined :: a)
     i <- newSym base
     V.variable (ident i) a Nothing
     return (VArrayC i)
compileVArray (InitVArray base is) =
  do a <- compTA (range (length is) V.downto 0) (undefined :: exp i) (undefined :: a)
     i <- newSym base
     x <- sequence [compE (litE a :: exp b) | (a :: b) <- is]
     V.variable (ident i) a (Just $ lift $ V.aggregate x)
     return (VArrayC i)
compileVArray (GetVArray ix (VArrayC arr)) =
  do (v, i) <- freshVar "a" :: VHDL (a, V.Identifier)
     e <- compE ix
     V.assignVariable i (lift $ V.PrimName $ V.indexed (ident arr) e)
     return v
compileVArray (SetVArray i e (VArrayC arr)) =
  do i' <- compE i
     e' <- compE e
     V.assignArray (V.indexed (ident arr) i') e'
compileVArray (CopyVArray (VArrayC a) (VArrayC b) l) =
  do len <- compE l
     let slice = (lift (V.lit (0 :: Word8)), lift len)
         dest  = V.slice (ident a) slice
         src   = V.slice (ident b) slice
     V.assignArray src (lift $ V.PrimName dest)
compileVArray (UnsafeFreezeVArray (VArrayC a)) = return $ IArrayC a

runVArray :: forall exp prog a. EvaluateExp exp => VArrayCMD exp prog a -> IO a
runVArray (NewVArray _ len)           = fmap VArrayE . IR.newIORef =<< IA.newArray_ (0, evalE len)
runVArray (InitVArray _ is)           = fmap VArrayE . IR.newIORef =<< IA.newListArray (0, fromIntegral $ length is) is
runVArray (GetVArray i (VArrayE a))   = do r <- IR.readIORef a; fmap litE $ IA.readArray r (evalE i)
runVArray (SetVArray i e (VArrayE a)) = do r <- IR.readIORef a; IA.writeArray r (evalE i) (evalE e)
runVArray (UnsafeFreezeVArray (VArrayE a))       = fmap IArrayE . freeze =<< IR.readIORef a
runVArray (CopyVArray (VArrayE a) (VArrayE b) l) =
  do arr <- IR.readIORef a
     brr <- IR.readIORef b
     sequence_
       [ IA.readArray brr i >>= IA.writeArray arr i
       | i <- genericTake (evalE l) [0..]
       ]

--------------------------------------------------------------------------------
-- ** Loops.

instance (CompileExp exp, EvaluateExp exp) => Interp (LoopCMD exp) VHDL
  where
    interp = compileLoop

instance EvaluateExp exp => Interp (LoopCMD exp) IO
  where
    interp = runLoop

compileLoop :: forall exp a. (CompileExp exp, EvaluateExp exp) => LoopCMD exp VHDL a -> VHDL a
compileLoop (For r step) =
  do (v, i) <- freshVar "l"
     loop   <- V.inFor i (range 0 V.to (evalE r)) (step v)
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
compileComponent (StructComponent n sig) =
  do comp <- newSym n
     V.component $
       V.entity (ident comp) $ declare sig >>=
       V.architecture (ident comp) (V.Ident "IMP")
     return (Just comp)
  where
    declare :: Sig exp VHDL b ->  VHDL (VHDL ())
    declare (Unit m)     = return m
    declare (Lam  n m f) =
      do t <- compFT f
         i <- newSym n
         V.signal (ident i) m t Nothing
         declare (f (SignalC i))
compileComponent (PortMap (Component (Just n) sig) as) =
  do l  <- newSym (Base "l")
     vs <- V.declareComponent (ident n) (apply sig as)
     V.portMap (ident l) (ident n) (fmap ident $ reverse vs)
  where
     -- todo: associate 'n' with 'i'
    apply :: Sig exp VHDL b -> Arg b -> VHDL [VarId]
    apply (Unit _)     (Nill)               = return []
    apply (Lam  n m f) (s@(SignalC i) :> v) =
      (i :) <$> apply (f s) v

runComponent :: forall exp a. EvaluateExp exp => ComponentCMD exp IO a -> IO a
runComponent (StructComponent _ _)            = return Nothing
runComponent (PortMap (Component _ sig) args) =
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
