{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE KindSignatures        #-}
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
import Language.Embedded.Hardware.Expression.Represent.Bit (Bits)
import Language.Embedded.Hardware.Command.CMD

import Language.Embedded.VHDL (VHDL)
import qualified Language.VHDL          as V
import qualified Language.Embedded.VHDL as V

import Data.Array.IO (freeze)
import Data.List     (genericTake)
import Data.Proxy
import Data.Word     (Word8)
import qualified Data.IORef    as IR
import qualified Data.Array.IO as IA
--import GHC.TypeLits

--------------------------------------------------------------------------------
-- * Translation of hardware commands into VHDL.
--------------------------------------------------------------------------------

evalEM :: forall exp a. EvaluateExp exp => Maybe (exp a) -> a
evalEM e = maybe (error "empty value") (id) $ fmap evalE e

compEM :: forall exp a. CompileExp exp => Maybe (exp a) -> VHDL (Maybe V.Expression)
compEM e = maybe (return Nothing) (>>= return . Just) $ fmap compE e

compTM :: forall exp a. HType a => Maybe (exp a) -> VHDL V.Type
compTM _ = compT (Proxy :: Proxy a)

compTF :: forall exp a b. HType a => (exp a -> b) -> VHDL V.Type
compTF _ = compT (Proxy :: Proxy a)

compTA :: forall array i a. HType a => V.Range -> array i a -> VHDL V.Type
compTA range _ =
  do i <- newSym (Base "type")
     t <- compT (Proxy :: Proxy a)
     let array = V.constrainedArray (ident i) t range
     V.addType array
     return (typed array)
  where
    typed :: V.TypeDeclaration -> V.SubtypeIndication
    typed (V.TDFull    (V.FullTypeDeclaration i _))     = named i
    typed (V.TDPartial (V.IncompleteTypeDeclaration i)) = named i

    named :: V.Identifier -> V.SubtypeIndication
    named i = V.SubtypeIndication Nothing (V.TMType (V.NSimple i)) Nothing

--------------------------------------------------------------------------------

newSym :: Name -> VHDL String
newSym (Base  n) = V.newSym n
newSym (Exact n) = return   n

freshVar :: forall exp a. HType a => Name -> VHDL (Val a)
freshVar prefix =
  do i <- newSym prefix
     t <- compT (Proxy :: Proxy a)
     V.variable (ident i) t Nothing
     return (ValC i)

--------------------------------------------------------------------------------

ident :: String -> V.Identifier
ident s = V.Ident s

ident' :: Name -> V.Identifier
ident' (Base  n) = ident n
ident' (Exact n) = ident n

range :: Integral a => a -> V.Direction -> a -> V.Range
range a d b = V.range (V.point $ toInteger a) d (V.point $ toInteger b)

name :: String -> V.Primary
name = V.PrimName . V.NSimple . ident

--------------------------------------------------------------------------------

fromIdent :: ToIdent a => a -> V.Identifier
fromIdent a = let (Ident i) = toIdent a in ident i

--------------------------------------------------------------------------------
-- ** Signals.

instance CompileExp exp => Interp SignalCMD VHDL (Param2 exp HType)
  where
    interp = compileSignal

instance InterpBi SignalCMD IO (Param1 pred)
  where
    interpBi = runSignal

compileSignal :: CompileExp exp => SignalCMD (Param3 VHDL exp HType) a -> VHDL a
compileSignal (NewSignal base mode exp) =
  do v <- compEM exp
     t <- compTM exp
     i <- newSym base
     V.signal (ident i) mode t v
     return (SignalC i)
compileSignal (GetSignal (SignalC s)) =
  do i <- freshVar (Base "s")
     V.assignVariable (fromIdent i) (lift $ name s)
     return i
compileSignal (SetSignal (SignalC s) exp) =
  do V.assignSignal (ident s) =<< compE exp
compileSignal (UnsafeFreezeSignal (SignalC s)) =
  do return $ ValC s

runSignal :: SignalCMD (Param3 IO IO pred) a -> IO a
runSignal (NewSignal _ _ Nothing)     = fmap SignalE $ IR.newIORef (error "uninitialized signal")
runSignal (NewSignal _ _ (Just a))    = fmap SignalE . IR.newIORef =<< a
runSignal (GetSignal (SignalE r))     = fmap ValE $ IR.readIORef r
runSignal (SetSignal (SignalE r) exp) = IR.writeIORef r =<< exp
runSignal x@(UnsafeFreezeSignal r)    = runSignal (GetSignal r `asTypeOf` x)

--------------------------------------------------------------------------------
-- ** Variables.

instance CompileExp exp => Interp VariableCMD VHDL (Param2 exp HType)
  where
    interp = compileVariable

instance InterpBi VariableCMD IO (Param1 pred)
  where
    interpBi = runVariable

compileVariable :: forall exp a. CompileExp exp => VariableCMD (Param3 VHDL exp HType) a -> VHDL a
compileVariable (NewVariable base exp) =
  do v <- compEM exp
     t <- compTM exp
     i <- newSym base
     V.variable (ident i) t v
     return (VariableC i)
compileVariable (GetVariable (VariableC var)) =
  do i <- freshVar (Base "v")
     V.assignVariable (fromIdent i) (lift $ V.PrimName $ V.NSimple $ ident var)
     return i
compileVariable (SetVariable (VariableC var) exp) =
  do V.assignVariable (ident var) =<< compE exp
compileVariable (UnsafeFreezeVariable (VariableC v)) =
  do return $ ValC v

runVariable :: VariableCMD (Param3 IO IO pred) a -> IO a
runVariable (NewVariable _ Nothing)         = fmap VariableE $ IR.newIORef (error "uninitialized variable")
runVariable (NewVariable _ (Just a))        = fmap VariableE . IR.newIORef =<< a
runVariable (GetVariable (VariableE v))     = fmap ValE $ IR.readIORef v
runVariable (SetVariable (VariableE v) exp) = IR.writeIORef v =<< exp
runVariable x@(UnsafeFreezeVariable v)      = runVariable (GetVariable v `asTypeOf` x)

--------------------------------------------------------------------------------
-- ** Constants.

instance CompileExp exp => Interp ConstantCMD VHDL (Param2 exp HType)
  where
    interp = compileConstant

instance InterpBi ConstantCMD IO (Param1 pred)
  where
    interpBi = runConstant

compileConstant :: forall exp a. CompileExp exp => ConstantCMD (Param3 VHDL exp HType) a -> VHDL a
compileConstant (NewConstant base exp) =
  do v <- compE  exp
     t <- compT  exp
     i <- newSym base
     V.constant (ident i) t v
     return (ConstantC i)
compileConstant (GetConstant (ConstantC c)) =
  do return $ ValC c

runConstant :: ConstantCMD (Param3 IO IO pred) a -> IO a
runConstant (NewConstant _ exp)           = return . ConstantE =<< exp
runConstant (GetConstant (ConstantE exp)) = return $ ValE exp

--------------------------------------------------------------------------------
-- ** Arrays.

-- ....

--------------------------------------------------------------------------------
-- ** Virtual Arrays.

instance CompileExp exp => Interp VArrayCMD VHDL (Param2 exp HType)
  where
    interp = compileVArray

instance InterpBi VArrayCMD IO (Param1 pred)
  where
    interpBi = runVArray

compileVArray :: forall exp a. CompileExp exp => VArrayCMD (Param3 VHDL exp HType) a -> VHDL a
compileVArray (NewVArray base len) =
  do l <- compE len
     let r = V.range (lift l) V.downto (V.point (0 :: Int))
     t <- compTA r (undefined :: a)
     i <- newSym base
     V.variable (ident i) t Nothing
     return (VArrayC i)
compileVArray (InitVArray base is) =
  do let r = V.range (V.point (length is)) V.downto (V.point (0 :: Int))
     t <- compTA r (undefined :: a)
     i <- newSym base
     x <- mapM literal is
     let v = V.aggregate $ V.aggregated x
     V.variable (ident i) t (Just $ lift v)
     return (VArrayC i)
compileVArray (GetVArray ix (VArrayC arr)) =
  do i <- freshVar (Base "a")
     e <- compE ix
     V.assignVariable (fromIdent i) (lift $ V.PrimName $ V.indexed (ident arr) e)
     return i
compileVArray (SetVArray i e (VArrayC arr)) =
  do i' <- compE i
     e' <- compE e
     V.assignArray (V.indexed (ident arr) i') e'
compileVArray (CopyVArray (VArrayC a) (VArrayC b) l) =
  do len <- compE l
     let slice = (lift (V.lit "0"), lift len)
         dest  = V.slice (ident a) slice
         src   = V.slice (ident b) slice
     V.assignArray src (lift $ V.PrimName dest)
compileVArray (UnsafeFreezeVArray (VArrayC arr)) = return $ IArrayC arr
compileVArray (UnsafeThawVArray (IArrayC arr)) = return $ VArrayC arr

runVArray :: VArrayCMD (Param3 IO IO pred) a -> IO a
runVArray (NewVArray _ len) =
  do len' <- len
     arr  <- IA.newArray_ (0, len')
     return (VArrayE arr)
runVArray (InitVArray _ is) =
  do arr  <- IA.newListArray (0, fromIntegral $ length is - 1) is
     return (VArrayE arr)
runVArray (GetVArray i (VArrayE arr)) =
  do (l, h) <- IA.getBounds arr
     ix  <- i
     if (ix < l || ix > h)
        then error "getArr out of bounds"
        else do v <- IA.readArray arr ix
                return (ValE v)
runVArray (SetVArray i e (VArrayE arr)) =
  do (l, h) <- IA.getBounds arr
     ix <- i
     e' <- e
     if (ix < l || ix > h)
        then error "setArr out of bounds"
        else IA.writeArray arr (fromIntegral ix) e'
runVArray (CopyVArray (VArrayE arr) (VArrayE brr) l) =
  do l'      <- l
     (0, ha) <- IA.getBounds arr
     (0, hb) <- IA.getBounds brr
     if (l' > hb + 1 || l' > ha + 1)
        then error "copyArr out of bounts"
        else sequence_ [ IA.readArray arr i >>= IA.writeArray brr i
                       | i <- genericTake l' [0..] ]
runVArray (UnsafeFreezeVArray (VArrayE arr)) = fmap IArrayE $ IA.freeze arr
runVArray (UnsafeThawVArray (IArrayE arr)) = fmap VArrayE $ IA.thaw arr

--------------------------------------------------------------------------------
-- ** Loops.

instance CompileExp exp => Interp LoopCMD VHDL (Param2 exp HType)
  where
    interp = compileLoop

instance InterpBi LoopCMD IO (Param1 pred)
  where
    interpBi = runLoop

compileLoop :: forall exp a. CompileExp exp => LoopCMD (Param3 VHDL exp HType) a -> VHDL a
compileLoop (For h step) =  -- (range 0 V.to (evalE r))
  do h'   <- compE h
     let r = V.range (V.point (0 :: Int)) V.to (lift h')
     i    <- freshVar (Base "l")
     loop <- V.inFor undefined r (step i)
     V.addSequential $ V.SLoop $ loop
compileLoop (While cont step) =
  do l    <- V.newLabel
     loop <- V.inWhile l Nothing $
       do b    <- cont
          exit <- compE b
          V.exit l exit
          step
     V.addSequential $ V.SLoop $ loop

runLoop :: LoopCMD (Param3 IO IO pred) a -> IO a
runLoop (For r step) = r >>= loop
  where
    loop i | i > 0     = step (ValE i) >> loop (i - 1)
           | otherwise = return ()
runLoop (While b step) = loop
  where
    loop = do e <- join b
              when e (step >> loop)

--------------------------------------------------------------------------------
-- ** Conditional.

instance CompileExp exp => Interp ConditionalCMD VHDL (Param2 exp HType)
  where
    interp = compileConditional

instance InterpBi ConditionalCMD IO (Param1 pred)
  where
    interpBi = runConditional

compileConditional :: forall exp a. CompileExp exp => ConditionalCMD (Param3 VHDL exp HType) a -> VHDL a
compileConditional (If (a, b) cs em) =
  do let (es, ds) = unzip cs
         el       = maybe (return ()) id em
     ae  <- compE a
     ese <- mapM compE es
     s   <- V.inConditional (ae, b) (zip ese ds) el
     V.addSequential $ V.SIf s
compileConditional (Case e cs d) =
  do let el = maybe (return ()) id d
     ae  <- compE e
     ce  <- mapM compC cs
     s   <- V.inCase ae ce el
     V.addSequential $ V.SCase s
  where
    compC :: HType b => When b VHDL -> VHDL (V.Choices, VHDL ())
    compC (When (Is e) p)   = do
      e' <- literal e
      return $ (V.Choices [V.ChoiceSimple $ lift e'], p)
    compC (When (To l h) p) = do
      l' <- literal l
      h' <- literal h
      return $ (V.Choices [V.ChoiceRange (V.DRRange (V.range (lift l') V.to (lift h')))], p)
compileConditional (Null) = V.null

runConditional :: ConditionalCMD (Param3 IO IO pred) a -> IO a
runConditional (If (a, b) cs em) =
  do c <- a
     if c then b else loop cs
  where
    loop [] = maybe (return ()) id em
    loop ((c, p):xs) = do
      b <- c
      if b then p else (loop xs)
runConditional (Case e cs d) =
  do c <- e
     loop c cs
  where
    loop v [] = maybe (return ()) id d
    loop v ((When (Is u)   p):cs) = if v == u         then p else loop v cs
    loop v ((When (To l h) p):cs) = if v > l && v < h then p else loop v cs
runConditional (Null) = return ()

--------------------------------------------------------------------------------
-- ** Components.

instance CompileExp exp => Interp ComponentCMD VHDL (Param2 exp HType)
  where
    interp = compileComponent

instance InterpBi ComponentCMD IO (Param1 pred)
  where
    interpBi = runComponent

compileComponent :: forall exp a. CompileExp exp => ComponentCMD (Param3 VHDL exp HType) a -> VHDL a
compileComponent (StructComponent base sig) =
  do comp <- newSym base
     V.component $
       do p <- V.entity (ident comp) (traverseS sig)
          V.architecture (ident comp) (V.Ident "imp") p
     return $ case base of
       Base  _ -> Base comp
       Exact _ -> Exact comp
  where
    traverseS :: Signature (Param3 VHDL exp HType) b ->  VHDL (VHDL ())
    traverseS (Ret prog) = return prog
    traverseS (Lam n m f) = 
      do t <- compTF f
         i <- newSym n
         V.signal (ident i) m t Nothing
         traverseS (f (SignalC i))

compileComponent (PortMap (Component base sig) as) =
  do i  <- newSym base
     l  <- V.newLabel
     vs <- apply sig as
     V.declareComponent  (ident i) vs
     V.portMap l (ident i) (assoc sig as)
  where
    apply :: Signature (Param3 VHDL exp HType) b -> Arg b -> VHDL [V.InterfaceDeclaration]
    apply (Ret _)     (Nill)                      = return []
    apply (Lam n m f) (ArgSignal s@(SignalC i) v) =
      do t  <- compTF f
         is <- apply (f s) v
         let i = V.InterfaceSignalDeclaration [ident' n] (Just m) t False Nothing
         return (i : is)

    assoc :: Signature (Param3 VHDL exp HType) b -> Arg b -> [(V.Identifier, V.Identifier)]
    assoc (Ret _)     (Nill)                      = []
    assoc (Lam n _ f) (ArgSignal s@(SignalC i) v) = (ident' n, ident i) : assoc (f s) v

runComponent :: ComponentCMD (Param3 IO IO pred) a -> IO a
runComponent (StructComponent _ _)            = return None
runComponent (PortMap (Component _ sig) args) =
  error "hardware-edsl-todo: figure out how to simulate processes in Haskell."

--------------------------------------------------------------------------------
-- ** Structural.

instance CompileExp exp => Interp StructuralCMD VHDL (Param2 exp HType)
  where
    interp = compileStructural

instance InterpBi StructuralCMD IO (Param1 pred)
  where
    interpBi = runStructural

compileStructural :: forall exp a. CompileExp exp => StructuralCMD (Param3 VHDL exp HType) a -> VHDL a
compileStructural (StructEntity e prog)  =
  V.entity (ident' e) prog
compileStructural (StructArchitecture e a prog) =
  V.architecture (ident' e) (ident' a) prog
compileStructural (StructProcess xs prog) =
  do label  <- V.newLabel
     (a, c) <- V.inProcess label (fmap (\(Ident i) -> ident i) xs) prog
     V.addConcurrent (V.ConProcess c)
     return a

runStructural :: StructuralCMD (Param3 IO IO pred) a -> IO a
runStructural (StructEntity _ prog)         = prog
runStructural (StructArchitecture _ _ prog) = prog
runStructural (StructProcess xs prog)       =
  do error "hardware-edsl-todo: figure out how to simulate processes in Haskell."

--------------------------------------------------------------------------------
