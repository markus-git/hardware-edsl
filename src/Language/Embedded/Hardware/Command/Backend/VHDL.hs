{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ConstraintKinds       #-}

module Language.Embedded.Hardware.Command.Backend.VHDL (CompileType(..)) where

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

--------------------------------------------------------------------------------

class CompileType ct
  where
    compileType :: ct a => proxy1 ct -> proxy2 a -> VHDL V.Type
    compileLit  :: ct a => proxy1 ct ->        a -> VHDL V.Expression

instance CompileType HType
  where
    compileType _ = compT
    compileLit  _ = literal

proxyM :: Maybe (exp a) -> Proxy a
proxyM _ = Proxy

proxyF :: (exp a -> b) -> Proxy a
proxyF _ = Proxy

compTM :: forall proxy ct exp a. (CompileType ct, ct a) => proxy ct -> Maybe (exp a) -> VHDL V.Type
compTM _ _ = compileType (Proxy::Proxy ct) (Proxy::Proxy a)

compTF :: forall proxy ct exp a b. (CompileType ct, ct a) => proxy ct -> (exp a -> b) -> VHDL V.Type
compTF _ _ = compileType (Proxy::Proxy ct) (Proxy::Proxy a)

compTA :: forall proxy ct array i a. (CompileType ct, ct a) => proxy ct -> V.Range -> array i a -> VHDL V.Type
compTA _ range _ =
  do i <- newSym (Base "type")
     t <- compileType (Proxy::Proxy ct) (Proxy::Proxy a)
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

freshVar :: forall proxy ct exp a. (CompileType ct, ct a) => proxy ct -> Name -> VHDL (Val a)
freshVar _ prefix =
  do i <- newSym prefix
     t <- compileType (Proxy::Proxy ct) (Proxy::Proxy a)
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

fromIdent :: ToIdent a => a -> V.Identifier
fromIdent a = let (Ident i) = toIdent a in ident i

simpleName :: ToIdent a => a -> V.Name
simpleName = V.NSimple . fromIdent

--------------------------------------------------------------------------------
-- ** Signals.

instance (CompileExp exp, CompileType ct) => Interp SignalCMD VHDL (Param2 exp ct)
  where
    interp = compileSignal

instance InterpBi SignalCMD IO (Param1 pred)
  where
    interpBi = runSignal

compileSignal :: forall exp ct a. (CompileExp exp, CompileType ct) => SignalCMD (Param3 VHDL exp ct) a -> VHDL a
compileSignal (NewSignal base mode exp) =
  do v <- compEM exp
     t <- compTM (Proxy::Proxy ct) exp
     i <- newSym base
     V.signal (ident i) mode t v
     return (SignalC i)
compileSignal (GetSignal (SignalC s)) =
  do i <- freshVar (Proxy::Proxy ct) (Base "s")
     V.assignVariable (simpleName i) (lift $ name s)
     return i
compileSignal (SetSignal (SignalC s) exp) =
  do V.assignSignal (V.NSimple $ ident s) =<< compE exp
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

instance (CompileExp exp, CompileType ct) => Interp VariableCMD VHDL (Param2 exp ct)
  where
    interp = compileVariable

instance InterpBi VariableCMD IO (Param1 pred)
  where
    interpBi = runVariable

compileVariable :: forall ct exp a. (CompileExp exp, CompileType ct) => VariableCMD (Param3 VHDL exp ct) a -> VHDL a
compileVariable (NewVariable base exp) =
  do v <- compEM exp
     t <- compTM (Proxy::Proxy ct) exp
     i <- newSym base
     V.variable (ident i) t v
     return (VariableC i)
compileVariable (GetVariable (VariableC var)) =
  do i <- freshVar (Proxy::Proxy ct) (Base "v")
     V.assignVariable (simpleName i) (lift $ V.PrimName $ V.NSimple $ ident var)
     return i
compileVariable (SetVariable (VariableC var) exp) =
  do V.assignVariable (V.NSimple $ ident var) =<< compE exp
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

instance (CompileExp exp, CompileType ct) => Interp ConstantCMD VHDL (Param2 exp ct)
  where
    interp = compileConstant

instance InterpBi ConstantCMD IO (Param1 pred)
  where
    interpBi = runConstant

compileConstant :: forall ct exp a. (CompileExp exp, CompileType ct) => ConstantCMD (Param3 VHDL exp ct) a -> VHDL a
compileConstant (NewConstant base (exp :: exp c)) =
  do v <- compE  exp
     t <- compileType (Proxy::Proxy ct) (Proxy::Proxy c)
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

instance (CompileExp exp, CompileType ct) => Interp ArrayCMD VHDL (Param2 exp ct)
  where
    interp = compileArray

instance InterpBi ArrayCMD IO (Param1 pred)
  where
    interpBi = runArray

compileArray :: forall ct exp a. (CompileExp exp, CompileType ct) => ArrayCMD (Param3 VHDL exp ct) a -> VHDL a
compileArray (GetArray ix (SignalC s)) =
  do i <- freshVar (Proxy::Proxy ct) (Base "a")
     e <- compE ix
     V.assignVariable (simpleName i) (lift $ V.PrimName $ V.indexed (ident s) e)
     return i
compileArray (GetRangeS to (l, u) s) =
  do i   <- newSym (Base "v")
     to' <- compE to
     l'  <- compE l
     u'  <- compE u
     V.variable (ident i) (vector $ lift to') Nothing
     V.assignVariable (V.NSimple $ ident i) (lift $ V.name $ V.slice (fromIdent s) (lift l', lift u'))
     return (ValC i)
   where
     vector exp = V.SubtypeIndication Nothing
       (V.TMType (V.NSlice (V.SliceName
         (V.PName (V.NSimple (V.Ident "std_logic_vector")))
           (V.DRRange (V.RSimple (exp) (V.DownTo)
             (V.SimpleExpression Nothing
               (V.Term (V.FacPrim (V.lit "0") (Nothing)) []) []))))))
       (Nothing)
compileArray (SetRangeS (t1, t2) a (f1, f2) b) =
  do t1' <- compE t1
     t2' <- compE t2
     f1' <- compE f1
     f2' <- compE f2
     let v1 = V.slice (fromIdent a) (lift t1', lift t2')
         v2 = V.name $ V.slice (fromIdent b) (lift f1', lift f2')
     V.assignSignal v1 (lift v2)

runArray :: ArrayCMD (Param3 IO IO pred) a -> IO a
runArray = error "vhdl-todo: evaluate Array"

--------------------------------------------------------------------------------
-- ** Virtual Arrays.

instance (CompileExp exp, CompileType ct) => Interp VArrayCMD VHDL (Param2 exp ct)
  where
    interp = compileVArray

instance InterpBi VArrayCMD IO (Param1 pred)
  where
    interpBi = runVArray

compileVArray :: forall ct exp a. (CompileExp exp, CompileType ct) => VArrayCMD (Param3 VHDL exp ct) a -> VHDL a
compileVArray (NewVArray base len) =
  do l <- compE len
     let r = V.range (lift l) V.downto (V.point (0 :: Int))
     t <- compTA (Proxy::Proxy ct) r (undefined :: a)
     i <- newSym base
     V.variable (ident i) t Nothing
     return (VArrayC i)
compileVArray (InitVArray base is) =
  do let r = V.range (V.point (length is)) V.downto (V.point (0 :: Int))
     t <- compTA (Proxy::Proxy ct) r (undefined :: a)
     i <- newSym base
     x <- mapM (compileLit (Proxy::Proxy ct)) is
     let v = V.aggregate $ V.aggregated x
     V.variable (ident i) t (Just $ lift v)
     return (VArrayC i)
compileVArray (GetVArray ix (VArrayC arr)) =
  do i <- freshVar (Proxy::Proxy ct) (Base "a")
     e <- compE ix
     V.assignVariable (simpleName i) (lift $ V.PrimName $ V.indexed (ident arr) e)
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
     ref  <- IR.newIORef arr
     return (VArrayE ref)
runVArray (InitVArray _ is) =
  do arr  <- IA.newListArray (0, fromIntegral $ length is - 1) is
     ref  <- IR.newIORef arr
     return (VArrayE ref)
runVArray (GetVArray i (VArrayE ref)) =
  do arr    <- IR.readIORef ref
     (l, h) <- IA.getBounds arr
     ix  <- i
     if (ix < l || ix > h)
        then error "getArr out of bounds"
        else do v <- IA.readArray arr ix
                return (ValE v)
runVArray (SetVArray i e (VArrayE ref)) =
  do arr    <- IR.readIORef ref
     (l, h) <- IA.getBounds arr
     ix <- i
     e' <- e
     if (ix < l || ix > h)
        then error "setArr out of bounds"
        else IA.writeArray arr (fromIntegral ix) e'
runVArray (CopyVArray (VArrayE ra) (VArrayE rb) l) =
  do l'      <- l
     arr     <- IR.readIORef ra
     brr     <- IR.readIORef rb
     (0, ha) <- IA.getBounds arr
     (0, hb) <- IA.getBounds brr
     if (l' > hb + 1 || l' > ha + 1)
        then error "copyArr out of bounts"
        else sequence_ [ IA.readArray arr i >>= IA.writeArray brr i
                       | i <- genericTake l' [0..] ]
runVArray (UnsafeFreezeVArray (VArrayE ref)) = IR.readIORef ref >>= IA.freeze >>= return . IArrayE
runVArray (UnsafeThawVArray   (IArrayE arr)) = IA.thaw arr >>= IR.newIORef >>= return . VArrayE

--------------------------------------------------------------------------------
-- ** Loops.

instance (CompileExp exp, CompileType ct) => Interp LoopCMD VHDL (Param2 exp ct)
  where
    interp = compileLoop

instance InterpBi LoopCMD IO (Param1 pred)
  where
    interpBi = runLoop

compileLoop :: forall ct exp a. (CompileExp exp, CompileType ct) => LoopCMD (Param3 VHDL exp ct) a -> VHDL a
compileLoop (For h step) =
  do --i    <- freshVar (Proxy::Proxy ct) (Base "l")
     i    <- newSym (Base "l")
     h'   <- compE h
     let r = V.range (V.point (0 :: Int)) V.to (lift h')
     loop <- V.inFor (ident i) r (step (ValC i))
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

instance (CompileExp exp, CompileType ct) => Interp ConditionalCMD VHDL (Param2 exp ct)
  where
    interp = compileConditional

instance InterpBi ConditionalCMD IO (Param1 pred)
  where
    interpBi = runConditional

compileConditional :: forall ct exp a. (CompileExp exp, CompileType ct) => ConditionalCMD (Param3 VHDL exp ct) a -> VHDL a
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
    compC :: ct b => When b VHDL -> VHDL (V.Choices, VHDL ())
    compC (When (Is e) p)   = do
      e' <- compileLit (Proxy::Proxy ct) e
      return $ (V.Choices [V.ChoiceSimple $ lift e'], p)
    compC (When (To l h) p) = do
      l' <- compileLit (Proxy::Proxy ct) l
      h' <- compileLit (Proxy::Proxy ct) h
      return $ (V.Choices [V.ChoiceRange (V.DRRange (V.range (lift l') V.to (lift h')))], p)
compileConditional (Null) = V.null
-- *** ...
compileConditional (WhenRising (SignalC s) p) =
  do let c = V.function (V.Ident "rising_edge") [lift $ V.name $ V.NSimple $ V.Ident s]
     s <- V.inConditional (lift c, p) [] (return ())
     V.addSequential $ V.SIf s

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
-- *** ...
runConditional (WhenRising s p) =
  do error "vhdl: cannot evaluate 'rising_edge'"

--------------------------------------------------------------------------------
-- ** Components.

instance (CompileExp exp, CompileType ct) => Interp ComponentCMD VHDL (Param2 exp ct)
  where
    interp = compileComponent

instance InterpBi ComponentCMD IO (Param1 pred)
  where
    interpBi = runComponent

compileComponent :: forall ct exp a. (CompileExp exp, CompileType ct) => ComponentCMD (Param3 VHDL exp ct) a -> VHDL a
compileComponent (StructComponent base sig) =
  do comp <- newSym base
     V.component $
       do p <- V.entity (ident comp) (traverseS sig)
          V.architecture (ident comp) (V.Ident "imp") p
     return $ case base of
       Base  _ -> Base comp
       Exact _ -> Exact comp
  where
    traverseS :: Signature (Param3 VHDL exp ct) b ->  VHDL (VHDL ())
    traverseS (Ret prog) = return prog
    traverseS (Lam n m f) = 
      do t <- compTF (Proxy::Proxy ct) f
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
    apply :: Signature (Param3 VHDL exp ct) b -> Arg b -> VHDL [V.InterfaceDeclaration]
    apply (Ret _)     (Nill)                      = return []
    apply (Lam n m f) (ArgSignal s@(SignalC i) v) =
      do t  <- compTF (Proxy::Proxy ct) f
         is <- apply (f s) v
         let i = V.InterfaceSignalDeclaration [ident' n] (Just m) t False Nothing
         return (i : is)

    assoc :: Signature (Param3 VHDL exp ct) b -> Arg b -> [(V.Identifier, V.Identifier)]
    assoc (Ret _)     (Nill)                      = []
    assoc (Lam n _ f) (ArgSignal s@(SignalC i) v) = (ident' n, ident i) : assoc (f s) v

runComponent :: ComponentCMD (Param3 IO IO pred) a -> IO a
runComponent (StructComponent _ _)            = return None
runComponent (PortMap (Component _ sig) args) =
  error "hardware-edsl-todo: figure out how to simulate processes in Haskell."

--------------------------------------------------------------------------------
-- ** Structural.

instance (CompileExp exp, CompileType ct) => Interp StructuralCMD VHDL (Param2 exp ct)
  where
    interp = compileStructural

instance InterpBi StructuralCMD IO (Param1 pred)
  where
    interpBi = runStructural

compileStructural :: forall ct exp a. (CompileExp exp, CompileType ct) => StructuralCMD (Param3 VHDL exp ct) a -> VHDL a
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
-- ** VHDL.
{-
data SignalArg pred where
  SigArg :: pred a => Signal a -> SignalArg pred

instance Argument SignalArg pred where
  mkArg   (SigArg (SignalC s)) = lift $ V.name $ V.NSimple $ V.Ident s
  mkParam (SigArg (SignalC s)) = V.Ident s
-}
--------------------------------------------------------------------------------
{-     
instance (CompileExp exp, CompileType ct) => Interp VHDL_CMD VHDL (Param2 exp ct)
  where
    interp = compileVHDL

instance InterpBi VHDL_CMD IO (Param1 pred)
  where
    interpBi = runVHDL

compileVHDL :: forall ct exp a. (CompileExp exp, CompileType ct) => VHDL_CMD (Param3 VHDL exp ct) a -> VHDL a
compileVHDL (CallFun n as) = undefined

runVHDL :: VHDL_CMD (Param3 IO IO pred) a -> IO a
runVHDL = undefined
-}
