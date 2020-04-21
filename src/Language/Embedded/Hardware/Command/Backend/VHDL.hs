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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Embedded.Hardware.Command.Backend.VHDL
  ( SignalEnv(..)
  , emptyEnv
  , VHDLGenT
  , VHDLGen
  , runVHDLGenT
  , runVHDLGen
  , CompileType(..)
  )
  where

import Control.Monad.Operational.Higher

import Language.Embedded.Hardware.Interface
import Language.Embedded.Hardware.Expression.Hoist
import Language.Embedded.Hardware.Expression.Represent
import Language.Embedded.Hardware.Expression.Represent.Bit (Bits, ni)
import Language.Embedded.Hardware.Command.CMD

import Language.Embedded.VHDL (VHDLT, VHDL)
import qualified Language.VHDL          as V
import qualified Language.Embedded.VHDL as V
--
import qualified Language.Embedded.VHDL.Monad as V
--


import Control.Monad.Identity (Identity)
import Control.Monad.Reader   (ReaderT, MonadReader)
import Control.Monad.State    (StateT,  MonadState, MonadIO)
import qualified Control.Monad.Identity as CMI
import qualified Control.Monad.Reader   as CMR
import qualified Control.Monad.State    as CMS

import Data.Array.IO (freeze)
import Data.List     (genericTake)
import Data.Proxy
import Data.Word     (Word8)
import qualified Data.IORef    as IR
import qualified Data.Array.IO as IA

import GHC.TypeLits (KnownNat)

--------------------------------------------------------------------------------
-- * VHDL code generation.
--------------------------------------------------------------------------------

data SignalEnv = SignalEnv
  { _clock :: Signal Bool
  , _reset :: Signal Bool
  }

-- todo: 'wrapMain' will add these ports.
emptyEnv :: SignalEnv
emptyEnv  = SignalEnv {
    _clock = SignalC "clk"
  , _reset = SignalC "rst" }

type MonadGen m = (Functor m, Applicative m, Monad m, MonadReader SignalEnv m)

newtype VHDLGenT m a = VHDLGenT { unVHDLGenT :: ReaderT SignalEnv (VHDLT m) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader SignalEnv
           , MonadState  V.VHDLEnv
           )

type VHDLGen = VHDLGenT Identity

runVHDLGenT :: Monad m => VHDLGenT m a -> SignalEnv -> VHDLT m a
runVHDLGenT m = CMR.runReaderT (unVHDLGenT m)

runVHDLGen :: VHDLGen a -> SignalEnv -> VHDL a
runVHDLGen = runVHDLGenT

--------------------------------------------------------------------------------

readClock :: MonadGen m => m (V.Identifier)
readClock = CMR.asks (\e -> let (SignalC clk) = _clock e in ident' clk)

readReset :: MonadGen m => m (V.Identifier)
readReset = CMR.asks (\e -> let (SignalC clk) = _reset e in ident' clk)

localClock :: MonadGen m => String -> m () -> m ()
localClock clk = CMR.local $ \e -> e { _clock = SignalC clk }

localReset :: MonadGen m => String -> m () -> m ()
localReset rst = CMR.local $ \e -> e { _reset = SignalC rst }

--------------------------------------------------------------------------------
-- ** Translation of hardware types and expressions into VHDL.
--------------------------------------------------------------------------------

class CompileType ct
  where
    compileType :: ct a => proxy1 ct -> proxy2 a -> VHDL V.Type
    compileLit  :: ct a => proxy1 ct ->        a -> VHDL V.Expression
    compileBits :: ct a => proxy1 ct ->        a -> VHDL V.Expression

instance CompileType PrimType
  where
    compileType _ x = declareType $ proxyE x
    compileLit  _   = return . literal . primTypeVal
    compileBits _   = return . literal . primTypeBits

--------------------------------------------------------------------------------

compTC :: forall proxy ct exp a . (CompileType ct, ct a) => Proxy ct -> Proxy a -> VHDLGen V.Type
compTC ct p = VHDLGenT $ CMS.lift $ compileType ct p

compTM :: forall proxy ct exp a . (CompileType ct, ct a) => Proxy ct -> Maybe (exp a) -> VHDLGen V.Type
compTM ct m = compTC ct (proxyM m)

compTF :: forall proxy ct exp a b . (CompileType ct, ct a) => Proxy ct -> (exp a -> b) -> VHDLGen V.Type
compTF ct f = compTC ct (proxyF f)

compTA :: forall proxy ct arr i a . (CompileType ct, ct a) => Proxy ct -> V.Range -> arr a -> VHDLGen V.Type
compTA ct range a =
  do i <- newSym (Base "array")
     t <- compTC ct (proxyE a)
     -- use work.types
     V.addLibrary "WORK"
     V.addImport  "WORK.types"
     let arr = V.constrainedArray (ident' i) t range
     b <- V.lookupArrayType arr
     case b of
       Just t' ->
         do return (named t')
       Nothing ->
         do V.addType arr
            return (typed arr)
  where
    typed :: V.TypeDeclaration -> V.SubtypeIndication
    typed (V.TDFull    (V.FullTypeDeclaration i _))     = named i
    typed (V.TDPartial (V.IncompleteTypeDeclaration i)) = named i

    named :: V.Identifier -> V.SubtypeIndication
    named i = V.SubtypeIndication Nothing (V.TMType (V.NSimple i)) Nothing

--------------------------------------------------------------------------------

compLC :: forall proxy ct a . (CompileType ct, ct a) => Proxy ct -> a -> VHDLGen V.Expression
compLC ct a = VHDLGenT $ CMS.lift $ compileLit ct a

compBC :: forall proxy ct a . (CompileType ct, ct a) => Proxy ct -> a -> VHDLGen V.Expression
compBC ct a = VHDLGenT $ CMS.lift $ compileBits ct a

--------------------------------------------------------------------------------

evalEM :: forall exp a . EvaluateExp exp => Maybe (exp a) -> a
evalEM e = maybe (error "empty value") id $ fmap evalE e

compER :: forall exp a . CompileExp exp => exp a -> VHDLGen (V.Expression)
compER e = VHDLGenT $ CMS.lift $ compE e

compEM :: forall exp a . CompileExp exp => Maybe (exp a) -> VHDLGen (Maybe V.Expression)
compEM e = maybe (return Nothing) (>>= return . Just) (fmap compER e)

--------------------------------------------------------------------------------

proxyE :: exp a -> Proxy a
proxyE _ = Proxy

proxyS :: Signal a -> Proxy a
proxyS _ = Proxy

proxyV :: Variable a -> Proxy a
proxyV _ = Proxy

proxyA :: Array i a -> Proxy a
proxyA _ = Proxy

proxyM :: Maybe (exp a) -> Proxy a
proxyM _ = Proxy

proxyF :: (exp a -> b) -> Proxy a
proxyF _ = Proxy

--------------------------------------------------------------------------------
-- Names.
--------------------------------------------------------------------------------

freshVar :: forall proxy ct exp a . (CompileType ct, ct a)
  => proxy ct -> Name -> VHDLGen (Val a)
freshVar _ prefix =
  do i <- newSym prefix
     t <- compTC (Proxy::Proxy ct) (Proxy::Proxy a)
     V.variable (ident' i) t Nothing
     return (ValC i)

newSym :: Name -> VHDLGen String
newSym (Base  n) = V.newSym n
newSym (Exact n) = return   n

--------------------------------------------------------------------------------
-- ** Signals.
--------------------------------------------------------------------------------

instance (CompileExp exp, CompileType ct)
    => Interp SignalCMD VHDLGen (Param2 exp ct)
  where
    interp = compileSignal

instance InterpBi SignalCMD IO (Param1 pred)
  where
    interpBi = runSignal

compileSignal :: forall exp ct a. (CompileExp exp, CompileType ct)
  => SignalCMD (Param3 VHDLGen exp ct) a
  -> VHDLGen a
compileSignal (NewSignal base exp) =
  do i <- newSym base
     v <- compEM exp
     t <- compTM (Proxy :: Proxy ct) exp
     V.signal (ident' i) V.InOut t (fmap (flip V.uType t) v)
     return (SignalC i)
compileSignal (GetSignal (SignalC s)) =
  do i <- freshVar (Proxy :: Proxy ct) (Base "v")
     V.assignVariable (simple $ ident i) (simple' s)
     return i
compileSignal (SetSignal (SignalC s) exp) =
  do e' <- compER exp
     t  <- compTC (Proxy :: Proxy ct) (proxyE exp)
     V.assignSignal (simple $ ident s) (V.uType e' t)
compileSignal (UnsafeFreezeSignal (SignalC s)) =
  do return $ ValC s

runSignal :: SignalCMD (Param3 IO IO pred) a -> IO a
runSignal x@(UnsafeFreezeSignal r) = runSignal (GetSignal r `asTypeOf` x)
runSignal _ = error "hardware-edsl.todo: run signals."

--------------------------------------------------------------------------------
-- ** Variables.
--------------------------------------------------------------------------------

instance (CompileExp exp, CompileType ct)
    => Interp VariableCMD VHDLGen (Param2 exp ct)
  where
    interp = compileVariable

instance InterpBi VariableCMD IO (Param1 pred)
  where
    interpBi = runVariable

-- todo: why not initialize variable?
compileVariable :: forall ct exp a. (CompileExp exp, CompileType ct)
  => VariableCMD (Param3 VHDLGen exp ct) a
  -> VHDLGen a
compileVariable (NewVariable base exp) =
  do i <- newSym base
     v <- compEM exp
     t <- compTM (Proxy::Proxy ct) exp
     V.variable (ident' i) t (Nothing)
     case v of
       Nothing -> return ()
       Just v' -> V.assignVariable (simple $ ident i) (V.uType v' t)
     return (VariableC i)
compileVariable (GetVariable (VariableC var)) =
  do i <- freshVar (Proxy :: Proxy ct) (Base "v")
     V.assignVariable (simple $ ident i) (simple' var)
     return i
compileVariable (SetVariable (VariableC var) exp) =
  do e' <- compER exp
     t  <- compTC (Proxy :: Proxy ct) (proxyE exp)
     V.assignVariable (simple var) (V.uType e' t)
compileVariable (UnsafeFreezeVariable (VariableC v)) =
  do return $ ValC v

runVariable :: VariableCMD (Param3 IO IO pred) a -> IO a
runVariable x@(UnsafeFreezeVariable v)      = runVariable (GetVariable v `asTypeOf` x)
runVariable (NewVariable _ Nothing)         = fmap VariableE $ IR.newIORef (error "uninitialized variable")
runVariable (NewVariable _ (Just a))        = fmap VariableE . IR.newIORef =<< a
runVariable (GetVariable (VariableE v))     = fmap ValE $ IR.readIORef v
runVariable (SetVariable (VariableE v) exp) = IR.writeIORef v =<< exp

--------------------------------------------------------------------------------
-- ** Arrays.
--------------------------------------------------------------------------------

instance (CompileExp exp, CompileType ct)
    => Interp ArrayCMD VHDLGen (Param2 exp ct)
  where
    interp = compileArray

instance InterpBi ArrayCMD IO (Param1 pred)
  where
    interpBi = runArray

compileArray :: forall ct exp a. (CompileExp exp, CompileType ct)
  => ArrayCMD (Param3 VHDLGen exp ct) a
  -> VHDLGen a
compileArray (NewArray base len) =
  do i <- newSym base
     l <- compER len
     let len = V.sub [unpackTerm l, lift $ V.literal $ V.number "1"]
     t <- compTA (Proxy :: Proxy ct) (rangeZero (lift len)) (undefined :: a)
     V.array (ident' i) V.InOut t Nothing
     return (ArrayC i)
compileArray (InitArray base is) =
  do i <- newSym base
     let len = V.sub [lift $ V.literal $ V.number $ show $ length is, lift $ V.literal $ V.number "1"]
     t <- compTA (Proxy :: Proxy ct) (rangeZero (lift len)) (undefined :: a)
     x <- mapM (compBC (Proxy::Proxy ct)) is
     let v = V.aggregate $ V.aggregated x
     V.array (ident' i) V.InOut t (Just $ lift v)
     return (ArrayC i)
compileArray (GetArray (ArrayC s) ix) =
  do i <- freshVar (Proxy :: Proxy ct) (Base "a")
     e <- compER ix
     V.assignVariable (simple $ ident i) (indexed' s e)
     return i
compileArray (SetArray (ArrayC s) ix e) =
  do ix' <- compER ix
     e'  <- compER e
     t   <- compTC (Proxy :: Proxy ct) (proxyE e)
     V.assignArray (indexed s ix') (V.uType e' t)
compileArray (CopyArray (ArrayC a, oa) (ArrayC b, ob) l) =
  do oa' <- compER oa
     ob' <- compER ob
     len <- compER l
     let upper_a = V.add [unpackTerm len, unpackTerm oa']
         upper_b = V.add [unpackTerm len, unpackTerm ob']
         dest    = slice  a $ range (lift upper_a) V.downto oa'
         src     = slice' b $ range (lift upper_b) V.downto ob'
     V.assignSignal dest src
compileArray (UnsafeFreezeArray (ArrayC a)) = return $ IArrayC a
compileArray (UnsafeThawArray (IArrayC a)) = return $ ArrayC a
compileArray (ResetArray (ArrayC a) rst) =
  do rst' <- compER rst
     t    <- compTC (Proxy :: Proxy ct) (proxyE rst)
     let others = V.aggregate $ V.others (V.uType rst' t)
     V.assignArray (simple a) (lift others)

runArray :: ArrayCMD (Param3 IO IO pred) a -> IO a
runArray = error "hardware-edsl.todo: run arrays"

--------------------------------------------------------------------------------
-- ** Virtual Arrays.
--------------------------------------------------------------------------------

instance (CompileExp exp, CompileType ct)
    => Interp VArrayCMD VHDLGen (Param2 exp ct)
  where
    interp = compileVArray

instance InterpBi VArrayCMD IO (Param1 pred)
  where
    interpBi = runVArray

compileVArray :: forall ct exp a. (CompileExp exp, CompileType ct)
  => VArrayCMD (Param3 VHDLGen exp ct) a
  -> VHDLGen a
compileVArray (NewVArray base len) =
  do i <- newSym base
     l <- compER len
     let len = V.sub [unpackTerm l, lift $ V.literal $ V.number "1"]
     t <- compTA (Proxy :: Proxy ct) (rangeZero (lift len)) (undefined :: a)
     V.variable (ident' i) t Nothing
     return (VArrayC i)
compileVArray (InitVArray base is) =
  do i <- newSym base
     let len = V.sub [lift $ V.literal $ V.number $ show $ length is, lift $ V.literal $ V.number "1"]
     t <- compTA (Proxy :: Proxy ct) (rangeZero (lift len)) (undefined :: a)
     x <- mapM (compBC (Proxy :: Proxy ct)) is
     let v = V.aggregate $ V.aggregated x
     V.variable (ident' i) t (Just $ lift v)
     return (VArrayC i)
compileVArray (GetVArray (VArrayC arr) ix) =
  do i <- freshVar (Proxy :: Proxy ct) (Base "a")
     e <- compER ix
     V.assignVariable (simple $ ident i) (indexed' arr e)
     return i
compileVArray (SetVArray a@(VArrayC arr) i e) =
  do i' <- compER i
     e' <- compER e
     t  <- compTC (Proxy::Proxy ct) (proxyE e)
     V.assignVariable (indexed arr i') (V.uType e' t)
compileVArray (CopyVArray (VArrayC a, oa) (VArrayC b, ob) l) =
  do oa' <- compER oa
     ob' <- compER ob
     len <- compER l
     let lower_a = V.add [unpackTerm len, unpackTerm oa']
         lower_b = V.add [unpackTerm len, unpackTerm ob']
         dest    = slice  a $ range oa' V.downto $ lift $ lower_a
         src     = slice' b $ range ob' V.downto $ lift $ lower_b
     V.assignVariable dest src
compileVArray (UnsafeFreezeVArray (VArrayC arr)) = return $ IArrayC arr
compileVArray (UnsafeThawVArray (IArrayC arr)) = return $ VArrayC arr

runVArray :: VArrayCMD (Param3 IO IO pred) a -> IO a
runVArray = error "hardware-edsl.todo: run arrays"

{-
runVArray (NewVArray _ len) =
  do len' <- len
     arr  <- IA.newArray_ (0, len')
     return (VArrayE arr)
runVArray (InitVArray _ is) =
  do arr  <- IA.newListArray (0, fromIntegral $ length is - 1) is
     return (VArrayE arr)
runVArray (GetVArray (VArrayE arr) i) =
  do ix  <- i
     (l, h) <- IA.getBounds arr
     if (ix < l || ix > h)
       then error "getArr out of bounds"
       else do
         v <- IA.readArray arr ix
         return (ValE v)
runVArray (SetVArray (VArrayE arr) i e) =
  do (l, h) <- IA.getBounds arr
     ix <- i
     e' <- e
     if (ix < l || ix > h)
        then error "setArr out of bounds"
        else IA.writeArray arr (fromIntegral ix) e'
runVArray (CopyVArray (VArrayE arr, oa) (VArrayE brr, ob) l) =
  do oa'     <- oa
     ob'     <- ob
     l'      <- l
     (0, ha) <- IA.getBounds arr
     (0, hb) <- IA.getBounds brr
     if (l' > hb + 1 - oa' || l' > ha + 1 - ob')
        then error "copyArr out of bounts"
        else sequence_ [ IA.readArray brr (i+ob') >>= IA.writeArray arr (i+oa')
                       | i <- genericTake l' [0..] ]
runVArray (UnsafeFreezeVArray (VArrayE arr)) = IA.freeze arr >>= return . IArrayE
runVArray (UnsafeThawVArray   (IArrayE arr)) = IA.thaw   arr >>= return . VArrayE
-}
--------------------------------------------------------------------------------
-- ** Loops.
--------------------------------------------------------------------------------

instance (CompileExp exp, CompileType ct)
    => Interp LoopCMD VHDLGen (Param2 exp ct)
  where
    interp = compileLoop

instance InterpBi LoopCMD IO (Param1 pred)
  where
    interpBi = runLoop

-- todo: loops, as expressed here, aren't synthesizable in general for the
-- range is given as expressions, which could be any variable. To ensure the
-- for-loops are synthesizable we should limit its range to a constant.
compileLoop :: forall ct exp a. (CompileExp exp, CompileType ct)
  => LoopCMD (Param3 VHDLGen exp ct) a
  -> VHDLGen a
compileLoop (For l u step) =
  do i    <- newSym (Base "l")
     l'   <- compER l
     u'   <- compER u
     t    <- compTC (Proxy :: Proxy ct) (proxyE l)
     let int v = V.uCast v t (V.integer Nothing)
         typ i = undefined
     loop <- V.inFor (ident' i)
       (range (int l') V.to (int u'))
       (step (ValC i))
     V.addSequential $ V.SLoop $ loop
compileLoop (While cont step) =
  do l    <- V.newLabel
     loop <- V.inWhile l Nothing $
       do b    <- cont
          exit <- compER b
          V.exit l exit
          step
     V.addSequential $ V.SLoop $ loop

runLoop :: LoopCMD (Param3 IO IO pred) a -> IO a
runLoop (For l u step) =
  do l' <- l
     u' <- u
     loop l' u'
  where
    loop l u | l <= u    = step (ValE l) >> loop (l + 1) u
             | otherwise = return ()
runLoop (While b step) = loop
  where
    loop = do e <- join b
              when e (step >> loop)

--------------------------------------------------------------------------------
-- ** Conditional.
--------------------------------------------------------------------------------

instance (CompileExp exp, CompileType ct)
    => Interp ConditionalCMD VHDLGen (Param2 exp ct)
  where
    interp = compileConditional

instance InterpBi ConditionalCMD IO (Param1 pred)
  where
    interpBi = runConditional

compileConditional :: forall ct exp a. (CompileExp exp, CompileType ct)
  => ConditionalCMD (Param3 VHDLGen exp ct) a
  -> VHDLGen a
compileConditional (If (a, b) cs em) =
  do let (es, ds) = unzip cs
         el = maybe (return ()) id em
     ae  <- compER a
     ese <- mapM compER es
     s   <- V.inConditional (ae, b) (zip ese ds) el
     V.addSequential $ V.SIf s
compileConditional (Case e cs d) =
  do let el = maybe (return ()) id d
     ae  <- compER e
     ce  <- mapM compC cs
     s   <- V.inCase ae ce el
     V.addSequential $ V.SCase s
  where
    compC :: ct b => When b VHDLGen -> VHDLGen (V.Choices, VHDLGen ())
    compC (When (Is e) p)   = do
      e' <- compLC (Proxy :: Proxy ct) e
      return $ (V.Choices [V.is $ unpackSimple e'], p)
    compC (When (To l h) p) = do
      l' <- compLC (Proxy :: Proxy ct) l
      h' <- compLC (Proxy :: Proxy ct) h
      return $ (V.Choices [V.between $ range l' V.to h'], p)
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
-- ** Processes.
--------------------------------------------------------------------------------

instance (CompileExp exp, CompileType ct)
    => Interp ProcessCMD VHDLGen (Param2 exp ct)
  where
    interp = compileProcess

instance InterpBi ProcessCMD IO (Param1 pred)
  where
    interpBi = runProcess

compileProcess :: forall ct exp a. (CompileExp exp, CompileType ct)
  => ProcessCMD (Param3 VHDLGen exp ct) a
  -> VHDLGen a
compileProcess (Process is prog rst) =
  do clock <- readClock
     reset <- readReset
     label <- V.newLabel
     let is'  = identifiers is
     let rst' = fmap ((,) reset) rst
     V.inSingleProcess label clock rst' (identifiers is) prog
  where
    identifiers :: Signals -> [V.Identifier]
    identifiers = fmap (\(Ident i) -> V.Ident i)

runProcess :: ProcessCMD (Param3 IO IO pred) a -> IO a
runProcess _ =
  do error "hardware-edsl-todo: figure out how to simulate processes in Haskell."

--------------------------------------------------------------------------------
-- ** Components.
--------------------------------------------------------------------------------

instance (CompileExp exp, CompileType ct)
    => Interp ComponentCMD VHDLGen (Param2 exp ct)
  where
    interp = compileComponent

instance InterpBi ComponentCMD IO (Param1 pred)
  where
    interpBi = runComponent

compileComponent :: forall ct exp a. (CompileExp exp, CompileType ct)
  => ComponentCMD (Param3 VHDLGen exp ct) a
  -> VHDLGen a
compileComponent (DeclareComponent base clock reset sig) =
  do comp <- newSym base
     clk  <- newSym clock
     rst  <- newSym reset
     args <- V.component $
       do (prg, is) <- V.entity  (ident' comp) (traverseSig clk rst [] sig)
          V.architecture (ident' comp) (V.Ident "behav") prg
          return is
     return (comp, args)
compileComponent (PortMap (Component name args sig) as) =
  do let i = ident' name
     l  <- V.newLabel
     vs <- applySig sig as args
     ac <- assocSig sig as args
     V.importComponent i vs
     V.portMap l i ac
     --
--     (V.VHDLEnv _ _ _ c _ _ _ _ _ _ _ _) <- CMS.get
--     error $ show c

runComponent :: ComponentCMD (Param3 IO IO pred) a -> IO a
runComponent _ = error "hardware-edsl-todo: run components."

--------------------------------------------------------------------------------

traverseSig :: forall ct exp a . (CompileExp exp, CompileType ct)
  => String   -- Clock name.
  -> String   -- Reset name.
  -> [String] -- Store of all parameters.
  -> Signature (Param3 VHDLGen exp ct) a
  -> VHDLGen (VHDLGen (), [String])
traverseSig clk rst is (Ret  prog) =
  do let c@(V.Ident c') = ident' clk
         r@(V.Ident r') = ident' rst
         xs = reverse $ r' : c' : is 
     V.port c V.In V.std_logic Nothing
     V.port r V.In V.std_logic Nothing
     let prg = localClock clk $ localReset rst $ prog 
     return (prg, xs)
traverseSig clk rst is (SSig n m sf) = 
  do i <- newSym n
     t <- compTF (Proxy :: Proxy ct) sf
     V.port (ident' i) m t Nothing
     traverseSig clk rst (i : is) (sf (SignalC i))
traverseSig clk rst is (SArr n m l af) =
  do i <- newSym n
     let len = V.sub [lift $ V.literal $ V.number $ show l, lift $ V.literal $ V.number "1"]
     t <- compTA (Proxy :: Proxy ct) (rangeZero (lift len)) (proxyF af)
     V.port (ident' i) m t Nothing
     traverseSig clk rst (i : is) (af (ArrayC i))

applySig :: forall ct exp a . (CompileExp exp, CompileType ct)
  => Signature (Param3 VHDLGen exp ct) a
  -> Argument ct a
  -> [String]
  -> VHDLGen [V.InterfaceDeclaration]
applySig (Ret _) (Nil) [c, r] = return [decl c, decl r]
  where
    decl :: String -> V.InterfaceDeclaration
    decl x = V.InterfaceSignalDeclaration [V.Ident x] (Just V.In) V.std_logic False Nothing
applySig (SSig _ m sf) (ASig s@(SignalC _) v) (n:ns) =
  do t  <- compTF (Proxy :: Proxy ct) sf
     is <- applySig (sf s) v ns
     let i = V.InterfaceSignalDeclaration [ident' n] (Just m) t False Nothing
     return (i : is)
applySig (SArr _ m l af) (AArr a@(ArrayC _) v) (n:ns) =
  do let len = V.sub [lift $ V.literal $ V.number $ show l, lift $ V.literal $ V.number "1"]
     t  <- compTA (Proxy :: Proxy ct) (rangeZero (lift len)) (proxyF af)
     is <- applySig (af a) v ns
     let i = V.InterfaceSignalDeclaration [ident' n] (Just m) t False Nothing
     return (i : is)

assocSig :: forall ct exp a . (CompileExp exp, CompileType ct)
  => Signature (Param3 VHDLGen exp ct) a
  -> Argument ct a
  -> [String]
  -> VHDLGen [(Maybe V.Identifier, V.Identifier)]
assocSig (Ret _) (Nil) [c, r] =
  do clk <- readClock
     rst <- readReset
     return [ (Just (V.Ident "clk"), clk)
            , (Just (V.Ident "rst"), rst)]
assocSig (SSig _ _ sf) (ASig s@(SignalC i) v) (n:ns) =
  ((Just (ident' n), ident' i) :) <$> assocSig (sf s) v ns
assocSig (SArr _ _ _ af) (AArr a@(ArrayC i) v) (n:ns) =
  ((Just (ident' n), ident' i) :) <$> assocSig (af a) v ns

--------------------------------------------------------------------------------
-- ** VHDL.
--------------------------------------------------------------------------------

instance (CompileExp exp, CompileType ct)
    => Interp VHDLCMD VHDLGen (Param2 exp ct)
  where
    interp = compileVHDL

instance InterpBi VHDLCMD IO (Param1 pred)
  where
    interpBi = runVHDL

compileVHDL :: forall ct exp a. (CompileExp exp, CompileType ct)
  => VHDLCMD (Param3 VHDLGen exp ct) a
  -> VHDLGen a
compileVHDL (DeclarePort base exp mode) =
  do i <- newSym base
     v <- compEM exp
     t <- compTM (Proxy::Proxy ct) exp
     V.port (ident' i) mode t v
     return (SignalC i)
compileVHDL (CopyBits (as@(SignalC a), oa) (bs@(SignalC b), ob) l) =
  do oa' <- compER oa
     ob' <- compER ob
     len <- compER l
     ta  <- compTC (Proxy :: Proxy ct) (proxyS as)
     tb  <- compTC (Proxy :: Proxy ct) (proxyS bs)
     let upper_oa' = V.add [unpackTerm len, unpackTerm oa']
         upper_ob' = V.add [unpackTerm len, unpackTerm ob']
     let dest = slice  a $ range (lift upper_oa') V.downto oa'
         src  = slice' b $ range (lift upper_ob') V.downto ob'
     V.assignSignal dest (V.uCoerce src tb ta)
compileVHDL (CopyVBits (av@(VariableC a), oa) (bs@(SignalC b), ob) l) =
  do oa' <- compER oa
     ob' <- compER ob
     len <- compER l
     ta  <- compTC (Proxy :: Proxy ct) (proxyV av)
     tb  <- compTC (Proxy :: Proxy ct) (proxyS bs)
     let upper_oa' = V.add [unpackTerm len, unpackTerm oa']
         upper_ob' = V.add [unpackTerm len, unpackTerm ob']
     let dest  = slice  a $ range (lift upper_oa') V.downto oa'
         src   = slice' b $ range (lift upper_ob') V.downto ob'
     V.assignVariable dest (V.uCoerce src tb ta)
compileVHDL (CopyABits (ar@(ArrayC a), oa, ia) (bs@(SignalC b), ob) l) =
  do oa' <- compER oa
     ia' <- compER ia
     ob' <- compER ob
     len <- compER l
     ta  <- compTC (Proxy :: Proxy ct) (proxyA ar)
     tb  <- compTC (Proxy :: Proxy ct) (proxyS bs)
     let upper_oa' = V.add [unpackTerm len, unpackTerm oa']
         upper_ob' = V.add [unpackTerm len, unpackTerm ob']
     let index = indexed a ia'
         dest  = V.slice index $ range (lift upper_oa') V.downto oa'
         src   = slice'  b     $ range (lift upper_ob') V.downto ob'
     V.assignSignal dest (V.uCoerce src tb ta)
compileVHDL (GetBit (SignalC bits) ix) =
  do i   <- freshVar (Proxy :: Proxy ct) (Base "b")
     ix' <- compER ix
     V.assignVariable (simple $ ident i) (indexed' bits ix')
     return i
compileVHDL (SetBit s@(SignalC bits) ix bit) =
  do ix'  <- compER ix
     bit' <- compER bit
     t    <- compTC (Proxy :: Proxy ct) (proxyE s)
     case V.isBit t of
       True  -> V.assignSignal (simple bits)      (bit')
       False -> V.assignArray  (indexed bits ix') (bit')
compileVHDL (GetBits s@(SignalC bits) start len) =
  do i      <- freshVar (Proxy :: Proxy ct) (Base "b")
     start' <- compER start
     len'   <- compER len
     typ    <- compTC (Proxy :: Proxy ct) (proxyS s)
     let upper = V.add [unpackTerm len', unpackTerm start']
         exp   = slice' bits $ range (lift upper) V.downto start'
     V.assignVariable (simple $ ident i) (V.uCoerce exp typ $ V.integer Nothing)
     return i

runVHDL :: VHDLCMD (Param3 IO IO pred) a -> IO a
runVHDL = error "hardware-edsl.runVHDL: todo."

--------------------------------------------------------------------------------
-- Helpers.
--------------------------------------------------------------------------------

-- todo: this... why does this work?
instance ToIdent String where toIdent = Ident
instance ToIdent Ident  where toIdent = id
instance ToIdent Name   where
  toIdent (Base s)  = Ident s
  toIdent (Exact s) = Ident s

ident :: ToIdent a => a -> String
ident a = let (Ident s) = toIdent a in s

ident' :: ToIdent a => a -> V.Identifier
ident' a = V.Ident $ ident a

--------------------------------------------------------------------------------

simple   :: String -> V.Name
simple   s = V.simple s

simple'  :: String -> V.Expression
simple'  s = lift $ V.name $ simple s

indexed  :: String -> V.Expression -> V.Name
indexed  s = V.indexed $ V.simple s

indexed' :: String -> V.Expression -> V.Expression
indexed' s = lift . V.name . indexed s

slice    :: String -> V.Range -> V.Name
slice    s = V.slice $ V.simple s

slice'   :: String -> V.Range -> V.Expression
slice'   s = lift . V.name . slice s

literal  :: String -> V.Expression
literal  s = lift $ V.literal $ V.number s

range    :: V.Expression -> V.Direction -> V.Expression -> V.Range
range    l dir r = V.range (unpackSimple l) dir (unpackSimple r)

--------------------------------------------------------------------------------

rangeZero :: V.Expression -> V.Range
rangeZero l = V.range (unpackSimple l) V.downto (V.point 0)

rangePoint :: Integral a => a -> V.Range
rangePoint a = V.range (V.point $ toInteger a) V.downto (V.point 0)

--------------------------------------------------------------------------------

unpackShift :: V.Expression -> V.ShiftExpression
unpackShift (V.ENand (V.Relation s Nothing) Nothing) = s
unpackShift e = lift e

unpackSimple :: V.Expression -> V.SimpleExpression
unpackSimple (V.ENand (V.Relation (V.ShiftExpression s Nothing) Nothing) Nothing) = s
unpackSimple e = lift e

unpackTerm :: V.Expression -> V.Term
unpackTerm (V.ENand (V.Relation (V.ShiftExpression (V.SimpleExpression Nothing t []) Nothing) Nothing) Nothing) = t
unpackTerm e = lift e

--------------------------------------------------------------------------------
