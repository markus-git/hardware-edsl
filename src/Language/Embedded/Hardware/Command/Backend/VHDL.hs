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
import Language.Embedded.Hardware.Expression.Represent.Bit (Bits, ni)
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

import GHC.TypeLits (KnownNat)

--------------------------------------------------------------------------------
-- * Translation of hardware commands into VHDL.
--------------------------------------------------------------------------------

class CompileType ct
  where
    compileType :: ct a => proxy1 ct -> proxy2 a -> VHDL V.Type
    compileLit  :: ct a => proxy1 ct ->        a -> VHDL V.Expression
    compileBits :: ct a => proxy1 ct ->        a -> VHDL V.Expression

instance CompileType PrimType
  where
    compileType _ = compT
    compileLit  _ = return . literal . primTypeVal
    compileBits _ = return . literal . primTypeBits

--------------------------------------------------------------------------------

compT :: forall proxy a . PrimType a => proxy a -> VHDL V.Type
compT _ = declareType (Proxy :: Proxy a)

compTM :: forall proxy ct exp a . (CompileType ct, ct a)
  => proxy ct -> Maybe (exp a) -> VHDL V.Type
compTM _ _ = compileType (Proxy::Proxy ct) (Proxy::Proxy a)

compTF :: forall proxy ct exp a b . (CompileType ct, ct a)
  => proxy ct -> (exp a -> b) -> VHDL V.Type
compTF _ _ = compileType (Proxy::Proxy ct) (Proxy::Proxy a)

compTA :: forall proxy ct array i a . (CompileType ct, ct a)
  => proxy ct -> V.Range -> array a -> VHDL V.Type
compTA _ range _ =
  do i <- newSym (Base "array")
     t <- compileType (Proxy::Proxy ct) (Proxy::Proxy a)
     let array = V.constrainedArray (ident' i) t range
     s <- V.findType array
     case s of
       Just n  -> do return (named n)
       Nothing -> do V.addType array
                     return (typed array)
  where
    typed :: V.TypeDeclaration -> V.SubtypeIndication
    typed (V.TDFull    (V.FullTypeDeclaration i _))     = named i
    typed (V.TDPartial (V.IncompleteTypeDeclaration i)) = named i

    named :: V.Identifier -> V.SubtypeIndication
    named i = V.SubtypeIndication Nothing (V.TMType (V.NSimple i)) Nothing

--------------------------------------------------------------------------------

evalEM :: forall exp a . EvaluateExp exp
  => Maybe (exp a) -> a
evalEM e = maybe (error "empty value") id $ fmap evalE e

compEM :: forall exp a . CompileExp exp
  => Maybe (exp a) -> VHDL (Maybe V.Expression)
compEM e = maybe (return Nothing) (>>= return . Just) $ fmap compE e

--------------------------------------------------------------------------------

proxyE :: exp a -> Proxy a
proxyE _ = Proxy

proxyM :: Maybe (exp a) -> Proxy a
proxyM _ = Proxy

proxyF :: (exp a -> b) -> Proxy a
proxyF _ = Proxy

--------------------------------------------------------------------------------

freshVar :: forall proxy ct exp a . (CompileType ct, ct a)
  => proxy ct -> Name -> VHDL (Val a)
freshVar _ prefix =
  do i <- newSym prefix
     t <- compileType (Proxy::Proxy ct) (Proxy::Proxy a)
     V.variable (ident' i) t Nothing
     return (ValC i)

newSym :: Name -> VHDL String
newSym (Base  n) = V.newSym n
newSym (Exact n) = return   n

ident :: ToIdent a => a -> String
ident a = let (Ident s) = toIdent a in s

ident' :: ToIdent a => a -> V.Identifier
ident' a = V.Ident $ ident a

-- todo: this... why does this work?
instance ToIdent String where toIdent = Ident
instance ToIdent Ident  where toIdent = id
instance ToIdent Name   where
  toIdent (Base s)  = Ident s
  toIdent (Exact s) = Ident s

--------------------------------------------------------------------------------
-- ** Signals.
--------------------------------------------------------------------------------

instance (CompileExp exp, CompileType ct) => Interp SignalCMD VHDL (Param2 exp ct)
  where
    interp = compileSignal

instance InterpBi SignalCMD IO (Param1 pred)
  where
    interpBi = runSignal

-- todo: is concurrent... really necessary? I think the VHDL monad handle that.
compileSignal :: forall exp ct a. (CompileExp exp, CompileType ct) => SignalCMD (Param3 VHDL exp ct) a -> VHDL a
compileSignal (NewSignal base mode exp) =
  do i <- newSym base
     v <- compEM exp
     t <- compTM (Proxy::Proxy ct) exp
     V.signal (ident' i) mode t v
     return (SignalC i)
compileSignal (GetSignal (SignalC s)) =
  do i <- freshVar (Proxy::Proxy ct) (Base "v")
     V.assignVariable (simple $ ident i) (simple' s)
     return i
compileSignal (SetSignal (SignalC s) exp) =
  do e' <- compE exp
     t  <- compileType (Proxy::Proxy ct) (proxyE exp)
     V.assignSignal (simple $ ident s) (V.uType e' t)
compileSignal (UnsafeFreezeSignal (SignalC s)) =
  do return $ ValC s
compileSignal (ConcurrentSetSignal (SignalC s) exp) =
  do e <- compE exp
     V.concurrentSignal (simple $ ident s) e

runSignal :: SignalCMD (Param3 IO IO pred) a -> IO a
runSignal (NewSignal _ _ Nothing)     = fmap SignalE $ IR.newIORef (error "uninitialized signal")
runSignal (NewSignal _ _ (Just a))    = fmap SignalE . IR.newIORef =<< a
runSignal (GetSignal (SignalE r))     = fmap ValE $ IR.readIORef r
runSignal (SetSignal (SignalE r) exp) = IR.writeIORef r =<< exp
runSignal x@(UnsafeFreezeSignal r)    = runSignal (GetSignal r `asTypeOf` x)
runSignal (ConcurrentSetSignal (SignalE r) exp) =
  error "hardware-edsl.todo: run concurrent signals."

--------------------------------------------------------------------------------
-- ** Variables.
--------------------------------------------------------------------------------

instance (CompileExp exp, CompileType ct) => Interp VariableCMD VHDL (Param2 exp ct)
  where
    interp = compileVariable

instance InterpBi VariableCMD IO (Param1 pred)
  where
    interpBi = runVariable

-- todo: why not initialize variable?
compileVariable :: forall ct exp a. (CompileExp exp, CompileType ct) => VariableCMD (Param3 VHDL exp ct) a -> VHDL a
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
  do i <- freshVar (Proxy::Proxy ct) (Base "v")
     V.assignVariable (simple $ ident i) (simple' var)
     return i
compileVariable (SetVariable (VariableC var) exp) =
  do e' <- compE exp
     t  <- compileType (Proxy::Proxy ct) (proxyE exp)
     V.assignVariable (simple var) (V.uType e' t)
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
--------------------------------------------------------------------------------

instance (CompileExp exp, CompileType ct) => Interp ConstantCMD VHDL (Param2 exp ct)
  where
    interp = compileConstant

instance InterpBi ConstantCMD IO (Param1 pred)
  where
    interpBi = runConstant

compileConstant :: forall ct exp a. (CompileExp exp, CompileType ct) => ConstantCMD (Param3 VHDL exp ct) a -> VHDL a
compileConstant (NewConstant base (exp :: exp c)) =
  do i <- newSym base
     v <- compE exp
     t <- compileType (Proxy::Proxy ct) (Proxy::Proxy c)
     V.constant (ident' i) t v
     return (ConstantC i)
compileConstant (GetConstant (ConstantC c)) =
  do return $ ValC c

runConstant :: ConstantCMD (Param3 IO IO pred) a -> IO a
runConstant (NewConstant _ exp)           = return . ConstantE =<< exp
runConstant (GetConstant (ConstantE exp)) = return $ ValE exp

--------------------------------------------------------------------------------
-- ** Arrays.
--------------------------------------------------------------------------------

instance (CompileExp exp, CompileType ct) => Interp ArrayCMD VHDL (Param2 exp ct)
  where
    interp = compileArray

instance InterpBi ArrayCMD IO (Param1 pred)
  where
    interpBi = runArray

compileArray :: forall ct exp a. (CompileExp exp, CompileType ct) => ArrayCMD (Param3 VHDL exp ct) a -> VHDL a
compileArray (NewArray base len) =
  do i <- newSym base
     l <- compE len
     t <- compTA (Proxy::Proxy ct) (rangeZero l) (undefined :: a)
     V.array (ident' i) V.InOut t Nothing
     return (ArrayC i)
compileArray (InitArray base is) =
  do i <- newSym base
     t <- compTA (Proxy::Proxy ct) (rangePoint (length is - 1)) (undefined :: a)
     x <- mapM (compileBits (Proxy::Proxy ct)) is
     let v = V.aggregate $ V.aggregated x
     V.array (ident' i) V.InOut t (Just $ lift v)
     return (ArrayC i)
compileArray (GetArray (ArrayC s) ix) =
  do i <- freshVar (Proxy::Proxy ct) (Base "a")
     e <- compE ix
     V.assignVariable (simple $ ident i) (indexed' s e)
     return i
compileArray (SetArray (ArrayC s) ix e) =
  do ix' <- compE ix
     e'  <- compE e
     t   <- compileType (Proxy::Proxy ct) (proxyE e)
     V.assignArray (indexed s ix') (V.uType e' t)
compileArray (CopyArray (ArrayC a, oa) (ArrayC b, ob) l) =
  do oa' <- compE oa
     ob' <- compE ob
     len <- compE l
     let lower_a = V.add [unpackTerm len, unpackTerm oa']
         lower_b = V.add [unpackTerm len, unpackTerm ob']
         dest    = slice  a $ range oa' V.downto $ lift $ lower_a
         src     = slice' b $ range ob' V.downto $ lift $ lower_b
     V.assignSignal dest src
compileArray (ResetArray (ArrayC a) rst) =
  do rst' <- compE rst
     t    <- compileType (Proxy::Proxy ct) (proxyE rst)
     let others = V.aggregate $ V.others (V.uType rst' t)
     V.assignArray (simple a) (lift others)

runArray :: ArrayCMD (Param3 IO IO pred) a -> IO a
runArray = error "hardware-edsl.todo: run arrays"

--------------------------------------------------------------------------------
-- ** Virtual Arrays.
--------------------------------------------------------------------------------

instance (CompileExp exp, CompileType ct) => Interp VArrayCMD VHDL (Param2 exp ct)
  where
    interp = compileVArray

instance InterpBi VArrayCMD IO (Param1 pred)
  where
    interpBi = runVArray

compileVArray :: forall ct exp a. (CompileExp exp, CompileType ct) => VArrayCMD (Param3 VHDL exp ct) a -> VHDL a
compileVArray (NewVArray base len) =
  do l <- compE len
     t <- compTA (Proxy::Proxy ct) (rangeZero l) (undefined :: a)
     i <- newSym base
     V.variable (ident' i) t Nothing
     return (VArrayC i)
compileVArray (InitVArray base is) =
  do let r = rangePoint (length is - 1)
     t <- compTA (Proxy::Proxy ct) r (undefined :: a)
     i <- newSym base
     x <- mapM (compileBits (Proxy::Proxy ct)) is
     let v = V.aggregate $ V.aggregated x
     V.variable (ident' i) t (Just $ lift v)
     return (VArrayC i)
compileVArray (GetVArray (VArrayC arr) ix) =
  do i <- freshVar (Proxy::Proxy ct) (Base "a")
     e <- compE ix
     V.assignVariable (simple $ ident i) (indexed' arr e)
     return i
compileVArray (SetVArray a@(VArrayC arr) i e) =
  do i' <- compE i
     e' <- compE e
     t  <- compileType (Proxy::Proxy ct) (proxyE e)
     V.assignVariable (indexed arr i') (V.uType e' t)
compileVArray (CopyVArray (VArrayC a, oa) (VArrayC b, ob) l) =
  do oa' <- compE oa
     ob' <- compE ob
     len <- compE l
     let lower_a = V.add [unpackTerm len, unpackTerm oa']
         lower_b = V.add [unpackTerm len, unpackTerm ob']
         dest    = slice  a $ range oa' V.downto $ lift $ lower_a
         src     = slice' b $ range ob' V.downto $ lift $ lower_b
     V.assignVariable dest src
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
runVArray (GetVArray (VArrayE arr) i) =
  do (l, h) <- IA.getBounds arr
     ix  <- i
     if (ix < l || ix > h)
        then error "getArr out of bounds"
        else do v <- IA.readArray arr ix
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

--------------------------------------------------------------------------------
-- ** Loops.
--------------------------------------------------------------------------------

instance (CompileExp exp, CompileType ct) => Interp LoopCMD VHDL (Param2 exp ct)
  where
    interp = compileLoop

instance InterpBi LoopCMD IO (Param1 pred)
  where
    interpBi = runLoop

compileLoop :: forall ct exp a. (CompileExp exp, CompileType ct) => LoopCMD (Param3 VHDL exp ct) a -> VHDL a
compileLoop (For l u step) =
  do -- *** todo: temp solution, should check if signed and size.
     i    <- newSym (Base "l")
     l'   <- compE l
     u'   <- compE u
     loop <- V.inFor (ident' i) (range l' V.to u') (step (ValC i))
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

instance (CompileExp exp, CompileType ct) => Interp ConditionalCMD VHDL (Param2 exp ct)
  where
    interp = compileConditional

instance InterpBi ConditionalCMD IO (Param1 pred)
  where
    interpBi = runConditional

compileConditional :: forall ct exp a. (CompileExp exp, CompileType ct) => ConditionalCMD (Param3 VHDL exp ct) a -> VHDL a
compileConditional (If (a, b) cs em) =
  do let (es, ds) = unzip cs
         el = maybe (return ()) id em
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
      return $ (V.Choices [V.is $ unpackSimple e'], p)
    compC (When (To l h) p) = do
      l' <- compileLit (Proxy::Proxy ct) l
      h' <- compileLit (Proxy::Proxy ct) h
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
-- ** Components.
--------------------------------------------------------------------------------

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
       do p <- V.entity  (ident' comp) (traverseSig sig)
          V.architecture (ident' comp) (V.Ident "imp") p
     return comp
compileComponent (PortMap (Component name sig) as) =
  do let i = ident' name
     l  <- V.newLabel
     vs <- applySig sig as
     V.inheritContext i
     V.declareComponent i vs
     V.portMap l i (assocSig sig as)

runComponent :: ComponentCMD (Param3 IO IO pred) a -> IO a
runComponent = error "hardware-edsl-todo: run components."

--------------------------------------------------------------------------------

traverseSig :: forall ct exp a . (CompileExp exp, CompileType ct) => Signature (Param3 VHDL exp ct) a -> VHDL (VHDL ())
traverseSig (Ret  prog)   = return prog
traverseSig (SSig n m sf) = 
  do i <- newSym n
     t <- compTF (Proxy::Proxy ct) sf
     V.signal (ident' i) m t Nothing
     traverseSig (sf (SignalC i))
traverseSig (SArr n m l af) =
  do i <- newSym n
     t <- compTA (Proxy::Proxy ct) (rangePoint l) (proxyF af)
     V.array (ident' i) m t Nothing
     traverseSig (af (ArrayC i))

applySig :: forall ct exp a . (CompileExp exp, CompileType ct)
  => Signature (Param3 VHDL exp ct) a -> Argument ct a
  -> VHDL [V.InterfaceDeclaration]
applySig (Ret _)       (Nil)                  = return []
applySig (SSig n m sf) (ASig s@(SignalC i) v) =
  do t  <- compTF (Proxy::Proxy ct) sf
     is <- applySig (sf s) v
     let i = V.InterfaceSignalDeclaration [ident' n] (Just m) t False Nothing
     return (i : is)
applySig (SArr n m l af) (AArr a@(ArrayC i) v) =
  do t  <- compTA (Proxy::Proxy ct) (rangePoint l) (proxyF af)
     is <- applySig (af a) v
     let i = V.InterfaceSignalDeclaration [ident' n] (Just m) t False Nothing
     return (i : is)

assocSig :: forall ct exp a . (CompileExp exp, CompileType ct)
  => Signature (Param3 VHDL exp ct) a -> Argument ct a
  -> [(V.Identifier, V.Identifier)]
assocSig (Ret _)         (Nil)                  = []
assocSig (SSig n _ sf)   (ASig s@(SignalC i) v) =
  (ident' n, ident' i) : assocSig (sf s) v
assocSig (SArr n _ _ af) (AArr a@(ArrayC i) v)  =
  (ident' n, ident' i) : assocSig (af a) v

--------------------------------------------------------------------------------
-- ** Structural.
--------------------------------------------------------------------------------

instance (CompileExp exp, CompileType ct) => Interp StructuralCMD VHDL (Param2 exp ct)
  where
    interp = compileStructural

instance InterpBi StructuralCMD IO (Param1 pred)
  where
    interpBi = runStructural

compileStructural :: forall ct exp a. (CompileExp exp, CompileType ct) => StructuralCMD (Param3 VHDL exp ct) a -> VHDL a
compileStructural (StructEntity (Exact e) prog)  =
  do V.entity (V.Ident e) prog
compileStructural (StructArchitecture (Exact e) (Exact a) prog) =
  do V.architecture (V.Ident e) (V.Ident a) prog
compileStructural (StructProcess xs prog) =
  do label  <- V.newLabel
     (a, c) <- V.inProcess label (fmap (\(Ident i) -> V.Ident i) xs) prog
     V.addConcurrent (V.ConProcess c)
     return a

runStructural :: StructuralCMD (Param3 IO IO pred) a -> IO a
runStructural (StructEntity _ prog)         = prog
runStructural (StructArchitecture _ _ prog) = prog
runStructural (StructProcess xs prog)       =
  do error "hardware-edsl-todo: figure out how to simulate processes in Haskell."

--------------------------------------------------------------------------------
-- ** VHDL.
--------------------------------------------------------------------------------

instance (CompileExp exp, CompileType ct) => Interp VHDLCMD VHDL (Param2 exp ct)
  where
    interp = compileVHDL

instance InterpBi VHDLCMD IO (Param1 pred)
  where
    interpBi = runVHDL

compileVHDL :: forall ct exp a. (CompileExp exp, CompileType ct) => VHDLCMD (Param3 VHDL exp ct) a -> VHDL a
compileVHDL (Rising (SignalC clk) (SignalC rst) tru fls) =
  do let condC = V.function (simple "rising_edge") [simple' clk]
         condR = V.eq (unpackShift $ simple' rst) (unpackShift $ literal "'0'")
     clock <- V.inConditional (lift condC,
         do reset <- V.inConditional (lift condR, tru) [] (fls)
            V.addSequential $ V.SIf $ reset
       ) [] (return ())
     V.addSequential $ V.SIf $ clock
compileVHDL (CopyBits ((SignalC a), oa) ((SignalC b), ob) l) =
  do oa' <- compE oa
     ob' <- compE ob
     len <- compE l
     let lower_a = V.add [unpackTerm len, unpackTerm oa']
         lower_b = V.add [unpackTerm len, unpackTerm ob']
         dest    = slice  a $ range oa' V.downto $ lift $ lower_a
         src     = slice' b $ range ob' V.downto $ lift $ lower_b
     V.assignSignal dest src
compileVHDL (CopyVBits ((VariableC a), oa) ((SignalC b), ob) l) =
  do oa' <- compE oa
     ob' <- compE ob
     len <- compE l
     let lower_a = V.add [unpackTerm len, unpackTerm oa']
         lower_b = V.add [unpackTerm len, unpackTerm ob']
         dest    = slice  a $ range oa' V.downto $ lift $ lower_a
         src     = slice' b $ range ob' V.downto $ lift $ lower_b
     V.assignVariable dest src
compileVHDL (GetBit (SignalC bits) ix) =
  do i   <- freshVar (Proxy::Proxy ct) (Base "b")
     ix' <- compE ix
     V.assignVariable (simple $ ident i) (indexed' bits ix')
     return i
compileVHDL (SetBit s@(SignalC bits) ix bit) =
  do ix'  <- compE ix
     bit' <- compE bit
     t    <- compileType (Proxy::Proxy ct) (proxyE s)
     case V.isBit t of
       True  -> V.assignSignal (simple bits)      (bit')
       False -> V.assignArray  (indexed bits ix') (bit')
compileVHDL (GetBits (SignalC bits) l u) =
  do i  <- freshVar (Proxy::Proxy ct) (Base "b")
     l' <- compE l
     u' <- compE u
     -- todo: this wrap around.
     V.assignVariable (simple $ ident i)
       ( lift $ V.toInteger $
         lift $ V.asSigned  $
         slice' bits $
         range l' V.downto u')
     return i

runVHDL :: VHDLCMD (Param3 IO IO pred) a -> IO a
runVHDL = error "hardware-edsl.runVHDL: todo."

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

literal :: String -> V.Expression
literal s = lift $ V.literal $ V.number s

range  :: V.Expression -> V.Direction -> V.Expression -> V.Range
range l dir r = V.range (unpackSimple l) dir (unpackSimple r)

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
