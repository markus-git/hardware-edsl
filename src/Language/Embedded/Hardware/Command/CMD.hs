{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Language.Embedded.Hardware.Command.CMD where

import Language.Embedded.VHDL (Mode)
import Language.Embedded.Hardware.Interface
import Language.Embedded.Hardware.Expression.Represent.Bit

import Control.Monad.Reader (ReaderT(..), runReaderT, lift)
import Control.Monad.Operational.Higher

import Data.Ix       (Ix)
import Data.IORef    (IORef)
import Data.Array.IO (IOArray)
import qualified Data.Array as Arr

import qualified Language.VHDL as V (Expression, Name, Identifier)

--import GHC.TypeLits
import qualified GHC.Exts as GHC (Constraint)

--------------------------------------------------------------------------------
-- * Hardware commands.
--------------------------------------------------------------------------------

data Name =
    None
  | Base  VarId
  | Exact VarId 

-- | ...
swapM :: Monad m => Maybe (m a) -> m (Maybe a)
swapM = maybe (return Nothing) (>>= return . Just)

--------------------------------------------------------------------------------
-- ** Values.

-- | Value representation.
data Val a = ValC String | ValE a

-- | ...
valToExp :: (PredicateExp exp a, FreeExp exp) => Val a -> exp a
valToExp (ValC i) = varE i
valToExp (ValE a) = litE a

--------------------------------------------------------------------------------
-- ** Signals.

-- | Signal representation.
data Signal a = SignalC VarId | SignalE (IORef a)

-- | Commands for signals.
data SignalCMD fs a
  where
    -- ^ Create a new signal.
    NewSignal :: pred a => Name -> Mode -> Maybe (exp a) -> SignalCMD (Param3 prog exp pred) (Signal a)
    -- ^ Fetch the contents of a signal.
    GetSignal :: pred a => Signal a -> SignalCMD (Param3 prog exp pred) (Val a)
    -- ^ Write the value to a signal.
    SetSignal :: pred a => Signal a -> exp a -> SignalCMD (Param3 prog exp pred) ()
    -- ^ Unsafe version of fetching a signal.
    UnsafeFreezeSignal :: pred a => Signal a -> SignalCMD (Param3 prog exp pred) (Val a)
    -- *** ...

instance HFunctor SignalCMD
  where
    hfmap _ (NewSignal n m e) = NewSignal n m e
    hfmap _ (GetSignal s) = GetSignal s
    hfmap _ (SetSignal s e) = SetSignal s e
    hfmap _ (UnsafeFreezeSignal s) = UnsafeFreezeSignal s

instance HBifunctor SignalCMD
  where
    hbimap _ f (NewSignal n m e) = NewSignal n m (fmap f e)
    hbimap _ _ (GetSignal s) = GetSignal s
    hbimap _ f (SetSignal s e) = SetSignal s (f e)
    hbimap _ _ (UnsafeFreezeSignal s) = UnsafeFreezeSignal s

instance (SignalCMD :<: instr) => Reexpressible SignalCMD instr
  where
    reexpressInstrEnv reexp (NewSignal n m e) = lift . singleInj . NewSignal n m =<< swapM (fmap reexp e)
    reexpressInstrEnv reexp (GetSignal s) = lift $ singleInj $ GetSignal s
    reexpressInstrEnv reexp (SetSignal s e) = lift . singleInj . SetSignal s =<< reexp e
    reexpressInstrEnv reexp (UnsafeFreezeSignal s) = lift $ singleInj $ UnsafeFreezeSignal s

--------------------------------------------------------------------------------
-- ** Variables.

-- | Variable representation.
data Variable a = VariableC VarId | VariableE (IORef a)

-- | Commands for variables.
data VariableCMD fs a
  where
    -- ^ Create a new variable.
    NewVariable :: pred a => Name -> Maybe (exp a) -> VariableCMD (Param3 prog exp pred) (Variable a)
    -- ^ Fetch the contents of a variable.
    GetVariable :: pred a => Variable a -> VariableCMD (Param3 prog exp pred) (Val a)
    -- ^ Write the value to a variable.
    SetVariable :: pred a => Variable a -> exp a -> VariableCMD (Param3 prog exp pred) ()
    -- ^ Unsafe version of fetching a variable.
    UnsafeFreezeVariable :: pred a => Variable a -> VariableCMD (Param3 prog exp pred) (Val a)

instance HFunctor VariableCMD
  where
    hfmap _ (NewVariable n e)        = NewVariable n e
    hfmap _ (GetVariable s)          = GetVariable s
    hfmap _ (SetVariable s e)        = SetVariable s e
    hfmap _ (UnsafeFreezeVariable s) = UnsafeFreezeVariable s

instance HBifunctor VariableCMD
  where
    hbimap _ f (NewVariable n e) = NewVariable n (fmap f e)
    hbimap _ _ (GetVariable v) = GetVariable v
    hbimap _ f (SetVariable v e) = SetVariable v (f e)
    hbimap _ _ (UnsafeFreezeVariable v) = UnsafeFreezeVariable v

instance (VariableCMD :<: instr) => Reexpressible VariableCMD instr
  where
    reexpressInstrEnv reexp (NewVariable n e) = lift . singleInj . NewVariable n =<< swapM (fmap reexp e)
    reexpressInstrEnv reexp (GetVariable v) = lift $ singleInj $ GetVariable v
    reexpressInstrEnv reexp (SetVariable v e) = lift . singleInj . SetVariable v =<< reexp e
    reexpressInstrEnv reexp (UnsafeFreezeVariable v) = lift $ singleInj $ UnsafeFreezeVariable v

--------------------------------------------------------------------------------
-- ** Constants.

-- | Constant representation.
data Constant a = ConstantC VarId | ConstantE a

-- | Commands for constants.
data ConstantCMD fs a
  where
    -- ^ Create a new constant.
    NewConstant :: pred a => Name -> exp a -> ConstantCMD (Param3 prog exp pred) (Constant a)
    -- ^ Fetch the value of a constant.
    GetConstant :: pred a => Constant a -> ConstantCMD (Param3 prog exp pred) (Val a)

instance HFunctor ConstantCMD
  where
    hfmap _ (NewConstant n e) = NewConstant n e
    hfmap _ (GetConstant c)   = GetConstant c

instance HBifunctor ConstantCMD
  where
    hbimap _ f (NewConstant n e) = NewConstant n (f e)
    hbimap _ _ (GetConstant c)   = GetConstant c

instance (ConstantCMD :<: instr) => Reexpressible ConstantCMD instr
  where
    reexpressInstrEnv reexp (NewConstant n e) = lift . singleInj . NewConstant n =<< reexp e
    reexpressInstrEnv reexp (GetConstant c) = lift $ singleInj $ GetConstant c

--------------------------------------------------------------------------------
-- ** Arrays.

-- | Expression types that support compilation of array indexing
class CompArrayIx exp
  where
    -- | Generate code for an array indexing operation
    compArrayIx :: PredicateExp exp a => exp i -> Array i a -> Maybe (exp a)
    compArrayIx _ _ = Nothing

-- | Array reprensentation.
data Array i a = ArrayC VarId | ArrayE (IOArray i a)

-- | Commands for signal arrays.
data ArrayCMD fs a
  where
    -- *** ...
    GetRange
      :: (pred i, Integral i, Ix i, pred UBits)
      => exp i -> exp i -> Signal (Bits n)
      -> ArrayCMD (Param3 prog exp pred) (Val UBits)
    SetRange
      :: (pred i, Integral i, Ix i, pred UBits)
      => exp i -> exp i -> Signal (Bits n) -> exp UBits
      -> ArrayCMD (Param3 prog exp pred) ()

instance HFunctor ArrayCMD 
  where
    hfmap _ (GetRange l h s)   = GetRange l h s
    hfmap _ (SetRange l h s e) = SetRange l h s e

instance HBifunctor ArrayCMD
  where
    hbimap _ f (GetRange r1 r2 s)   = GetRange (f r1) (f r2) s
    hbimap _ f (SetRange r1 r2 s e) = SetRange (f r1) (f r2) s (f e)

instance (ArrayCMD :<: instr) => Reexpressible ArrayCMD instr
  where
    reexpressInstrEnv reexp (GetRange r1 r2 s) =
      do r1' <- reexp r1
         r2' <- reexp r2
         lift $ singleInj $ GetRange r1' r2' s
    reexpressInstrEnv reexp (SetRange r1 r2 s e) =
      do r1' <- reexp r1
         r2' <- reexp r2
         e'  <- reexp e
         lift $ singleInj $ SetRange r1' r2' s e'

--------------------------------------------------------------------------------
-- ** Virtual arrays.

-- | Virtual array reprensentation.
data VArray i a = VArrayC VarId | VArrayE (IORef (IOArray i a))

-- | Immutable arrays.
data IArray i a = IArrayC VarId | IArrayE (Arr.Array i a)

-- | Commands for variable arrays.
data VArrayCMD fs a
  where
    -- ^ Creates an array of given length.
    NewVArray :: (pred a, Integral i, Ix i) => Name -> exp i -> VArrayCMD (Param3 prog exp pred) (VArray i a)
    -- ^ Creates an array from the given list of elements.
    InitVArray :: (pred a, Integral i, Ix i) => Name -> [a] -> VArrayCMD (Param3 prog exp pred) (VArray i a)
    -- ^ Fetches the array's value at a specified index.
    GetVArray :: (pred a, Integral i, Ix i) => exp i -> VArray i a -> VArrayCMD (Param3 prog exp pred) (Val a)
    -- ^ Writes a value to an array at some specified index.
    SetVArray :: (pred a, Integral i, Ix i) => exp i -> exp a -> VArray i a -> VArrayCMD (Param3 prog exp pred) ()
    -- ^ ...
    CopyVArray:: (pred a, Integral i, Ix i) => VArray i a -> VArray i a -> exp i -> VArrayCMD (Param3 prog exp pred) ()
    -- ^ Unsafe version of fetching an array's value.
    UnsafeFreezeVArray :: (pred a, Integral i, Ix i) => VArray i a -> VArrayCMD (Param3 prog exp pred) (IArray i a)
    -- ^ ...
    UnsafeThawVArray :: (pred a, Integral i, Ix i) => IArray i a -> VArrayCMD (Param3 prog exp pred) (VArray i a)

instance HFunctor VArrayCMD
  where
    hfmap _ (NewVArray n i) = NewVArray n i
    hfmap _ (InitVArray n is) = InitVArray n is
    hfmap _ (GetVArray i a) = GetVArray i a
    hfmap _ (SetVArray i e a) = SetVArray i e a
    hfmap _ (CopyVArray a b l) = CopyVArray a b l
    hfmap _ (UnsafeFreezeVArray a) = UnsafeFreezeVArray a
    hfmap _ (UnsafeThawVArray a) = UnsafeThawVArray a

instance HBifunctor VArrayCMD
  where
    hbimap _ f (NewVArray n i) = NewVArray n (f i)
    hbimap _ _ (InitVArray n is) = InitVArray n is
    hbimap _ f (GetVArray i a) = GetVArray (f i) a
    hbimap _ f (SetVArray i e a) = SetVArray (f i) (f e) a
    hbimap _ f (CopyVArray a b l) = CopyVArray a b (f l)
    hbimap _ _ (UnsafeFreezeVArray a) = UnsafeFreezeVArray a
    hbimap _ _ (UnsafeThawVArray a) = UnsafeThawVArray a

instance (VArrayCMD :<: instr) => Reexpressible VArrayCMD instr
  where
    reexpressInstrEnv reexp (NewVArray n i) = lift . singleInj . NewVArray n =<< reexp i
    reexpressInstrEnv reexp (InitVArray n is) = lift $ singleInj $ InitVArray n is
    reexpressInstrEnv reexp (GetVArray i a) = do i' <- reexp i; lift $ singleInj $ GetVArray i' a
    reexpressInstrEnv reexp (SetVArray i e a) = do i' <- reexp i; e' <- reexp e; lift $ singleInj $ SetVArray i' e' a
    reexpressInstrEnv reexp (CopyVArray a b l) = lift . singleInj . CopyVArray a b =<< reexp l
    reexpressInstrEnv reexp (UnsafeFreezeVArray a) = lift $ singleInj $ UnsafeFreezeVArray a
    reexpressInstrEnv reexp (UnsafeThawVArray a) = lift $ singleInj $ UnsafeThawVArray a

--------------------------------------------------------------------------------
-- ** Looping.

-- | Commands for looping constructs.
data LoopCMD fs a
  where
    -- ^ Creates a new for loop.
    For   :: (pred n, Integral n) => exp n -> (Val n -> prog ()) -> LoopCMD (Param3 prog exp pred) ()
    -- ^ Creates a new while loop.
    While :: prog (exp Bool) -> prog () -> LoopCMD (Param3 prog exp pred) ()

instance HFunctor LoopCMD
  where
    hfmap f (For r step)      = For r (f . step)
    hfmap f (While cont step) = While (f cont) (f step)

instance HBifunctor LoopCMD
  where
    hbimap g f (For r step) = For (f r) (g . step)
    hbimap g f (While cont step) = While (g $ fmap f cont) (g step)

instance (LoopCMD :<: instr) => Reexpressible LoopCMD instr
  where
    reexpressInstrEnv reexp (For r step) = do
      r' <- reexp r
      ReaderT $ \env -> singleInj $
        For r' (flip runReaderT env . step)
    reexpressInstrEnv reexp (While cont step) = do
      ReaderT $ \env -> singleInj $
        While (runReaderT (cont >>= reexp) env)
              (runReaderT step env)

--------------------------------------------------------------------------------
-- ** Conditional statements.

-- | ...
data When a prog = When (Constraint a) (prog ())

-- | ...
data Constraint a where
  Is :: Eq a  => a      -> Constraint a
  To :: Ord a => a -> a -> Constraint a

-- | Commnads for conditional statements.
data ConditionalCMD fs a
  where
    -- ^ ...
    If :: (exp Bool, prog ()) -> [(exp Bool, prog ())] -> Maybe (prog ()) -> ConditionalCMD (Param3 prog exp pred) ()
    -- ^ ...
    Case :: pred a => exp a -> [When a prog] -> Maybe (prog ()) -> ConditionalCMD (Param3 prog exp pred) ()
    -- ^ ...
    Null :: ConditionalCMD (Param3 prog exp pred) ()
    -- ***
    WhenRising :: pred Bool => Signal Bool -> prog () -> ConditionalCMD (Param3 prog exp pred) ()

instance HFunctor ConditionalCMD
  where
    hfmap f (If   a cs b) = If (fmap f a) (fmap (fmap f) cs) (fmap f b)
    hfmap f (Case e xs d) = Case e (fmap (wmap f) xs) (fmap f d)
      where wmap f (When a p) = When a (f p)
    hfmap _ (Null)        = Null
    -- *** ...
    hfmap f (WhenRising s p) = WhenRising s (f p)

instance HBifunctor ConditionalCMD
  where
    hbimap g f (If a cs b) = If (pmap a) (fmap pmap cs) (fmap g b)
      where pmap (x, y) = (f x, g y)
    hbimap g f (Case e xs d) = Case (f e) (fmap wmap xs) (fmap g d)
      where wmap (When a p) = When a (g p)
    hbimap _ _ (Null) = Null
    -- *** ...
    hbimap g f (WhenRising s p) = WhenRising s (g p)

instance (ConditionalCMD :<: instr) => Reexpressible ConditionalCMD instr
  where
    reexpressInstrEnv reexp (If (c, a) cs b) =
      do let (xs, ys) = unzip cs
         c'  <-      reexp c
         xs' <- mapM reexp xs
         ReaderT $ \env ->
           let ys' = fmap (flip runReaderT env) ys
           in singleInj $
             If (c', runReaderT a env)
                (zip xs' ys')
                (fmap (flip runReaderT env) b)
    reexpressInstrEnv reexp (Case c cs d) =
      do let (xs, ys) = unzipWhen cs
         c' <- reexp c
         ReaderT $ \env ->
           let ys' = fmap (flip runReaderT env) ys
           in singleInj $
             Case c'
               (zipWhen xs ys')
               (fmap (flip runReaderT env) d)
    reexpressInstrEnv reexp (Null) = lift $ singleInj $ Null
    -- ***
    reexpressInstrEnv reexp (WhenRising n p) =
      ReaderT $ \env -> singleInj $ WhenRising n $ runReaderT p env

unzipWhen :: [When a p] -> ([Constraint a], [p ()])
unzipWhen = unzip . fmap (\(When a p) -> (a, p))

zipWhen   :: [Constraint a] -> [p ()] -> [When a p]
zipWhen x y = fmap (\(a, p) -> When a p) $ zip x y

--------------------------------------------------------------------------------
-- ** Components.

-- | Signature description.
data Signature fs a
  where
    Ret :: prog () -> Signature (Param3 prog exp pred) ()
    Lam :: pred a
      => Name
      -> Mode
      -> (Signal a -> Signature (Param3 prog exp pred) b)
      -> Signature (Param3 prog exp pred) (Signal a -> b)

-- | Signature arguments.
data Arg a
  where
    Nill      :: Arg ()
    ArgSignal :: Signal a -> Arg b -> Arg (Signal a -> b)

-- | ...
(.>) :: Signal a -> Arg b -> Arg (Signal a -> b)
(.>) = ArgSignal

infixr .>

instance HFunctor Signature
  where
    hfmap f (Ret m)       = Ret (f m)
    hfmap f (Lam n m sig) = Lam n m (hfmap f . sig)

instance HBifunctor Signature
  where
    hbimap g f (Ret m)       = Ret (g m)
    hbimap g f (Lam n m sig) = Lam n m (hbimap g f . sig)

--------------------------------------------------------------------------------

-- | Named components.
data Component fs a = Component Name (Signature fs a)

-- | Commands for generating stand-alone components and calling them.
data ComponentCMD fs a
  where
    -- ^ ...
    StructComponent
      :: Name -> Signature (Param3 prog exp pred) a
      -> ComponentCMD (Param3 prog exp pred) Name
    -- ^ ...
    PortMap
      :: Component (Param3 prog exp pred) a -> Arg a
      -> ComponentCMD (Param3 prog exp pred) ()

instance HFunctor ComponentCMD
  where
    hfmap f (StructComponent n sig)        = StructComponent n (hfmap f sig)
    hfmap f (PortMap (Component m sig) as) = PortMap (Component m (hfmap f sig)) as

instance HBifunctor ComponentCMD
  where
    hbimap g f (StructComponent n sig)        = StructComponent n (hbimap g f sig)
    hbimap g f (PortMap (Component m sig) as) = PortMap (Component m (hbimap g f sig)) as

instance (ComponentCMD :<: instr) => Reexpressible ComponentCMD instr
  where
    reexpressInstrEnv reexp (StructComponent n sig) =
      ReaderT $ \env ->
        singleInj $ StructComponent n (reexpressSignature env sig)
    reexpressInstrEnv reexp (PortMap (Component m sig) as) =
      ReaderT $ \env ->
        singleInj $ PortMap (Component m (reexpressSignature env sig)) as

reexpressSignature
  :: env
  -> Signature (Param3 (ReaderT env (ProgramT instr (Param2 exp2 pred) m)) exp1 pred) a
  -> Signature (Param3              (ProgramT instr (Param2 exp2 pred) m)  exp2 pred) a
reexpressSignature env (Ret prog)   = Ret (runReaderT prog env)
reexpressSignature env (Lam n m sf) = Lam n m (reexpressSignature env . sf)

--------------------------------------------------------------------------------
-- ** Structural entities.

data Ident = Ident VarId

class    ToIdent a            where toIdent :: a -> Ident
instance ToIdent (Val      a) where toIdent (ValC      i) = Ident i
instance ToIdent (Signal   a) where toIdent (SignalC   i) = Ident i
instance ToIdent (Variable a) where toIdent (VariableC i) = Ident i
instance ToIdent (Constant a) where toIdent (ConstantC i) = Ident i
instance ToIdent (Array  i a) where toIdent (ArrayC    i) = Ident i
instance ToIdent (VArray i a) where toIdent (VArrayC   i) = Ident i

-- | Construct the untyped signal list for processes.
(.:) :: ToIdent a => a -> [Ident] -> [Ident]
(.:) x xs = toIdent x : xs

infixr .:

-- | Commands for structural entities.
data StructuralCMD fs (a :: *)
  where
    -- ^ Wraps the program in an entity.
    StructEntity
      :: Name -> prog a -> StructuralCMD (Param3 prog exp pred) a
    -- ^ Wraps the program in an architecture.
    StructArchitecture
      :: Name -> Name -> prog a -> StructuralCMD (Param3 prog exp pred) a
    -- ^ Wraps the program in a process.
    StructProcess
      :: [Ident] -> prog () -> StructuralCMD (Param3 prog exp pred) ()

instance HFunctor StructuralCMD
  where
    hfmap f (StructEntity e p)         = StructEntity e (f p)
    hfmap f (StructArchitecture e a p) = StructArchitecture e a (f p)
    hfmap f (StructProcess xs p)       = StructProcess xs (f p)

instance HBifunctor StructuralCMD
  where
    hbimap g f (StructEntity e p)         = StructEntity e (g p)
    hbimap g f (StructArchitecture e a p) = StructArchitecture e a (g p)
    hbimap g f (StructProcess xs p)       = StructProcess xs (g p)

instance (StructuralCMD :<: instr) => Reexpressible StructuralCMD instr
  where
    reexpressInstrEnv reexp (StructEntity n p)         =
      ReaderT $ \env -> singleInj $ StructEntity n $ runReaderT p env
    reexpressInstrEnv reexp (StructArchitecture e n p) =
      ReaderT $ \env -> singleInj $ StructArchitecture e n $ runReaderT p env
    reexpressInstrEnv reexp (StructProcess is p)       =
      ReaderT $ \env -> singleInj $ StructProcess is $ runReaderT p env

--------------------------------------------------------------------------------
-- ** VHDL.
{-
class Argument arg pred
  where
    mkArg   :: arg pred -> VHDL V.Expression
    mkParam :: arg pred -> V.Identifier

data FunArg exp pred
  where
    SignalArg :: pred a => Signal a -> FunArg exp pred

data VHDL_CMD fs a
  where
    CallFun :: pred a => String -> [FunArg exp pred] -> VHDL_CMD (Param3 prog exp pred) (Val a)

instance HFunctor VHDL_CMD
  where
    hfmap _ (CallFun n as) = CallFun n as

instance HBifunctor VHDL_CMD
  where
    hbimap _ f (CallFun n as) = CallFun n (map (mapFunArg f) as)

instance (VHDL_CMD :<: instr) => Reexpressible VHDL_CMD instr
  where
    reexpressInstrEnv reexp (CallFun n as) = lift . singleInj . CallFun n =<< mapM (mapFunArgM reexp) as

mapFunArg :: (forall a. exp1 a -> exp2 a) -> FunArg exp1 pred -> FunArg exp2 pred
mapFunArg f (SignalArg s) = SignalArg s

mapFunArgM :: Monad m => (forall a. exp1 a -> m (exp2 a)) -> FunArg exp1 pred -> m (FunArg exp2 pred)
mapFunArgM f (SignalArg s) = return $ SignalArg s
-}
--------------------------------------------------------------------------------

