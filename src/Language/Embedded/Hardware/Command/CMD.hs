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

import GHC.TypeLits

--------------------------------------------------------------------------------
-- * Hardware commands.
--------------------------------------------------------------------------------

-- | ...
swapM :: Monad m => Maybe (m a) -> m (Maybe a)
swapM = maybe (return Nothing) (>>= return . Just)

--------------------------------------------------------------------------------
-- ** Values.

-- | Value representation.
data Val a = ValC VarId | ValE a

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
    NewSignal :: pred a => VarId -> Mode -> Maybe (exp a) -> SignalCMD (Param3 prog exp pred) (Signal a)
    -- ^ Fetch the contents of a signal.
    GetSignal :: pred a => Signal a -> SignalCMD (Param3 prog exp pred) (Val a)
    -- ^ Write the value to a signal.
    SetSignal :: pred a => Signal a -> exp a -> SignalCMD (Param3 prog exp pred) ()
    -- ^ Unsafe version of fetching a signal.
    UnsafeFreezeSignal :: pred a => Signal a -> SignalCMD (Param3 prog exp pred) (Val a)

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
    NewVariable :: pred a => VarId -> Maybe (exp a) -> VariableCMD (Param3 prog exp pred) (Variable a)
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
    NewConstant :: pred a => VarId -> exp a -> ConstantCMD (Param3 prog exp pred) (Constant a)
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
data Array i a = ArrayC VarId | ArrayE (IORef (IOArray i a))

-- | Commands for signal arrays.
data ArrayCMD fs a

instance HFunctor ArrayCMD 
  where
    hfmap _ _ = undefined

instance HBifunctor ArrayCMD
  where
    hbimap _ _ _ = undefined

instance (ArrayCMD :<: instr) => Reexpressible ArrayCMD instr
  where
    reexpressInstrEnv _ _ = undefined

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
    NewVArray :: (pred a, Integral i, Ix i) => VarId -> exp i -> VArrayCMD (Param3 prog exp pred) (VArray i a)
    -- ^ Creates an array from the given list of elements.
    InitVArray :: (pred a, Integral i, Ix i) => VarId -> [a] -> VArrayCMD (Param3 prog exp pred) (VArray i a)
    -- ^ Fetches the array's value at a specified index.
    GetVArray :: (pred a, Integral i, Ix i) => exp i -> VArray i a -> VArrayCMD (Param3 prog exp pred) (Val a)
    -- ^ Writes a value to an array at some specified index.
    SetVArray :: (pred a, Integral i, Ix i) => exp i -> exp a -> VArray i a -> VArrayCMD (Param3 prog exp pred) ()
    -- ^ ...
    CopyVArray:: (pred a, Integral i, Ix i) => VArray i a -> VArray i a -> exp i -> VArrayCMD (Param3 prog exp pred) ()
    -- ^ Unsafe version of fetching an array's value.
    UnsafeFreezeVArray :: (pred a, Integral i, Ix i) => VArray i a -> VArrayCMD (Param3 prog exp pred) (IArray i a)
    -- ^ ...
    UnsafeThawVArray :: (pred a, Integral i, Ix i) => IArray i a -> VArrayCMD (Param3 prog exp pred) (Array i a)

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
data Constraint a = Is a | To a a

-- | Commnads for conditional statements.
data ConditionalCMD fs a
  where
    -- ^ ...
    If :: (exp Bool, prog ()) -> [(exp Bool, prog ())] -> Maybe (prog ()) -> ConditionalCMD (Param3 prog exp pred) ()
    -- ^ ...
    Case :: pred a => exp a -> [When a prog] -> Maybe (prog ()) -> ConditionalCMD (Param3 prog exp pred) ()
    -- ^ ...
    Null :: ConditionalCMD (Param3 prog exp pred) ()

instance HFunctor ConditionalCMD
  where
    hfmap f (If   a cs b) = If (fmap f a) (fmap (fmap f) cs) (fmap f b)
    hfmap f (Case e xs d) = Case e (fmap (wmap f) xs) (fmap f d)
      where wmap f (When a p) = When a (f p)
    hfmap _ (Null)        = Null

instance HBifunctor ConditionalCMD
  where
    hbimap g f (If a cs b) = If (pmap a) (fmap pmap cs) (fmap g b)
      where pmap (x, y) = (f x, g y)
    hbimap g f (Case e xs d) = Case (f e) (fmap wmap xs) (fmap g d)
      where wmap (When a p) = When a (g p)
    hbimap _ _ (Null) = Null

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

unzipWhen :: [When a p] -> ([Constraint a], [p ()])
unzipWhen = unzip . fmap (\(When a p) -> (a, p))

zipWhen   :: [Constraint a] -> [p ()] -> [When a p]
zipWhen x y = fmap (\(a, p) -> When a p) $ zip x y

--------------------------------------------------------------------------------
-- ** Components.

-- | Processes.
data Component fs a = Component (Maybe VarId) (Sig fs a)

-- | Signature declaring type of processes.
data Sig fs a
  where
    -- ^ Fully applied program, all signals are passed in a 'pointer' style.
    --   The monad `m` is implicit on all returns.
    Unit :: prog () -> Sig (Param3 prog exp pred) ()
    
    -- ^ ...
    Lam  :: pred a
      => VarId
      -> Mode
      -> (Signal a -> Sig (Param3 prog exp pred) b)
      -> Sig (Param3 prog exp pred) (Signal a -> b)

-- | Arguments for a signature.
data Arg a
  where
    Nill :: Arg ()
    (:>) :: Signal a -> Arg b -> Arg (Signal a -> b)

infixr :>

-- | Commands for generating stand-alone components and calling them.
data ComponentCMD fs a
  where
    -- ^ ...
    StructComponent
      :: VarId
      -> Sig (Param3 prog exp pred) a
      -> ComponentCMD (Param3 prog exp pred) (Maybe VarId)
    -- ^ ...
    PortMap
      :: Component (Param3 prog exp pred) a
      -> Arg a
      -> ComponentCMD (Param3 prog exp pred) ()

instance HFunctor Sig
  where
    hfmap f (Unit m)     = Unit $ f m
    hfmap f (Lam  n m g) = Lam n m (hfmap f . g)

instance HBifunctor Sig
  where
    hbimap g f (Unit m)    = Unit $ g m
    hbimap g f (Lam n m h) = Lam n m (hbimap g f . h)

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
    reexpressInstrEnv reexp (StructComponent n sig) = error "!"
{-
    ReaderT $ \env ->
      do let sig' = deep env sig
         singleInj $ StructComponent n sig'
      where
        deep :: env -> Sig (Param3 prog exp pred) a -> Sig (Param3 exp pred x) a
        deep env sig = case sig of
          (Unit m) -> Unit $ runReaderT m env
-}
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
--
-- *** Param exp?
data StructuralCMD fs a
  where
    -- ^ Wraps the program in an entity.
    StructEntity
      :: VarId -> prog a -> StructuralCMD (Param2 prog exp) a
    -- ^ Wraps the program in an architecture.
    StructArchitecture
      :: VarId -> VarId -> prog a -> StructuralCMD (Param2 prog exp) a
    -- ^ Wraps the program in a process.
    StructProcess
      :: [Ident] -> prog () -> StructuralCMD (Param2 prog exp) ()

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

--------------------------------------------------------------------------------

