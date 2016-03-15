{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Embedded.Hardware.Command.CMD where

import Language.Embedded.VHDL               (Mode)
import Language.Embedded.Hardware.Interface (PredicateExp, VarId)
import Language.Embedded.Hardware.Expression.Represent.Bit

import Control.Monad.Operational.Higher

import Data.Ix       (Ix)
import Data.IORef    (IORef)
import Data.Array.IO (IOArray)
import qualified Data.Array as Arr

import GHC.TypeLits

--------------------------------------------------------------------------------
-- * Hardware commands.
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** Signals.

-- | Signal representation.
data Signal a = SignalC VarId | SignalE (IORef a)

-- | Commands for signals.
data SignalCMD (exp :: * -> *) (prog :: * -> *) a
  where
    -- ^ Create a new signal.
    NewSignal :: PredicateExp exp a => VarId -> Mode -> Maybe (exp a) -> SignalCMD exp prog (Signal a)
    -- ^ Fetch the contents of a signal.
    GetSignal :: PredicateExp exp a => Signal a -> SignalCMD exp prog (exp a)
    -- ^ Write the value to a signal.
    SetSignal :: PredicateExp exp a => Signal a -> exp a -> SignalCMD exp prog ()
    -- ^ Unsafe version of fetching a signal.
    UnsafeFreezeSignal :: PredicateExp exp a => Signal a -> SignalCMD exp prog (exp a)

    -- *** Move to ArrayCMD *** ---
    SetOthers :: PredicateExp exp Bool =>
      Signal a -> exp Bool -> SignalCMD exp prog ()
    GetIndex :: (PredicateExp exp a, PredicateExp exp i, Integral i, Ix i) =>
      Signal a -> exp i -> SignalCMD exp prog (exp a)
    GetSlice :: (PredicateExp exp a, PredicateExp exp b, PredicateExp exp i, Integral i, Ix i) =>
      Signal a -> exp i -> exp i -> SignalCMD exp prog (Variable b)
    --------------------------------

type instance IExp (SignalCMD e)       = e
type instance IExp (SignalCMD e :+: i) = e

instance HFunctor (SignalCMD exp)
  where
    hfmap _ (NewSignal n m e)      = NewSignal n m e
    hfmap _ (GetSignal s)          = GetSignal s
    hfmap _ (SetSignal s e)        = SetSignal s e
    hfmap _ (UnsafeFreezeSignal s) = UnsafeFreezeSignal s
    -- ....
    hfmap _ (SetOthers s b)        = SetOthers s b
    hfmap _ (GetIndex  s i)        = GetIndex  s i
    hfmap _ (GetSlice  s l u)      = GetSlice  s l u

--------------------------------------------------------------------------------
-- ** Variables.

-- | Variable representation.
data Variable a = VariableC VarId | VariableE (IORef a)

-- | Commands for variables.
data VariableCMD (exp :: * -> *) (prog :: * -> *) a
  where
    -- ^ Create a new variable.
    NewVariable :: PredicateExp exp a => VarId -> Maybe (exp a) -> VariableCMD exp prog (Variable a)
    -- ^ Fetch the contents of a variable.
    GetVariable :: PredicateExp exp a => Variable a -> VariableCMD exp prog (exp a)
    -- ^ Write the value to a variable.
    SetVariable :: PredicateExp exp a => Variable a -> exp a -> VariableCMD exp prog ()
    -- ^ Unsafe version of fetching a variable.
    UnsafeFreezeVariable :: PredicateExp exp a => Variable a -> VariableCMD exp prog (exp a)

type instance IExp (VariableCMD e)       = e
type instance IExp (VariableCMD e :+: i) = e

instance HFunctor (VariableCMD exp)
  where
    hfmap _ (NewVariable n e)        = NewVariable n e
    hfmap _ (GetVariable s)          = GetVariable s
    hfmap _ (SetVariable s e)        = SetVariable s e
    hfmap _ (UnsafeFreezeVariable s) = UnsafeFreezeVariable s

--------------------------------------------------------------------------------
-- ** Constants.

-- | Constant representation.
data Constant a = ConstantC VarId | ConstantE a

-- | Commands for constants.
data ConstantCMD (exp :: * -> *) (prog :: * -> *) a
  where
    -- ^ Create a new constant.
    NewConstant :: PredicateExp exp a => VarId -> exp a -> ConstantCMD exp prog (Constant a)
    -- ^ Fetch the value of a constant.
    GetConstant :: PredicateExp exp a => Constant a -> ConstantCMD exp prog (exp a)

type instance IExp (ConstantCMD e)       = e
type instance IExp (ConstantCMD e :+: i) = e

instance HFunctor (ConstantCMD exp)
  where
    hfmap _ (NewConstant v e) = NewConstant v e
    hfmap _ (GetConstant c)   = GetConstant c

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
data ArrayCMD (exp :: * -> *) (prog :: * -> *) a
  where
    NewArray
      :: ArrayCMD exp prog (Array i Bool)
    
type instance IExp (ArrayCMD e)       = e
type instance IExp (ArrayCMD e :+: i) = e

instance HFunctor (ArrayCMD exp)
  where
    hfmap _ _ = undefined

--------------------------------------------------------------------------------
-- ** Virtual arrays.

-- | Virtual array reprensentation.
data VArray i a = VArrayC VarId | VArrayE (IORef (IOArray i a))

-- | Immutable arrays.
data IArray i a = IArrayC VarId | IArrayE (Arr.Array i a)

-- | Commands for variable arrays.
data VArrayCMD (exp :: * -> *) (prog :: * -> *) a
  where
    -- ^ Creates an array of given length.
    NewVArray
      :: ( PredicateExp exp a
         , PredicateExp exp i
         , Integral i
         , Ix i)
      => VarId -> exp i -> VArrayCMD exp prog (VArray i a)
    -- ^ Creates an array from the given list of elements.
    InitVArray
      :: ( PredicateExp exp a
         , PredicateExp exp i
         , Integral i
         , Ix i)
      => VarId -> [a] -> VArrayCMD exp prog (VArray i a)
    -- ^ Fetches the array's value at a specified index.
    GetVArray
      :: ( PredicateExp exp a
         , PredicateExp exp i
         , Integral i
         , Ix i)
      => exp i -> VArray i a -> VArrayCMD exp prog (exp a)
    -- ^ Writes a value to an array at some specified index.
    SetVArray
      :: ( PredicateExp exp a
         , PredicateExp exp i
         , Integral i
         , Ix i)
      => exp i -> exp a -> VArray i a -> VArrayCMD exp prog ()
    CopyVArray
      :: ( PredicateExp exp a
         , PredicateExp exp i
         , Integral i
         , Ix i)
      => VArray i a -> VArray i a -> exp i -> VArrayCMD exp prog ()
    -- ^ Unsafe version of fetching an array's value.
    UnsafeFreezeVArray
      :: ( PredicateExp exp a
         , PredicateExp exp i
         , Integral i
         , Ix i)
      => VArray i a -> VArrayCMD exp prog (IArray i a)

type instance IExp (VArrayCMD e)       = e
type instance IExp (VArrayCMD e :+: i) = e

instance HFunctor (VArrayCMD exp)
  where
    hfmap _ (NewVArray n i)        = NewVArray n i
    hfmap _ (InitVArray n is)      = InitVArray n is
    hfmap _ (GetVArray i a)        = GetVArray i a
    hfmap _ (SetVArray i e a)      = SetVArray i e a
    hfmap _ (CopyVArray a b l)     = CopyVArray a b l
    hfmap _ (UnsafeFreezeVArray a) = UnsafeFreezeVArray a

--------------------------------------------------------------------------------
-- ** Looping.

-- | Commands for looping constructs.
data LoopCMD (exp :: * -> *) (prog :: * -> *) a
  where
    -- ^ Creates a new for loop.
    For   :: (PredicateExp exp n, Integral n) => exp n -> (exp n -> prog ()) -> LoopCMD exp prog ()
    -- ^ Creates a new while loop.
    While :: PredicateExp exp Bool => prog (exp Bool) -> prog () -> LoopCMD exp prog ()

type instance IExp (LoopCMD e)       = e
type instance IExp (LoopCMD e :+: i) = e

instance HFunctor (LoopCMD exp)
  where
    hfmap f (For r step)      = For r (f . step)
    hfmap f (While cont step) = While (f cont) (f step)

--------------------------------------------------------------------------------
-- ** Conditional statements.

-- | ...
data When a prog = When (Constraint a) (prog ())

-- | ...
data Constraint a
       = Is a
       | To a a

-- | Commnads for conditional statements.
data ConditionalCMD (exp :: * -> *) (prog :: * -> *) a
  where
    If :: PredicateExp exp Bool =>
      (exp Bool, prog ())   ->
      [(exp Bool, prog ())] ->
      Maybe (prog ())       ->
      ConditionalCMD exp prog ()

    Case :: (PredicateExp exp a, Eq a, Ord a) =>
      exp a ->
      [When a prog] ->
      Maybe (prog ()) ->
      ConditionalCMD exp prog ()

    Null :: ConditionalCMD exp prog ()

type instance IExp (ConditionalCMD e)       = e
type instance IExp (ConditionalCMD e :+: i) = e

instance HFunctor (ConditionalCMD exp)
  where
    hfmap f (If   a cs b) = If (fmap f a) (fmap (fmap f) cs) (fmap f b)
    hfmap f (Case e xs d) = Case e (fmap (wmap f) xs) (fmap f d)
      where wmap f (When a p) = When a (f p)
    hfmap _ (Null)        = Null

--------------------------------------------------------------------------------
-- ** Components.

-- | Processes.
data Component exp m a = Component (Maybe VarId) (Sig exp m a)

-- | Signature declaring type of processes.
data Sig exp m a
  where
    -- ^ Fully applied program, all signals are passed in a 'pointer' style.
    --   The monad `m` is implicit on all returns.
    Unit :: m () -> Sig exp m ()
    
    -- ^ ...
    Lam  :: PredicateExp exp a
      => VarId
      -> Mode
      -> (Signal a -> Sig exp m b)
      -> Sig exp m (Signal a -> b)

-- | Arguments for a signature.
data Arg a
  where
    Nill :: Arg ()
    (:>) :: Signal a -> Arg b -> Arg (Signal a -> b)

infixr :>

-- | Commands for generating stand-alone components and calling them.
data ComponentCMD (exp :: * -> *) (prog :: * -> *) a
  where
    -- ^ ...
    StructComponent :: VarId -> Sig exp prog a -> ComponentCMD exp prog (Maybe VarId)
    -- ^ ...
    PortMap         :: Component exp prog a -> Arg a -> ComponentCMD exp prog ()

type instance IExp (ComponentCMD e)       = e
type instance IExp (ComponentCMD e :+: i) = e

instance HFunctor (Sig exp)
  where
    hfmap f (Unit m)     = Unit $ f m
    hfmap f (Lam  n m g) = Lam n m (hfmap f . g)

instance HFunctor (ComponentCMD exp)
  where
    hfmap f (StructComponent n sig)        = StructComponent n (hfmap f sig)
    hfmap f (PortMap (Component m sig) as) = PortMap (Component m (hfmap f sig)) as

--------------------------------------------------------------------------------
-- ** Structural entities.

data Ident = Ident VarId

class    ToIdent a where toIdent :: a -> Ident
instance ToIdent (Signal a)   where toIdent (SignalC i)   = Ident i
instance ToIdent (Variable a) where toIdent (VariableC i) = Ident i
instance ToIdent (Array i a)  where toIdent (ArrayC i)    = Ident i
instance ToIdent (VArray i a) where toIdent (VArrayC i)   = Ident i

-- | Construct the untyped signal list for processes.
(.:) :: ToIdent a => a -> [Ident] -> [Ident]
(.:) x xs = toIdent x : xs

infixr .:

-- | Commands for structural entities.
data StructuralCMD (exp :: * -> *) (prog :: * -> *) a
  where
    -- ^ Wraps the program in an entity.
    StructEntity
      :: VarId -> prog a -> StructuralCMD exp prog a
    -- ^ Wraps the program in an architecture.
    StructArchitecture
      :: VarId -> VarId -> prog a -> StructuralCMD exp prog a
    -- ^ Wraps the program in a process.
    StructProcess
      :: [Ident] -> prog () -> StructuralCMD exp prog ()

type instance IExp (StructuralCMD e)       = e
type instance IExp (StructuralCMD e :+: i) = e

instance HFunctor (StructuralCMD exp)
  where
    hfmap f (StructEntity e p)         = StructEntity e (f p)
    hfmap f (StructArchitecture e a p) = StructArchitecture e a (f p)
    hfmap f (StructProcess xs p)       = StructProcess xs (f p)

--------------------------------------------------------------------------------
-- ...

data IntegerCMD (exp :: * -> *) (prog :: * -> *) a
  where
    SignalInteger
      :: (PredicateExp exp a, Num a)
      => VarId -> Maybe (a, a) -> IntegerCMD exp prog (Signal Integer)
    ToInteger
      :: ( PredicateExp exp a
         , PredicateExp exp i
         , Integral i
         , Ix i
         , Num a)
      => exp i -> exp i -> Array i a -> IntegerCMD exp prog (exp Integer)

instance HFunctor (IntegerCMD exp)
  where
    hfmap _ (SignalInteger v m) = SignalInteger v m
    hfmap _ (ToInteger l h a)   = ToInteger l h a

--------------------------------------------------------------------------------

