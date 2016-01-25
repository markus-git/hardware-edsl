{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Embedded.Hardware.Command.CMD where

import Language.Embedded.VHDL               (Mode)
import Language.Embedded.Hardware.Interface (PredicateExp)

import Control.Monad.Operational.Higher

import Data.Ix       (Ix)
import Data.IORef    (IORef)
import Data.Array.IO (IOArray)

--------------------------------------------------------------------------------
-- * Hardware commands.
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** Signals.

-- | If a signal is declared with a scope of 'Entity' its classified as either a
--   port or generic signal.
data Clause   = Port | Generic
  deriving (Show)

-- | Scope of a signal.
data Scope    = SProcess | SArchitecture | SEntity
  deriving (Show)

-- | Signal representation.
data Signal a = SignalC Integer | SignalE (IORef a)

-- | Commands for signals.
data SignalCMD (exp :: * -> *) (prog :: * -> *) a
  where
    -- ^ Create a new signal.
    NewSignal :: PredicateExp exp a => Clause -> Scope -> Mode  -> Maybe (exp a) -> SignalCMD exp prog (Signal a)
    -- ^ Fetch the contents of a signal.
    GetSignal :: PredicateExp exp a => Signal a          -> SignalCMD exp prog (exp a)
    -- ^ Write the value to a signal.
    SetSignal :: PredicateExp exp a => Signal a -> exp a -> SignalCMD exp prog ()
    -- ^ Unsafe version of fetching a signal.
    UnsafeFreezeSignal :: PredicateExp exp a => Signal a -> SignalCMD exp prog (exp a)

type instance IExp (SignalCMD e)       = e
type instance IExp (SignalCMD e :+: i) = e

instance HFunctor (SignalCMD exp)
  where
    hfmap _ (NewSignal c s m e) = NewSignal c s m e
    hfmap _ (GetSignal s)       = GetSignal s
    hfmap _ (SetSignal s e)     = SetSignal s e
    hfmap _ (UnsafeFreezeSignal s) = UnsafeFreezeSignal s

--------------------------------------------------------------------------------
-- ** Variables.

-- | Variable representation.
data Variable a = VariableC Integer | VariableE (IORef a)

-- | Commands for variables.
data VariableCMD (exp :: * -> *) (prog :: * -> *) a
  where
    -- ^ Create a new variable.
    NewVariable :: PredicateExp exp a => Maybe (exp a) -> VariableCMD exp prog (Variable a)
    -- ^ Fetch the contents of a variable.
    GetVariable :: PredicateExp exp a => Variable a          -> VariableCMD exp prog (exp a)
    -- ^ Write the value to a variable.
    SetVariable :: PredicateExp exp a => Variable a -> exp a -> VariableCMD exp prog ()
    -- ^ Unsafe version of fetching a variable.
    UnsafeFreezeVariable :: PredicateExp exp a => Variable a -> VariableCMD exp prog (exp a)

type instance IExp (VariableCMD e)       = e
type instance IExp (VariableCMD e :+: i) = e

instance HFunctor (VariableCMD exp)
  where
    hfmap _ (NewVariable e)   = NewVariable e
    hfmap _ (GetVariable s)   = GetVariable s
    hfmap _ (SetVariable s e) = SetVariable s e
    hfmap _ (UnsafeFreezeVariable s) = UnsafeFreezeVariable s

--------------------------------------------------------------------------------
-- ** Arrays.

-- | Expression types that support compilation of array indexing
class CompArrayIx exp
  where
    -- | Generate code for an array indexing operation
    compArrayIx :: PredicateExp exp a => exp i -> Array i a -> Maybe (exp a)
    compArrayIx _ _ = Nothing

-- | Array reprensentation.
data Array i a = ArrayC Integer | ArrayE (IORef (IOArray i a))

-- | Commands for arrays.
data ArrayCMD (exp :: * -> *) (prog :: * -> *) a
  where
    -- ^ Creates an array of given length.
    NewArray
      :: ( PredicateExp exp a
         , PredicateExp exp i
         , Integral i
         , Ix i )
      => exp i -> ArrayCMD exp prog (Array i a)
    -- ^ Creates an array from the given list of elements.
    InitArray
      :: ( PredicateExp exp a
         , PredicateExp exp i
         , Integral i
         , Ix i )
      => [a] -> ArrayCMD exp prog (Array i a)
    -- ^ Fetches the array's value at a specified index.
    GetArray
      :: ( PredicateExp exp a
         , PredicateExp exp i
         , Integral i
         , Ix i )
      => exp i -> Array i a -> ArrayCMD exp prog (exp a)
    -- ^ Writes a value to an array at some specified index.
    SetArray
      :: ( PredicateExp exp a
         , PredicateExp exp i
         , Integral i
         , Ix i )
      => exp i -> exp a -> Array i a -> ArrayCMD exp prog ()
    -- ^ Unsafe version of fetching an array's value.
    UnsafeGetArray
      :: ( PredicateExp exp a
         , PredicateExp exp i
         , Integral i
         , Ix i )
      => exp i -> Array i a -> ArrayCMD exp prog (exp a)

type instance IExp (ArrayCMD e)       = e
type instance IExp (ArrayCMD e :+: i) = e

instance HFunctor (ArrayCMD exp)
  where
    hfmap _ (NewArray i)         = NewArray i
    hfmap _ (InitArray is)       = InitArray is
    hfmap _ (GetArray i a)       = GetArray i a
    hfmap _ (SetArray i e a)     = SetArray i e a
    hfmap _ (UnsafeGetArray i a) = UnsafeGetArray i a

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

-- | Commnads for conditional statements.
data ConditionalCMD (exp :: * -> *) (prog :: * -> *) a
  where
    If :: PredicateExp exp Bool
       =>  (exp Bool, prog ())  -- if
       -> [(exp Bool, prog ())] -- else-if
       -> Maybe (prog ())       -- else
       -> ConditionalCMD exp prog ()

type instance IExp (ConditionalCMD e)       = e
type instance IExp (ConditionalCMD e :+: i) = e

instance HFunctor (ConditionalCMD exp)
  where
    hfmap f (If a cs b) = If (fmap f a) (fmap (fmap f) cs) (fmap f b)

--------------------------------------------------------------------------------
-- ** Structural entities.

-- | Untyped signals.
data SignalX = forall a. SignalX (Signal a)

-- | Commands for structural entities.
data StructuralCMD (exp :: * -> *) (prog :: * -> *) a
  where
    -- ^ Wraps the program in an entity.
    Entity       :: String -> prog a -> StructuralCMD exp prog a
    -- ^ Wraps the program in an architecture.
    Architecture :: String -> String -> prog a -> StructuralCMD exp prog a
    -- ^ Wraps the program in a process.
    Process      :: [SignalX] -> prog () -> StructuralCMD exp prog ()

type instance IExp (StructuralCMD e)       = e
type instance IExp (StructuralCMD e :+: i) = e

instance HFunctor (StructuralCMD exp)
  where
    hfmap f (Entity e p)         = Entity e (f p)
    hfmap f (Architecture e a p) = Architecture e a (f p)
    hfmap f (Process xs p)       = Process xs (f p)

--------------------------------------------------------------------------------
