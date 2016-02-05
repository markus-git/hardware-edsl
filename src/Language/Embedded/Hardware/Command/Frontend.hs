{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Embedded.Hardware.Command.Frontend where

import Language.Embedded.VHDL               (Mode(..))
import Language.Embedded.Hardware.Interface
import Language.Embedded.Hardware.Command.CMD

import Control.Monad.Operational.Higher

import Data.Ix    (Ix)
import Data.IORef (readIORef)

import System.IO.Unsafe -- used for `veryUnsafeFreezeVariable`.

--------------------------------------------------------------------------------
-- ** Signals.

-- | Declare a signal.
newSignal  :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a) => IExp i a -> ProgramT i m (Signal a)
newSignal  = singleE . NewSignal Port SArchitecture InOut . Just

-- | Declare an uninitialized signal.
newSignal_ :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a) => ProgramT i m (Signal a)
newSignal_ = singleE $ NewSignal Port SArchitecture InOut Nothing

-- | Fetches the current value of a signal.
getSignal :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a) => Signal a -> ProgramT i m (IExp i a)
getSignal = singleE . GetSignal

-- | Update the value of a signal.
setSignal :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a) => Signal a -> IExp i a -> ProgramT i m ()
setSignal s = singleE . SetSignal s

-- | Unsafe version of fetching the contents of a signal.
unsafeFreezeSignal :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a) => Signal a -> ProgramT i m (IExp i a)
unsafeFreezeSignal = singleE . UnsafeFreezeSignal

-- | Declare port signals of the given mode and assign it initial value.
newPort :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a) => Mode -> IExp i a -> ProgramT i m (Signal a)
newPort m = singleE . NewSignal Port SEntity m . Just

-- | Declare port signals of the given mode.
newPort_ :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a) => Mode -> ProgramT i m (Signal a)
newPort_ m = singleE $ NewSignal Port SEntity m Nothing

-- | Declare generic signals of the given mode and assign it initial value.
newGeneric :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a) => Mode -> IExp i a -> ProgramT i m (Signal a)
newGeneric m = singleE . NewSignal Generic SEntity m . Just

-- | Declare generic signals of the given mode.
newGeneric_ :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a) => Mode -> ProgramT i m (Signal a)
newGeneric_ m = singleE $ NewSignal Generic SEntity m Nothing

-- | Short-hand for 'setSignal'.
(<==) :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a) => Signal a -> IExp i a -> ProgramT i m ()
(<==) = setSignal

--------------------------------------------------------------------------------
-- ** Variables.

-- | Declare a variable.
newVariable  :: (VariableCMD (IExp i) :<: i, PredicateExp (IExp i) a) => IExp i a -> ProgramT i m (Variable a)
newVariable  = singleE . NewVariable . Just

-- | Declare an uninitialized variable.
newVariable_ :: (VariableCMD (IExp i) :<: i, PredicateExp (IExp i) a) => ProgramT i m (Variable a)
newVariable_ = singleE $ NewVariable Nothing

-- | Fetches the current value of a variable.
getVariable  :: (VariableCMD (IExp i) :<: i, PredicateExp (IExp i) a) => Variable a -> ProgramT i m (IExp i a)
getVariable = singleE . GetVariable

-- | Updates the value of a variable.
setVariable :: (VariableCMD (IExp i) :<: i, PredicateExp (IExp i) a) => Variable a -> IExp i a -> ProgramT i m ()
setVariable v = singleE . SetVariable v

-- | Unsafe version of fetching the contents of a variable.
unsafeFreezeVariable
  :: (VariableCMD (IExp i) :<: i, PredicateExp (IExp i) a) => Variable a -> ProgramT i m (IExp i a)
unsafeFreezeVariable = singleE . UnsafeFreezeVariable

-- | Read the value of a reference without the monad in a very unsafe fashion.
veryUnsafeFreezeVariable
  :: (PredicateExp exp a, EvaluateExp exp, CompileExp exp) => Variable a -> exp a
veryUnsafeFreezeVariable (VariableE r) = litE $! unsafePerformIO $! readIORef r
veryUnsafeFreezeVariable (VariableC v) = varE v

-- | Short-hand for 'setVariable'.
(==:) :: (VariableCMD (IExp i) :<: i, PredicateExp (IExp i) a) => Variable a -> IExp i a -> ProgramT i m ()
(==:) = setVariable

--------------------------------------------------------------------------------
-- ** Arrays.

-- | Create an uninitialized array.
newArray
  :: ( PredicateExp (IExp instr) a
     , PredicateExp (IExp instr) i
     , Integral i, Ix i
     , ArrayCMD (IExp instr) :<: instr )
  => IExp instr i -> ProgramT instr m (Array i a)
newArray = singleE . NewArray

-- | Create an initialized array.
initArray
  :: ( PredicateExp (IExp instr) a
     , PredicateExp (IExp instr) i
     , Integral i, Ix i
     , ArrayCMD (IExp instr) :<: instr )
  => [a] -> ProgramT instr m (Array i a)  
initArray = singleE . InitArray

-- | Get an element of an array.
getArray
  :: ( PredicateExp (IExp instr) a
     , PredicateExp (IExp instr) i
     , Integral i, Ix i
     , ArrayCMD (IExp instr) :<: instr )
  => IExp instr i -> Array i a -> ProgramT instr m (IExp instr a)
getArray i = singleE . GetArray i

-- | Set an element of an array.
setArray
  :: ( PredicateExp (IExp instr) a
     , PredicateExp (IExp instr) i
     , Integral i, Ix i
     , ArrayCMD (IExp instr) :<: instr )
  => IExp instr i -> IExp instr a -> Array i a -> ProgramT instr m ()
setArray i a = singleE . SetArray i a

-- | Copy a slice of one array to another.
copyArray
  :: ( PredicateExp (IExp instr) a
     , PredicateExp (IExp instr) i
     , Integral i, Ix i
     , ArrayCMD (IExp instr) :<: instr )
  => Array i a -> Array i a -> IExp instr i -> ProgramT instr m ()
copyArray dest src = singleE . CopyArray dest src

-- | Freeze a mutable array into an immutable one by copying.
freezeArray
  :: ( PredicateExp (IExp instr) a
     , PredicateExp (IExp instr) i
     , Integral i, Ix i
     , Monad m
     , ArrayCMD (IExp instr) :<: instr )
  => Array i a -> IExp instr i -> ProgramT instr m (IArray i a)
freezeArray array len =
  do copy <- newArray len
     copyArray copy array len
     unsafeFreezeArray copy

-- | Unsafe version of fetching the contents of an array's index.
unsafeFreezeArray
  :: ( PredicateExp (IExp instr) a
     , PredicateExp (IExp instr) i
     , Integral i, Ix i
     , ArrayCMD (IExp instr) :<: instr )
  => Array i a -> ProgramT instr m (IArray i a)
unsafeFreezeArray = singleE . UnsafeFreezeArray

--------------------------------------------------------------------------------
-- ** Looping.

-- | For loop.
for
  :: (LoopCMD (IExp instr) :<: instr, PredicateExp (IExp instr) n, Integral n)
  => IExp instr n
  -> (IExp instr n -> ProgramT instr m ())
  -> ProgramT instr m ()
for range = singleE . For range

-- | While loop.
while
  :: (LoopCMD (IExp instr) :<: instr, PredicateExp (IExp instr) Bool)
  => ProgramT instr m (IExp instr Bool)
  -> ProgramT instr m ()
  -> ProgramT instr m ()
while cond = singleE . While cond

--------------------------------------------------------------------------------
-- ** Conditional statements.

-- | Conditional statements guarded by if and then clauses with an optional else.
conditional
  :: (ConditionalCMD (IExp i) :<: i, PredicateExp (IExp i) Bool)
  =>  (IExp i Bool, ProgramT i m ())
  -> [(IExp i Bool, ProgramT i m ())]
  -> Maybe (ProgramT i m ())
  -> ProgramT i m ()
conditional a bs = singleE . If a bs

-- | Guarded statement.
when
  :: (ConditionalCMD (IExp i) :<: i, PredicateExp (IExp i) Bool)
  => IExp i Bool
  -> ProgramT i m ()
  -> ProgramT i m ()
when e p = conditional (e, p) [] Nothing

-- | Standard if-then-else statement.
iff
  :: (ConditionalCMD (IExp i) :<: i, PredicateExp (IExp i) Bool)
  => IExp i Bool
  -> ProgramT i m ()
  -> ProgramT i m ()
  -> ProgramT i m ()
iff b t e = conditional (b, t) [] (Just e)

--------------------------------------------------------------------------------
-- ** Structural entities.

-- | Declare a new entity by wrapping the program to declare ports & generics.
entity :: (StructuralCMD (IExp i) :<: i) => String -> ProgramT i m a -> ProgramT i m a
entity e = singleE . Entity e

-- | Declare a new architecture for some entity by wrapping the given program.
architecture :: (StructuralCMD (IExp i) :<: i) => String -> String -> ProgramT i m a -> ProgramT i m a
architecture e a = singleE . Architecture e a

-- | Declare a new process listening to some signals by wrapping the given program.
process :: (StructuralCMD (IExp i) :<: i) => [SignalX] -> ProgramT i m () -> ProgramT i m ()
process is = singleE . Process is

-- | ...
hideSig :: Signal a -> SignalX
hideSig = SignalX

--------------------------------------------------------------------------------
