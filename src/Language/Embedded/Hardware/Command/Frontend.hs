{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Embedded.Hardware.Command.Frontend where

import Language.Embedded.VHDL               (Mode(..))
import Language.Embedded.Hardware.Interface
import Language.Embedded.Hardware.Command.CMD

import Language.Embedded.Hardware.Expression.Represent

import Control.Monad.Operational.Higher

import Data.Ix    (Ix)
import Data.IORef (readIORef)
import Data.Int
import Data.Word

import System.IO.Unsafe -- used for `veryUnsafeFreezeVariable`.

--------------------------------------------------------------------------------
-- ** Signals.

-- | Declare a named signal.
initNamedSignal
  :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a) => String -> IExp i a -> ProgramT i m (Signal a)
initNamedSignal name = singleE . NewSignal name Port SArchitecture InOut . Just

-- | Declare an uninitialized named signal.
newNamedSignal
  :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a) => String -> ProgramT i m (Signal a)
newNamedSignal name = singleE $ NewSignal name Port SArchitecture InOut Nothing

-- | Declare a signal.
initSignal  :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a) => IExp i a -> ProgramT i m (Signal a)
initSignal  = initNamedSignal "s"

-- | Declare an uninitialized signal.
newSignal   :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a) => ProgramT i m (Signal a)
newSignal = newNamedSignal "s"

-- | Fetches the current value of a signal.
getSignal :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a) => Signal a -> ProgramT i m (IExp i a)
getSignal = singleE . GetSignal

-- | Update the value of a signal.
setSignal :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a) => Signal a -> IExp i a -> ProgramT i m ()
setSignal s = singleE . SetSignal s

-- | Unsafe version of fetching the contents of a signal.
unsafeFreezeSignal :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a) => Signal a -> ProgramT i m (IExp i a)
unsafeFreezeSignal = singleE . UnsafeFreezeSignal

--------------------------------------------------------------------------------

-- | Declare port signals of the given mode and assign it initial value.
initPort
  :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a) => String -> Mode -> IExp i a -> ProgramT i m (Signal a)
initPort name m = singleE . NewSignal name Port SEntity m . Just

-- | Declare port signals of the given mode.
newPort
  :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a) => String -> Mode -> ProgramT i m (Signal a)
newPort name m = singleE $ NewSignal name Port SEntity m Nothing

-- | Declare generic signals of the given mode and assign it initial value.
initGeneric
  :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a) => String -> Mode -> IExp i a -> ProgramT i m (Signal a)
initGeneric name m = singleE . NewSignal name Generic SEntity m . Just

-- | Declare generic signals of the given mode.
newGeneric
  :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a) => String -> Mode -> ProgramT i m (Signal a)
newGeneric name m = singleE $ NewSignal name Generic SEntity m Nothing

--------------------------------------------------------------------------------

-- | Short-hand for 'setSignal'.
(<==) :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a) => Signal a -> IExp i a -> ProgramT i m ()
(<==) = setSignal

--------------------------------------------------------------------------------
-- ** Variables.

-- | Declare a named variable.
initNamedVariable
  :: (VariableCMD (IExp i) :<: i, PredicateExp (IExp i) a) => String -> IExp i a -> ProgramT i m (Variable a)
initNamedVariable name = singleE . NewVariable name . Just

-- | Declare an uninitialized named variable.
newNamedVariable
  :: (VariableCMD (IExp i) :<: i, PredicateExp (IExp i) a) => String -> ProgramT i m (Variable a)
newNamedVariable name = singleE $ NewVariable name Nothing

-- | Declare a variable.
initVariable :: (VariableCMD (IExp i) :<: i, PredicateExp (IExp i) a) => IExp i a -> ProgramT i m (Variable a)
initVariable = initNamedVariable "v"

-- | Declare an uninitialized variable.
newVariable  :: (VariableCMD (IExp i) :<: i, PredicateExp (IExp i) a) => ProgramT i m (Variable a)
newVariable = newNamedVariable "v"

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

-- | Create an uninitialized named array.
newNamedArray
  :: ( PredicateExp (IExp instr) a
     , PredicateExp (IExp instr) i
     , Integral i, Ix i
     , ArrayCMD (IExp instr) :<: instr)
  => String -> IExp instr i -> ProgramT instr m (Array i a)
newNamedArray name = singleE . NewArray name

-- | Create an initialized named array.
initNamedArray
  :: ( PredicateExp (IExp instr) a
     , PredicateExp (IExp instr) i
     , Integral i, Ix i
     , ArrayCMD (IExp instr) :<: instr)
  => String -> [a] -> ProgramT instr m (Array i a)  
initNamedArray name = singleE . InitArray name

newArray
  :: ( PredicateExp (IExp instr) a
     , PredicateExp (IExp instr) i
     , Integral i, Ix i
     , ArrayCMD (IExp instr) :<: instr)
  => IExp instr i -> ProgramT instr m (Array i a) 
newArray = newNamedArray "a"

initArray
  :: ( PredicateExp (IExp instr) a
     , PredicateExp (IExp instr) i
     , Integral i, Ix i
     , ArrayCMD (IExp instr) :<: instr)
  => [a] -> ProgramT instr m (Array i a)
initArray = initNamedArray "a"

-- | Get an element of an array.
getArray
  :: ( PredicateExp (IExp instr) a
     , PredicateExp (IExp instr) i
     , Integral i, Ix i
     , ArrayCMD (IExp instr) :<: instr)
  => IExp instr i -> Array i a -> ProgramT instr m (IExp instr a)
getArray i = singleE . GetArray i

-- | Set an element of an array.
setArray
  :: ( PredicateExp (IExp instr) a
     , PredicateExp (IExp instr) i
     , Integral i, Ix i
     , ArrayCMD (IExp instr) :<: instr)
  => IExp instr i -> IExp instr a -> Array i a -> ProgramT instr m ()
setArray i a = singleE . SetArray i a

-- | Copy a slice of one array to another.
copyArray
  :: ( PredicateExp (IExp instr) a
     , PredicateExp (IExp instr) i
     , Integral i, Ix i
     , ArrayCMD (IExp instr) :<: instr)
  => Array i a -> Array i a -> IExp instr i -> ProgramT instr m ()
copyArray dest src = singleE . CopyArray dest src

-- | Freeze a mutable array into an immutable one by copying.
freezeArray
  :: ( PredicateExp (IExp instr) a
     , PredicateExp (IExp instr) i
     , Integral i, Ix i
     , Monad m
     , ArrayCMD (IExp instr) :<: instr)
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
     , ArrayCMD (IExp instr) :<: instr)
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
-- ** Processes.

-- | Wrap a signed program in a new component.
process 
  :: forall i m a.
     ( ComponentCMD (IExp i) :<: i
     , Monad m)
  => Sig (IExp i) (ProgramT i m) a
  -> ProgramT i m (Process (IExp i) (ProgramT i m) a)
process sig =
  do n <- singleE $ Component sig
     return $ Process n sig

-- | Map signals to some component.
portmap
  :: forall i m a. (ComponentCMD (IExp i) :<: i)
  => Process (IExp i) (ProgramT i m) a
  -> Arg a
  -> ProgramT i m ()
portmap pro arg = singleE $ PortMap pro arg


--------------------------------------------------------------------------------
-- ** Structural entities.

-- | Declare a new entity by wrapping the program to declare ports & generics.
structEntity :: (StructuralCMD (IExp i) :<: i) => String -> ProgramT i m a -> ProgramT i m a
structEntity e = singleE . StructEntity e

-- | Declare a new architecture for some entity by wrapping the given program.
structArchitecture :: (StructuralCMD (IExp i) :<: i) => String -> String -> ProgramT i m a -> ProgramT i m a
structArchitecture e a = singleE . StructArchitecture e a

-- | Declare a new process listening to some signals by wrapping the given program.
structProcess :: (StructuralCMD (IExp i) :<: i) => [SignalX] -> ProgramT i m () -> ProgramT i m ()
structProcess is = singleE . StructProcess is

--------------------------------------------------------------------------------
