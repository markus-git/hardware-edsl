{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Embedded.Hardware.Command.Frontend where

import Language.Embedded.VHDL (Mode(..))
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
initNamedSignal name = singleE . NewSignal (Base name) InOut . Just

-- | Declare an uninitialized named signal.
newNamedSignal
  :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a) => String -> ProgramT i m (Signal a)
newNamedSignal name = singleE $ NewSignal (Base name) InOut Nothing

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
-- ..meh.

-- | ...
setOthers
  :: ( SignalCMD    (IExp i) :<: i
     , PredicateExp (IExp i) Bool)
  => Signal a -> IExp i Bool -> ProgramT i m ()
setOthers s ix = singleE (SetOthers s ix)

getIndex
  :: ( SignalCMD (IExp i) :<: i
     , PredicateExp (IExp i) a
     , PredicateExp (IExp i) ix
     , Integral ix
     , Ix ix)
  => Signal a -> IExp i ix -> ProgramT i m (IExp i a)
getIndex s ix = singleE (GetIndex s ix)

-- | ...
getSlice
  :: ( SignalCMD    (IExp i) :<: i
     , PredicateExp (IExp i) a
     , PredicateExp (IExp i) b
     , PredicateExp (IExp i) ix
     , Integral ix
     , Ix ix)
  => Signal a -> IExp i ix -> IExp i ix -> ProgramT i m (Variable b)
getSlice s l u = singleE (GetSlice s l u)

--------------------------------------------------------------------------------
-- ports.

-- | Declare port signals of the given mode and assign it initial value.
initPort, initUniquePort
  :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a)
  => String -> Mode -> IExp i a -> ProgramT i m (Signal a)
initPort       name m = singleE . NewSignal (Base   name) m . Just
initUniquePort name m = singleE . NewSignal (Unique name) m . Just

-- | Declare port signals of the given mode.
newPort, newUniquePort
  :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a)
  => String -> Mode -> ProgramT i m (Signal a)
newPort       name m = singleE $ NewSignal (Base   name) m Nothing
newUniquePort name m = singleE $ NewSignal (Unique name) m Nothing

--------------------------------------------------------------------------------
-- short-hands.

signal :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a) => String -> ProgramT i m (Signal a)
signal = newNamedSignal

(<--) :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a, FreeExp (IExp i)) => Signal a -> a -> ProgramT i m ()
(<--) s e = setSignal s (litE e)

(<:-) :: (SignalCMD (IExp i) :<: i, ConstantCMD (IExp i) :<: i, PredicateExp (IExp i) a, Monad m)
      => Signal a -> Constant a -> ProgramT i m ()
(<:-) s c = setSignal s =<< getConstant c

(<:=) :: (SignalCMD (IExp i) :<: i, VariableCMD (IExp i) :<: i, PredicateExp (IExp i) a, Monad m)
      => Signal a -> Variable a -> ProgramT i m ()
(<:=) s v = setSignal s =<< unsafeFreezeVariable v

(<==) :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a, Monad m) => Signal a -> Signal a -> ProgramT i m ()
(<==) s v = setSignal s =<< unsafeFreezeSignal v

--------------------------------------------------------------------------------
-- ** Variables.

-- | Declare a named variable.
initNamedVariable 
  :: (VariableCMD (IExp i) :<: i, PredicateExp (IExp i) a) => String -> IExp i a -> ProgramT i m (Variable a)
initNamedVariable name = singleE . NewVariable (Base name) . Just

-- | Declare an uninitialized named variable.
newNamedVariable
  :: (VariableCMD (IExp i) :<: i, PredicateExp (IExp i) a) => String -> ProgramT i m (Variable a)
newNamedVariable name = singleE $ NewVariable (Base name) Nothing

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

--------------------------------------------------------------------------------
-- short-hands.

variable :: (VariableCMD (IExp i) :<: i, PredicateExp (IExp i) a) => String -> ProgramT i m (Variable a)
variable = newNamedVariable

-- | Short-hand for 'setVariable'.
(==:) :: (VariableCMD (IExp i) :<: i, PredicateExp (IExp i) a) => Variable a -> IExp i a -> ProgramT i m ()
(==:) = setVariable

--------------------------------------------------------------------------------
-- ** Constants.

initNamedConstant
  :: (ConstantCMD (IExp i) :<: i, PredicateExp (IExp i) a) => String -> IExp i a -> ProgramT i m (Constant a)
initNamedConstant name = singleE . NewConstant (Base name)

initConstant :: (ConstantCMD (IExp i) :<: i, PredicateExp (IExp i) a) => IExp i a -> ProgramT i m (Constant a)
initConstant = initNamedConstant "c"

getConstant :: (ConstantCMD (IExp i) :<: i, PredicateExp (IExp i) a) => Constant a -> ProgramT i m (IExp i a)
getConstant = singleE . GetConstant

--------------------------------------------------------------------------------
-- short-hands.

constant :: (ConstantCMD (IExp i) :<: i, PredicateExp (IExp i) a) => String -> IExp i a -> ProgramT i m (Constant a)
constant = initNamedConstant

--------------------------------------------------------------------------------
-- ** Arrays.

-- ...

--------------------------------------------------------------------------------
-- ** Virtual arrays.

-- | Create an uninitialized named virtual array.
newNamedVArray
  :: ( PredicateExp (IExp instr) a
     , PredicateExp (IExp instr) i
     , Integral i, Ix i
     , VArrayCMD (IExp instr) :<: instr)
  => String -> IExp instr i -> ProgramT instr m (VArray i a)
newNamedVArray name = singleE . NewVArray (Base name)

-- | Create an initialized named array.
initNamedVArray
  :: ( PredicateExp (IExp instr) a
     , PredicateExp (IExp instr) i
     , Integral i, Ix i
     , VArrayCMD (IExp instr) :<: instr)
  => String -> [a] -> ProgramT instr m (VArray i a)  
initNamedVArray name = singleE . InitVArray (Base name)

newVArray
  :: ( PredicateExp (IExp instr) a
     , PredicateExp (IExp instr) i
     , Integral i, Ix i
     , VArrayCMD (IExp instr) :<: instr)
  => IExp instr i -> ProgramT instr m (VArray i a) 
newVArray = newNamedVArray "a"

initVArray
  :: ( PredicateExp (IExp instr) a
     , PredicateExp (IExp instr) i
     , Integral i, Ix i
     , VArrayCMD (IExp instr) :<: instr)
  => [a] -> ProgramT instr m (VArray i a)
initVArray = initNamedVArray "a"

-- | Get an element of an array.
getVArray
  :: ( PredicateExp (IExp instr) a
     , PredicateExp (IExp instr) i
     , Integral i, Ix i
     , VArrayCMD (IExp instr) :<: instr)
  => IExp instr i -> VArray i a -> ProgramT instr m (IExp instr a)
getVArray i = singleE . GetVArray i

-- | Set an element of an array.
setVArray
  :: ( PredicateExp (IExp instr) a
     , PredicateExp (IExp instr) i
     , Integral i, Ix i
     , VArrayCMD (IExp instr) :<: instr)
  => IExp instr i -> IExp instr a -> VArray i a -> ProgramT instr m ()
setVArray i a = singleE . SetVArray i a

-- | Copy a slice of one array to another.
copyVArray
  :: ( PredicateExp (IExp instr) a
     , PredicateExp (IExp instr) i
     , Integral i, Ix i
     , VArrayCMD (IExp instr) :<: instr)
  => VArray i a -> VArray i a -> IExp instr i -> ProgramT instr m ()
copyVArray dest src = singleE . CopyVArray dest src

-- | Freeze a mutable array into an immutable one by copying.
freezeArray
  :: ( PredicateExp (IExp instr) a
     , PredicateExp (IExp instr) i
     , Integral i, Ix i
     , Monad m
     , VArrayCMD (IExp instr) :<: instr)
  => VArray i a -> IExp instr i -> ProgramT instr m (IArray i a)
freezeArray array len =
  do copy <- newVArray len
     copyVArray copy array len
     unsafeFreezeVArray copy

-- | Unsafe version of fetching the contents of an array's index.
unsafeFreezeVArray
  :: ( PredicateExp (IExp instr) a
     , PredicateExp (IExp instr) i
     , Integral i, Ix i
     , VArrayCMD (IExp instr) :<: instr)
  => VArray i a -> ProgramT instr m (IArray i a)
unsafeFreezeVArray = singleE . UnsafeFreezeVArray

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

switch
  :: (ConditionalCMD (IExp i) :<: i, PredicateExp (IExp i) a, Eq a, Ord a)
  => IExp i a
  -> [When a (ProgramT i m)]
  -> ProgramT i m ()
switch e choices = singleE (Case e choices Nothing)

switched 
  :: (ConditionalCMD (IExp i) :<: i, PredicateExp (IExp i) a, Eq a, Ord a)
  => IExp i a
  -> [When a (ProgramT i m)]
  -> ProgramT i m ()
  -> ProgramT i m ()
switched e choices def = singleE (Case e choices (Just def))

null :: (ConditionalCMD (IExp i) :<: i) => ProgramT i m ()
null = singleE (Null)

is :: PredicateExp (IExp i) a => a -> ProgramT i m () -> When a (ProgramT i m)
is a = When (Is a) 

to :: PredicateExp (IExp i) a => a -> a -> ProgramT i m () -> When a (ProgramT i m)
to l h = When (To l h)

--------------------------------------------------------------------------------
-- ** Processes.

-- | Wrap a signed program in a new component.
component
  :: forall i m a.
     ( ComponentCMD (IExp i) :<: i
     , Monad m)
  => String
  -> Sig (IExp i) (ProgramT i m) a
  -> ProgramT i m (Component (IExp i) (ProgramT i m) a)
component name sig =
  do n <- singleE $ StructComponent (Unique name) sig
     return $ Component n sig

component_
  :: forall i m a.
     ( ComponentCMD (IExp i) :<: i
     , Monad m)
  => Sig (IExp i) (ProgramT i m) a
  -> ProgramT i m (Component (IExp i) (ProgramT i m) a)
component_ sig =
  do n <- singleE $ StructComponent (Base "comp") sig
     return $ Component n sig

-- | Map signals to some component.
portmap
  :: forall i m a. (ComponentCMD (IExp i) :<: i)
  => Component (IExp i) (ProgramT i m) a
  -> Arg a
  -> ProgramT i m ()
portmap pro arg = singleE $ PortMap pro arg

--------------------------------------------------------------------------------

output :: PredicateExp exp a => String -> (Signal a -> Sig exp m b) -> Sig exp m (Signal a -> b)
output n = Lam (Unique n) Out

input  :: PredicateExp exp a => String -> (Signal a -> Sig exp m b) -> Sig exp m (Signal a -> b)
input  n = Lam (Unique n) In
  
ret :: m () -> Sig exp m ()
ret = Unit

--------------------------------------------------------------------------------
-- ** Structural entities.

-- | Declare a new entity by wrapping the program to declare ports & generics.
structEntity :: (StructuralCMD (IExp i) :<: i) => String -> ProgramT i m a -> ProgramT i m a
structEntity e = singleE . StructEntity (Base e)

-- | Declare a new architecture for some entity by wrapping the given program.
structArchitecture :: (StructuralCMD (IExp i) :<: i) => String -> String -> ProgramT i m a -> ProgramT i m a
structArchitecture e a = singleE . StructArchitecture (Base e) (Base a)

-- | Declare a new process listening to some signals by wrapping the given program.
process :: (StructuralCMD (IExp i) :<: i) => [Ident] -> ProgramT i m () -> ProgramT i m ()
process is = singleE . StructProcess is

--------------------------------------------------------------------------------
-- ** ...

namedInteger
  :: (IntegerCMD (IExp i) :<: i, PredicateExp (IExp i) a, Num a)
  => VarId -> Maybe (a, a) -> ProgramT i m (Signal Integer)
namedInteger v m = singleE (SignalInteger v m)

integer
  :: (IntegerCMD (IExp i) :<: i, PredicateExp (IExp i) a, Num a)
  => Maybe (a, a) -> ProgramT i m (Signal Integer)
integer = namedInteger (Base "i")

--------------------------------------------------------------------------------

