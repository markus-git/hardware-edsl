{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE PolyKinds           #-}

module Language.Embedded.Hardware.Command.Frontend where

import Language.Embedded.VHDL (Mode(..))
import Language.Embedded.Hardware.Interface
import Language.Embedded.Hardware.Command.CMD

import Language.Embedded.Hardware.Expression.Represent
import Language.Embedded.Hardware.Expression.Represent.Bit

import Control.Monad.Operational.Higher

import Data.Ix    (Ix)
import Data.IORef (readIORef)
import Data.Int
import Data.Word

import System.IO.Unsafe -- used for `veryUnsafeFreezeVariable`.

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** Signals.

-- | Declare a named signal.
initNamedSignal :: (SignalCMD :<: instr, pred a) => String -> exp a -> ProgramT instr (Param2 exp pred) m (Signal a)
initNamedSignal name = singleInj . NewSignal (Base name) InOut . Just

-- | Declare a signal.
initSignal  :: (SignalCMD :<: instr, pred a) => exp a -> ProgramT instr (Param2 exp pred) m (Signal a)
initSignal  = initNamedSignal "s"

-- | Declare an uninitialized named signal.
newNamedSignal :: (SignalCMD :<: instr, pred a) => String -> ProgramT instr (Param2 exp pred) m (Signal a)
newNamedSignal name = singleInj $ NewSignal (Base name) InOut Nothing

-- | Declare an uninitialized signal.
newSignal   :: (SignalCMD :<: instr, pred a) => ProgramT instr (Param2 exp pred) m (Signal a)
newSignal = newNamedSignal "s"

-- | Fetches the current value of a signal.
getSignal :: (SignalCMD :<: instr, pred a, FreeExp exp, PredicateExp exp a,  Monad m)
  => Signal a -> ProgramT instr (Param2 exp pred) m (exp a)
getSignal = fmap valToExp . singleInj . GetSignal

-- | Update the value of a signal.
setSignal :: (SignalCMD :<: instr, pred a) => Signal a -> exp a -> ProgramT instr (Param2 exp pred) m ()
setSignal s = singleInj . SetSignal s

-- | Unsafe version of fetching the contents of a signal.
unsafeFreezeSignal :: (SignalCMD :<: instr, pred a, FreeExp exp, PredicateExp exp a, Monad m)
  => Signal a -> ProgramT instr (Param2 exp pred) m (exp a)
unsafeFreezeSignal = fmap valToExp . singleInj . UnsafeFreezeSignal

--------------------------------------------------------------------------------
-- *** Signal attributes.

-- ...

--------------------------------------------------------------------------------
-- ports.

-- | Declare port signals of the given mode and assign it initial value.
initNamedPort, initExactPort :: (SignalCMD :<: instr, pred a)
  => String -> Mode -> exp a -> ProgramT instr (Param2 exp pred) m (Signal a)
initNamedPort name m = singleInj . NewSignal (Base  name) m . Just
initExactPort name m = singleInj . NewSignal (Exact name) m . Just

initPort :: (SignalCMD :<: instr, pred a) => Mode -> exp a -> ProgramT instr (Param2 exp pred) m (Signal a)
initPort = initNamedPort "p"

-- | Declare port signals of the given mode.
newNamedPort, newExactPort :: (SignalCMD :<: instr, pred a)
  => String -> Mode -> ProgramT instr (Param2 exp pred) m (Signal a)
newNamedPort name m = singleInj $ NewSignal (Base  name) m Nothing
newExactPort name m = singleInj $ NewSignal (Exact name) m Nothing

newPort :: (SignalCMD :<: instr, pred a) => Mode -> ProgramT instr (Param2 exp pred) m (Signal a)
newPort = newNamedPort "p"

--------------------------------------------------------------------------------
-- short-hands.

signal :: (SignalCMD :<: instr, pred a) => String -> ProgramT instr (Param2 exp pred) m (Signal a)
signal = newNamedSignal

(<--) :: (SignalCMD :<: instr, pred a, PredicateExp exp a, FreeExp exp, Monad m)
  => Signal a -> a -> ProgramT instr (Param2 exp pred) m ()
(<--) s e = s <== (litE e)

(<=-) :: (SignalCMD :<: instr, pred a, PredicateExp exp a, FreeExp exp, Monad m)
  => Signal a -> Signal a -> ProgramT instr (Param2 exp pred) m ()
(<=-) s v = do v' <- unsafeFreezeSignal v; s <== v'

(<==) :: (SignalCMD :<: instr, pred a) => Signal a -> exp a -> ProgramT instr (Param2 exp pred) m ()
(<==) s e = setSignal s e

--------------------------------------------------------------------------------
-- ** Variables.

-- | Declare a named variable.
initNamedVariable :: (VariableCMD :<: instr, pred a)
  => String -> exp a -> ProgramT instr (Param2 exp pred) m (Variable a)
initNamedVariable name = singleInj . NewVariable (Base name) . Just

-- | Declare a variable.
initVariable :: (VariableCMD :<: instr, pred a) => exp a -> ProgramT instr (Param2 exp pred) m (Variable a)
initVariable = initNamedVariable "v"

-- | Declare an uninitialized named variable.
newNamedVariable :: (VariableCMD :<: instr, pred a)
  => String -> ProgramT instr (Param2 exp pred) m (Variable a)
newNamedVariable name = singleInj $ NewVariable (Base name) Nothing

-- | Declare an uninitialized variable.
newVariable  :: (VariableCMD :<: instr, pred a) => ProgramT instr (Param2 exp pred) m (Variable a)
newVariable = newNamedVariable "v"

-- | Fetches the current value of a variable.
getVariable  :: (VariableCMD :<: instr, pred a, PredicateExp exp a, FreeExp exp, Monad m)
  => Variable a -> ProgramT instr (Param2 exp pred) m (exp a)
getVariable = fmap valToExp . singleInj . GetVariable

-- | Updates the value of a variable.
setVariable :: (VariableCMD :<: instr, pred a) => Variable a -> exp a -> ProgramT instr (Param2 exp pred) m ()
setVariable v = singleInj . SetVariable v

-- | Unsafe version of fetching the contents of a variable.
unsafeFreezeVariable :: (VariableCMD :<: instr, pred a, PredicateExp exp a, FreeExp exp, Monad m)
  => Variable a -> ProgramT instr (Param2 exp pred) m (exp a)
unsafeFreezeVariable = fmap valToExp . singleInj . UnsafeFreezeVariable

-- | Read the value of a reference without the monad in a very unsafe fashion.
veryUnsafeFreezeVariable :: (PredicateExp exp a, FreeExp exp) => Variable a -> exp a
veryUnsafeFreezeVariable (VariableE r) = litE $! unsafePerformIO $! readIORef r
veryUnsafeFreezeVariable (VariableC v) = varE v

--------------------------------------------------------------------------------
-- short-hands.

variable :: (VariableCMD :<: instr, pred a) => String -> ProgramT instr (Param2 exp pred) m (Variable a)
variable = newNamedVariable

-- | Short-hand for 'setVariable'.
(==:) :: (VariableCMD :<: instr, pred a) => Variable a -> exp a -> ProgramT instr (Param2 exp pred) m ()
(==:) = setVariable

--------------------------------------------------------------------------------
-- ** Constants.

initNamedConstant :: (ConstantCMD :<: instr, pred a)
  => String -> exp a -> ProgramT instr (Param2 exp pred) m (Constant a)
initNamedConstant name = singleInj . NewConstant (Base name)

initConstant :: (ConstantCMD :<: instr, pred a) => exp a -> ProgramT instr (Param2 exp pred) m (Constant a)
initConstant = initNamedConstant "c"

getConstant :: (ConstantCMD :<: instr, pred a, PredicateExp exp a, FreeExp exp, Monad m)
  => Constant a -> ProgramT instr (Param2 exp pred) m (exp a)
getConstant = fmap valToExp . singleInj . GetConstant

--------------------------------------------------------------------------------
-- short-hands.

constant :: (ConstantCMD :<: instr, pred a) => String -> exp a -> ProgramT instr (Param2 exp pred) m (Constant a)
constant = initNamedConstant

--------------------------------------------------------------------------------
-- ** Arrays.

getSignalRange
  :: (ArrayCMD :<: instr, pred i, pred UBits, Integral i, Ix i, FreeExp exp, PredicateExp exp UBits, Monad m)
  => exp i -> exp i -> Signal (Bits n) -> ProgramT instr (Param2 exp pred) m (exp UBits)
getSignalRange low high = fmap valToExp . singleInj . GetRange low high

setSignalRange
  :: (ArrayCMD :<: instr, pred i, pred UBits, Integral i, Ix i)
  => exp i -> exp i -> Signal (Bits n) -> exp UBits -> ProgramT instr (Param2 exp pred) m ()
setSignalRange low high s = singleInj . SetRange low high s

getVariableRange
  :: (ArrayCMD :<: instr, pred i, pred UBits, Integral i, Ix i, FreeExp exp, PredicateExp exp UBits, Monad m)
  => exp i -> exp i -> Variable (Bits n) -> ProgramT instr (Param2 exp pred) m (exp UBits)
getVariableRange low high = fmap valToExp . singleInj . GetRangeV low high

setVariableRange
  :: (ArrayCMD :<: instr, pred i, pred UBits, Integral i, Ix i)
  => exp i -> exp i -> Variable (Bits n) -> exp UBits -> ProgramT instr (Param2 exp pred) m ()
setVariableRange low high v = singleInj . SetRangeV low high v
-- ...

--------------------------------------------------------------------------------
-- ** Virtual arrays.

-- | Create an initialized named virtual array.
initNamedVArray :: (VArrayCMD :<: instr, pred a, pred i, Integral i, Ix i)
  => String -> [a] -> ProgramT instr (Param2 exp pred) m (VArray i a)  
initNamedVArray name = singleInj . InitVArray (Base name)

-- | Create an initialized virtual array.
initVArray :: (VArrayCMD :<: instr, pred a, pred i, Integral i, Ix i)
  => [a] -> ProgramT instr (Param2 exp pred) m (VArray i a)
initVArray = initNamedVArray "a"

-- | Create an uninitialized named virtual array.
newNamedVArray :: (VArrayCMD :<: instr, pred a, pred i, Integral i, Ix i)
  => String -> exp i -> ProgramT instr (Param2 exp pred) m (VArray i a)
newNamedVArray name = singleInj . NewVArray (Base name)

-- | Create an uninitialized virtual array.
newVArray :: (VArrayCMD :<: instr, pred a, pred i, Integral i, Ix i)
  => exp i -> ProgramT instr (Param2 exp pred) m (VArray i a) 
newVArray = newNamedVArray "a"

-- | Get an element of an array.
getVArray :: (VArrayCMD :<: instr, pred a, pred i, Integral i, Ix i, PredicateExp exp a, FreeExp exp, Monad m)
  => exp i -> VArray i a -> ProgramT instr (Param2 exp pred) m (exp a)
getVArray i = fmap valToExp . singleInj . GetVArray i

-- | Set an element of an array.
setVArray :: (VArrayCMD :<: instr, pred a, pred i, Integral i, Ix i)
  => exp i -> exp a -> VArray i a -> ProgramT instr (Param2 exp pred) m ()
setVArray i a = singleInj . SetVArray i a

-- | Copy a slice of one array to another.
copyVArray :: (VArrayCMD :<: instr, pred a, pred i, Integral i, Ix i)
  => VArray i a -> VArray i a -> exp i -> ProgramT instr (Param2 exp pred) m ()
copyVArray dest src = singleInj . CopyVArray dest src

-- | Freeze a mutable array into an immutable one by copying.
freezeArray :: (VArrayCMD :<: instr, pred a, pred i, Integral i, Ix i, Monad m)
  => VArray i a -> exp i -> ProgramT instr (Param2 exp pred) m (IArray i a)
freezeArray array len =
  do copy <- newVArray len
     copyVArray copy array len
     unsafeFreezeVArray copy

-- | Unsafe version of fetching the contents of an array's index.
unsafeFreezeVArray :: (pred a, pred i, Integral i, Ix i, VArrayCMD :<: instr)
  => VArray i a -> ProgramT instr (Param2 exp pred) m (IArray i a)
unsafeFreezeVArray = singleInj . UnsafeFreezeVArray

--------------------------------------------------------------------------------
-- ** Looping.

-- | For loop.
for :: (LoopCMD :<: instr, pred n, Integral n, PredicateExp exp n, FreeExp exp, Monad m)
  => exp n -> (exp n -> ProgramT instr (Param2 exp pred) m ()) -> ProgramT instr (Param2 exp pred) m ()
for range body = singleInj $ For range (body . valToExp)

-- | While loop.
while :: (LoopCMD :<: instr, pred Bool)
  => ProgramT instr (Param2 exp pred) m (exp Bool)
  -> ProgramT instr (Param2 exp pred) m ()
  -> ProgramT instr (Param2 exp pred) m ()
while cond = singleInj . While cond

--------------------------------------------------------------------------------
-- ** Conditional statements.

-- | Conditional statements guarded by if and then clauses with an optional else.
conditional :: (ConditionalCMD :<: instr, pred Bool)
  =>  (exp Bool, ProgramT instr (Param2 exp pred) m ())
  -> [(exp Bool, ProgramT instr (Param2 exp pred) m ())]
  -> Maybe (ProgramT instr (Param2 exp pred) m ())
  -> ProgramT instr (Param2 exp pred) m ()
conditional a bs = singleInj . If a bs

-- | Guarded statement.
when :: (ConditionalCMD :<: instr, pred Bool)
  => exp Bool
  -> ProgramT instr (Param2 exp pred) m ()
  -> ProgramT instr (Param2 exp pred) m ()
when e p = conditional (e, p) [] Nothing

-- | Standard if-then-else statement.
iff
  :: (ConditionalCMD :<: instr, pred Bool)
  => exp Bool
  -> ProgramT instr (Param2 exp pred) m ()
  -> ProgramT instr (Param2 exp pred) m ()
  -> ProgramT instr (Param2 exp pred) m ()
iff b t e = conditional (b, t) [] (Just e)

ifE
  :: (ConditionalCMD :<: instr, pred Bool)
  => (exp Bool, ProgramT instr (Param2 exp pred) m ())
  -> (exp Bool, ProgramT instr (Param2 exp pred) m ())
  -> ProgramT instr (Param2 exp pred) m ()
ifE a b = conditional a [b] (Nothing)

--------------------------------------------------------------------------------

-- | ...
risingEdge
  :: (ConditionalCMD :<: instr, pred Bool)
  => Signal Bool
  -> ProgramT instr (Param2 exp pred) m ()
  -> ProgramT instr (Param2 exp pred) m ()
risingEdge s p = singleInj $ WhenRising s p

--------------------------------------------------------------------------------

switch :: (ConditionalCMD :<: instr, pred a, Eq a, Ord a)
  => exp a -> [When a (ProgramT instr (Param2 exp pred) m)] -> ProgramT instr (Param2 exp pred) m ()
switch e choices = singleInj (Case e choices Nothing)

switched  :: (ConditionalCMD :<: instr, pred a, Eq a, Ord a)
  => exp a
  -> [When a (ProgramT instr (Param2 exp pred) m)]
  -> ProgramT instr (Param2 exp pred) m ()
  -> ProgramT instr (Param2 exp pred) m ()
switched e choices def = singleInj (Case e choices (Just def))

null :: (ConditionalCMD :<: instr) => ProgramT instr (Param2 exp pred) m ()
null = singleInj (Null)

is :: (Eq a, pred a)
  => a -> ProgramT instr (Param2 exp pred) m ()
  -> When a (ProgramT instr (Param2 exp pred) m)
is a = When (Is a) 

to :: (Ord a, pred a)
  => a -> a -> ProgramT instr (Param2 exp pred) m ()
  -> When a (ProgramT instr (Param2 exp pred) m)
to l h = When (To l h)

--------------------------------------------------------------------------------
-- ** Processes.

type Sig  instr exp pred m = Signature (Param3 (ProgramT instr (Param2 exp pred) m) exp pred)
type Comp instr exp pred m = Component (Param3 (ProgramT instr (Param2 exp pred) m) exp pred)

-- | Wrap a signed program in a new component.
namedComponent :: (ComponentCMD :<: instr, Monad m) => String -> Sig instr exp pred m a
  -> ProgramT instr (Param2 exp pred) m (Comp instr exp pred m a)
namedComponent name sig =
  do n <- singleInj $ StructComponent (Base name) sig
     return $ Component n sig

component :: (ComponentCMD :<: instr, Monad m) => Sig instr exp pred m a
  -> ProgramT instr (Param2 exp pred) m (Comp instr exp pred m a)
component = namedComponent "comp"

-- | Map signals to some component.
portmap :: (ComponentCMD :<: instr) => Comp instr exp pred m a -> Arg a -> ProgramT instr (Param2 exp pred) m ()
portmap pro arg = singleInj $ PortMap pro arg

--------------------------------------------------------------------------------

exactOutput :: pred a => String -> (Signal a -> Sig instr exp pred m b) -> Sig instr exp pred m (Signal a -> b)
exactOutput n = Lam (Exact n) Out

namedOutput :: pred a => String -> (Signal a -> Sig instr exp pred m b) -> Sig instr exp pred m (Signal a -> b)
namedOutput n = Lam (Base n) Out

output :: pred a => (Signal a -> Sig instr exp pred m b) -> Sig instr exp pred m (Signal a -> b)
output = namedOutput "out"

exactInput  :: pred a => String -> (Signal a -> Sig instr exp pred m b) -> Sig instr exp pred m (Signal a -> b)
exactInput  n = Lam (Exact n) In

namedInput :: pred a => String -> (Signal a -> Sig instr exp pred m b) -> Sig instr exp pred m (Signal a -> b)
namedInput n = Lam (Base n) In

input :: pred a => (Signal a -> Sig instr exp pred m b) -> Sig instr exp pred m (Signal a -> b)
input = namedInput "in"

ret :: (ProgramT instr (Param2 exp pred) m) () -> Signature (Param3 (ProgramT instr (Param2 exp pred) m) exp pred) ()
ret = Ret

--------------------------------------------------------------------------------
-- ** Structural entities.

-- | Declare a new entity by wrapping the program to declare ports & generics.
entity :: (StructuralCMD :<: instr)
  => String
  -> ProgramT instr (Param2 exp pred) m a
  -> ProgramT instr (Param2 exp pred) m a
entity e = singleInj . StructEntity (Base e)

-- | Declare a new architecture for some entity by wrapping the given program.
architecture :: (StructuralCMD :<: instr)
  => String -> String
  -> ProgramT instr (Param2 exp pred) m a
  -> ProgramT instr (Param2 exp pred) m a
architecture e a = singleInj . StructArchitecture (Base e) (Base a)

-- | Declare a new process listening to some signals by wrapping the given program.
process :: (StructuralCMD :<: instr)
  => [Ident]
  -> ProgramT instr (Param2 exp pred) m ()
  -> ProgramT instr (Param2 exp pred) m ()
process is = singleInj . StructProcess is

--------------------------------------------------------------------------------

