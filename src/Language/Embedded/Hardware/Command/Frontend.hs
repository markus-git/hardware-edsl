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

import Language.Embedded.Hardware.Expression.Frontend (Primary(..), toInteger, cast')
import Language.Embedded.Hardware.Expression.Represent
import Language.Embedded.Hardware.Expression.Represent.Bit

import Control.Monad.Operational.Higher

import Data.Ix    (Ix)
import Data.IORef (readIORef)
import Data.Int
import Data.Word
import Data.Proxy
import Data.Typeable

import System.IO.Unsafe -- used for `veryUnsafeFreezeVariable`.

import GHC.TypeLits (KnownNat)

import Prelude hiding (toInteger)

--------------------------------------------------------------------------------
-- * Hardware frontend.
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** Signals.

-- | Declare a named signal.
initNamedSignal :: (SignalCMD :<: instr, pred a) => String -> exp a -> ProgramT instr (Param2 exp pred) m (Signal a)
initNamedSignal name = singleInj . NewSignal (Base name) . Just

-- | Declare a signal.
initSignal :: (SignalCMD :<: instr, pred a) => exp a -> ProgramT instr (Param2 exp pred) m (Signal a)
initSignal  = initNamedSignal "s"

-- | Declare an uninitialized named signal.
newNamedSignal :: (SignalCMD :<: instr, pred a) => String -> ProgramT instr (Param2 exp pred) m (Signal a)
newNamedSignal name = singleInj $ NewSignal (Base name) Nothing

-- | Declare an uninitialized signal.
newSignal :: (SignalCMD :<: instr, pred a) => ProgramT instr (Param2 exp pred) m (Signal a)
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

-- | Read the value of a signal without the monad in a very unsafe fashion.
veryUnsafeFreezeSignal :: (PredicateExp exp a, FreeExp exp) => Signal a -> exp a
veryUnsafeFreezeSignal (SignalE r) = litE $! unsafePerformIO $! readIORef r
veryUnsafeFreezeSignal (SignalC v) = varE v

--------------------------------------------------------------------------------
-- short-hands.

(<--) :: (SignalCMD :<: instr, pred a, PredicateExp exp a, FreeExp exp, Monad m)
  => Signal a
  -> a
  -> ProgramT instr (Param2 exp pred) m ()
(<--) s e = s <== (litE e)

(<==) :: (SignalCMD :<: instr, pred a)
  => Signal a
  -> exp a
  -> ProgramT instr (Param2 exp pred) m ()
(<==) s e = setSignal s e

(<=-) :: (SignalCMD :<: instr, pred a, PredicateExp exp a, FreeExp exp, Monad m)
  => Signal a
  -> Signal a
  -> ProgramT instr (Param2 exp pred) m ()
(<=-) s v = do v' <- unsafeFreezeSignal v; s <== v'

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
-- ** Arrays.

-- | Create an initialized named virtual array.
initNamedArray :: (ArrayCMD :<: instr, pred a)
  => String -> [a] -> ProgramT instr (Param2 exp pred) m (Array a)  
initNamedArray name = singleInj . InitArray (Base name)

-- | Create an initialized virtual array.
initArray :: (ArrayCMD :<: instr, pred a)
  => [a] -> ProgramT instr (Param2 exp pred) m (Array a)
initArray = initNamedArray "a"

-- | Create an uninitialized named virtual array.
newNamedArray :: (ArrayCMD :<: instr, pred a)
  => String -> exp Integer -> ProgramT instr (Param2 exp pred) m (Array a)
newNamedArray name = singleInj . NewArray (Base name)

-- | Create an uninitialized virtual array.
newArray :: (ArrayCMD :<: instr, pred a)
  => exp Integer -> ProgramT instr (Param2 exp pred) m (Array a) 
newArray = newNamedArray "a"

getArray :: (ArrayCMD :<: instr, pred a, PredicateExp exp a, FreeExp exp, Monad m)
  => Array a -> exp Integer -> ProgramT instr (Param2 exp pred) m (exp a)
getArray a i = fmap valToExp $ singleInj $ GetArray a i

-- | Set an element of an array.
setArray :: (ArrayCMD :<: instr, pred a, PredicateExp exp a, FreeExp exp, Monad m)
  => Array a -> exp Integer -> exp a -> ProgramT instr (Param2 exp pred) m ()
setArray a i = singleInj . SetArray a i

-- | Copy a slice of one array to another.
copyArray :: (ArrayCMD :<: instr, pred a)
  => (Array a, exp Integer) -- ^ destination and its offset.
  -> (Array a, exp Integer) -- ^ source and its offset.
  -> exp Integer            -- ^ number of elements to copy.
  -> ProgramT instr (Param2 exp pred) m ()
copyArray dest src = singleInj . CopyArray dest src

-- | Freeze an array into an immutable one by copying.
freezeArray :: (ArrayCMD :<: instr, pred a, Num (exp Integer), Monad m)
  => Array a -> exp Integer -> ProgramT instr (Param2 exp pred) m (IArray a)
freezeArray array len =
  do copy <- newArray len
     copyArray (copy,0) (array,0) len
     unsafeFreezeArray copy

-- | Thaw an immutable array into a mutable one by copying.
thawArray :: (ArrayCMD :<: instr, pred a, Num (exp Integer), Monad m)
  => IArray a -> exp Integer -> ProgramT instr (Param2 exp pred) m (Array a)
thawArray iarray len =
  do array <- unsafeThawArray iarray
     copy  <- newArray len
     copyArray (copy,0) (array,0) len
     return copy

-- | Freeze a mutable array to an immutable one wothout making a copy.
unsafeFreezeArray :: (ArrayCMD :<: instr, pred a)
  => Array a -> ProgramT instr (Param2 exp pred) m (IArray a)
unsafeFreezeArray = singleInj . UnsafeFreezeArray

-- | Thaw an immutable array to a mutable array without making a copy.
unsafeThawArray :: (ArrayCMD :<: instr, pred a)
  => IArray a -> ProgramT instr (Param2 exp pred) m (Array a)
unsafeThawArray = singleInj . UnsafeThawArray

-- | ...
resetArray :: (ArrayCMD :<: instr, pred a)
  => Array a -> exp a -> ProgramT instr (Param2 exp pred) m ()
resetArray a rst = singleInj $ ResetArray a rst

--------------------------------------------------------------------------------
-- ** Virtual arrays.

-- | Create an initialized named virtual array.
initNamedVArray :: (VArrayCMD :<: instr, pred a)
  => String -> [a] -> ProgramT instr (Param2 exp pred) m (VArray a)  
initNamedVArray name = singleInj . InitVArray (Base name)

-- | Create an initialized virtual array.
initVArray :: (VArrayCMD :<: instr, pred a)
  => [a] -> ProgramT instr (Param2 exp pred) m (VArray a)
initVArray = initNamedVArray "a"

-- | Create an uninitialized named virtual array.
newNamedVArray :: (VArrayCMD :<: instr, pred a)
  => String -> exp Integer -> ProgramT instr (Param2 exp pred) m (VArray a)
newNamedVArray name = singleInj . NewVArray (Base name)

-- | Create an uninitialized virtual array.
newVArray :: (VArrayCMD :<: instr, pred a)
  => exp Integer -> ProgramT instr (Param2 exp pred) m (VArray a)
newVArray = newNamedVArray "a"

-- | Get an element of an array.
getVArray :: (VArrayCMD :<: instr, pred a, PredicateExp exp a, FreeExp exp, Monad m)
  => VArray a -> exp Integer -> ProgramT instr (Param2 exp pred) m (exp a)
getVArray a i = fmap valToExp $ singleInj $ GetVArray a i

-- | Set an element of an array.
setVArray :: (VArrayCMD :<: instr, pred a)
  => VArray a -> exp Integer -> exp a -> ProgramT instr (Param2 exp pred) m ()
setVArray a i = singleInj . SetVArray a i

-- | Copy a slice of one array to another.
copyVArray :: (VArrayCMD :<: instr, pred a)
  => (VArray a, exp Integer) -- ^ destination and its offset.
  -> (VArray a, exp Integer) -- ^ source and its offset.
  -> exp Integer             -- ^ number of elements to copy.
  -> ProgramT instr (Param2 exp pred) m ()
copyVArray dest src = singleInj . CopyVArray dest src

-- | Freeze a mutable array into an immutable one by copying.
freezeVArray :: (VArrayCMD :<: instr, pred a, Num (exp Integer), Monad m)
  => VArray a -> exp Integer -> ProgramT instr (Param2 exp pred) m (IArray a)
freezeVArray array len =
  do copy <- newVArray len
     copyVArray (copy,0) (array,0) len
     unsafeFreezeVArray copy

-- | Thaw an immutable array into a mutable one by copying.
thawVArray :: (VArrayCMD :<: instr, pred a, Num (exp Integer), Monad m)
  => IArray a -> exp Integer -> ProgramT instr (Param2 exp pred) m (VArray a)
thawVArray iarray len =
  do array <- unsafeThawVArray iarray
     copy  <- newVArray len
     copyVArray (copy,0) (array,0) len
     return copy

-- | Freeze a mutable variable array to an immuatable one without making a copy.
unsafeFreezeVArray :: (VArrayCMD :<: instr, pred a)
  => VArray a -> ProgramT instr (Param2 exp pred) m (IArray a)
unsafeFreezeVArray = singleInj . UnsafeFreezeVArray

-- | Thaw an immutable array to a mutable variable array without making a copy.
unsafeThawVArray :: (VArrayCMD :<: instr, pred a)
  => IArray a -> ProgramT instr (Param2 exp pred) m (VArray a)
unsafeThawVArray = singleInj . UnsafeThawVArray

--------------------------------------------------------------------------------
-- ** Looping.

-- | For loop, but guarantees the range is a known constant.
iterate
  :: ( LoopCMD :<: instr
     , pred Integer, PredicateExp exp Integer
     , FreeExp exp
     , Primary exp
     , Monad m
     )
  => Integer -- ^ Lower bound
  -> Integer -- ^ Upper bound
  -> (exp Integer -> ProgramT instr (Param2 exp pred) m ()) -- ^ Loop body
  -> ProgramT instr (Param2 exp pred) m ()
iterate lower upper = for (value lower) (value upper)

-- | For loop.
for :: (LoopCMD :<: instr, pred Integer, PredicateExp exp Integer, FreeExp exp, Monad m)
  => exp Integer -- ^ Lower bound
  -> exp Integer -- ^ Upper bound
  -> (exp Integer -> ProgramT instr (Param2 exp pred) m ()) -- ^ Loop body
  -> ProgramT instr (Param2 exp pred) m ()
for lower upper body = singleInj $ For lower upper (body . valToExp)

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
iff :: (ConditionalCMD :<: instr, pred Bool)
  => exp Bool
  -> ProgramT instr (Param2 exp pred) m ()
  -> ProgramT instr (Param2 exp pred) m ()
  -> ProgramT instr (Param2 exp pred) m ()
iff b t e = conditional (b, t) [] (Just e)

ifE :: (ConditionalCMD :<: instr, pred Bool)
  => (exp Bool, ProgramT instr (Param2 exp pred) m ())
  -> (exp Bool, ProgramT instr (Param2 exp pred) m ())
  -> ProgramT instr (Param2 exp pred) m ()
ifE a b = conditional a [b] (Nothing)

--------------------------------------------------------------------------------

switch :: (ConditionalCMD :<: instr, pred a, Eq a, Ord a)
  => exp a
  -> [When a (ProgramT instr (Param2 exp pred) m)]
  -> ProgramT instr (Param2 exp pred) m ()
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
  => a
  -> ProgramT instr (Param2 exp pred) m ()
  -> When a (ProgramT instr (Param2 exp pred) m)
is a = When (Is a) 

to :: (Ord a, pred a)
  => a
  -> a
  -> ProgramT instr (Param2 exp pred) m ()
  -> When a (ProgramT instr (Param2 exp pred) m)
to l h = When (To l h)

--------------------------------------------------------------------------------
-- ** Processes.

type Sig  instr exp pred m = Signature (Param3 (ProgramT instr (Param2 exp pred) m) exp pred)
type Comp instr exp pred m = Component (Param3 (ProgramT instr (Param2 exp pred) m) exp pred)

-- | Declare a named component with named clock and reset signals.
clockedComponent :: (ComponentCMD :<: instr, Monad m)
  => String
  -> String
  -> String
  -> Sig instr exp pred m a
  -> ProgramT instr (Param2 exp pred) m (Comp instr exp pred m a)
clockedComponent name clock reset sig =
  do (name, args) <- singleInj $
       DeclareComponent (Base name) (Exact clock) (Exact reset) sig
     return $ Component name args sig

-- | Declare a named component.
namedComponent :: (ComponentCMD :<: instr, Monad m)
  => String
  -> Sig instr exp pred m a
  -> ProgramT instr (Param2 exp pred) m (Comp instr exp pred m a)
namedComponent name = clockedComponent name "clk" "rst"

-- | Declare a component.
component :: (ComponentCMD :<: instr, Monad m)
  => Sig instr exp pred m a
  -> ProgramT instr (Param2 exp pred) m (Comp instr exp pred m a)
component = namedComponent "comp"

-- | Call a component.
portmap :: (ComponentCMD :<: instr)
  => Comp instr exp pred m a
  -> Argument pred a
  -> ProgramT instr (Param2 exp pred) m ()
portmap pro arg = singleInj $ PortMap pro arg

-- | Empty argument.
nil :: Argument pred ()
nil = Nil

-- | Add signal to a argument.
(+:) :: (pred a, Integral a, PrimType a)
  => Signal a
  -> Argument pred b
  -> Argument pred (Signal a -> b)
(+:) x xs = ASig x xs

(++:) :: (pred a, Integral a, PrimType a, pred Integer)
  => Array a
  -> Argument pred b
  -> Argument pred (Array a -> b)
(++:) x xs = AArr x xs

infixr +:, ++:

--------------------------------------------------------------------------------

ret :: ProgramT instr (Param2 exp pred) m () -> Sig instr exp pred m ()
ret = Ret

--------------------------------------------------------------------------------

exactInput :: (pred a, Integral a, PrimType a)
  => String
  -> (Signal a -> Sig instr exp pred m b)
  -> Sig instr exp pred m (Signal a -> b)
exactInput  n = SSig (Exact n) In

namedInput :: (pred a, Integral a, PrimType a)
  => String
  -> (Signal a -> Sig instr exp pred m b)
  -> Sig instr exp pred m (Signal a -> b)
namedInput n = SSig (Base n) In

input :: (pred a, Integral a, PrimType a)
  => (Signal a -> Sig instr exp pred m b)
  -> Sig instr exp pred m (Signal a -> b)
input = namedInput "in"

--------------------------------------------------------------------------------

exactInputArray :: (pred a, Integral a, PrimType a, pred Integer)
  => String
  -> Integer
  -> (Array a -> Sig instr exp pred m b)
  -> Sig instr exp pred m (Array a -> b)
exactInputArray  n i = SArr (Exact n) In i

namedInputArray :: (pred a, Integral a, PrimType a, pred Integer)
  => String
  -> Integer
  -> (Array a -> Sig instr exp pred m b)
  -> Sig instr exp pred m (Array a -> b)
namedInputArray n = SArr (Base n) In

inputArray :: (pred a, Integral a, PrimType a, pred Integer)
  => Integer
  -> (Array a -> Sig instr exp pred m b)
  -> Sig instr exp pred m (Array a -> b)
inputArray = namedInputArray "in"

--------------------------------------------------------------------------------

exactOutput :: (pred a, Integral a, PrimType a)
  => String
  -> (Signal a -> Sig instr exp pred m b)
  -> Sig instr exp pred m (Signal a -> b)
exactOutput n = SSig (Exact n) Out

namedOutput :: (pred a, Integral a, PrimType a)
  => String
  -> (Signal a -> Sig instr exp pred m b)
  -> Sig instr exp pred m (Signal a -> b)
namedOutput n = SSig (Base n) Out

output :: (pred a, Integral a, PrimType a)
  => (Signal a -> Sig instr exp pred m b)
  -> Sig instr exp pred m (Signal a -> b)
output = namedOutput "out"

--------------------------------------------------------------------------------

exactOutputArray :: (pred a, Integral a, PrimType a, pred Integer)
  => String
  -> Integer
  -> (Array a -> Sig instr exp pred m b)
  -> Sig instr exp pred m (Array a -> b)
exactOutputArray  n = SArr (Exact n) Out

namedOutputArray :: (pred a, Integral a, PrimType a, pred Integer)
  => String
  -> Integer
  -> (Array a -> Sig instr exp pred m b)
  -> Sig instr exp pred m (Array a -> b)
namedOutputArray n = SArr (Base n) Out

outputArray ::
     ( pred a, Integral a, PrimType a, pred Integer)
  => Integer
  -> (Array a -> Sig instr exp pred m b)
  -> Sig instr exp pred m (Array a -> b)
outputArray = namedOutputArray "out"

--------------------------------------------------------------------------------
-- ** Structural entities.

processR :: (ProcessCMD :<: instr)
  => Signals                               -- ^ Other triggers.
  -> ProgramT instr (Param2 exp pred) m () -- ^ Reset program.
  -> ProgramT instr (Param2 exp pred) m () -- ^ Main program.
  -> ProgramT instr (Param2 exp pred) m ()
processR is rst prog = singleInj $ Process is rst (Just prog)

process :: (ProcessCMD :<: instr)
  => Signals                               -- ^ Other triggers.
  -> ProgramT instr (Param2 exp pred) m () -- ^ Main program.
  -> ProgramT instr (Param2 exp pred) m ()
process is prog = singleInj $ Process is prog Nothing

-- | Construct the untyped signal list for processes.
(.:) :: ToIdent a => a -> Signals -> Signals
(.:) x xs = toIdent x : xs

infixr .:

--------------------------------------------------------------------------------
-- ** VHDL specific instructions.
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- *** Bit operations.

copyBits :: (VHDLCMD :<: instr, pred a, pred b)
  => (Signal a, exp Integer)
  -> (Signal b, exp Integer)
  -> exp Integer
  -> ProgramT instr (Param2 exp pred) m ()
copyBits a b l = singleInj (CopyBits a b l)

copyVBits :: (VHDLCMD :<: instr, pred a, pred b)
  => (Variable a, exp Integer)
  -> (Signal   b, exp Integer)
  -> exp Integer
  -> ProgramT instr (Param2 exp pred) m ()
copyVBits a b l = singleInj (CopyVBits a b l)

copyABits :: (VHDLCMD :<: instr, pred a, pred b)
  => (Array  a, exp Integer, exp Integer)
  -> (Signal b, exp Integer)
  -> exp Integer
  -> ProgramT instr (Param2 exp pred) m ()
copyABits a b l = singleInj (CopyABits a b l)

getBit :: (VHDLCMD :<: instr, pred a, pred Bit, PredicateExp exp Bit, FreeExp exp, Monad m)
  => Signal a -> exp Integer -> ProgramT instr (Param2 exp pred) m (exp Bit)
getBit bits ix = fmap valToExp $ singleInj $ GetBit bits ix

setBit :: (VHDLCMD :<: instr, pred a, pred Bit)
  => Signal a -> exp Integer -> exp Bit -> ProgramT instr (Param2 exp pred) m ()
setBit bits ix bit = singleInj $ SetBit bits ix bit

getBits :: (VHDLCMD :<: instr, pred Integer, pred (Bits u), PredicateExp exp Integer, FreeExp exp, Monad m)
  => Signal (Bits u) -> exp Integer -> exp Integer
  -> ProgramT instr (Param2 exp pred) m (exp Integer)
getBits a l u = fmap valToExp $ singleInj $ GetBits a l u

-- todo: these bit operations really do not have to be over just `Bits`, since
--       VHDL treats all of our types as bit vectors anyway.
--------------------------------------------------------------------------------
