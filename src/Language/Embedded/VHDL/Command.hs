{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Language.Embedded.VHDL.Command
  ( Clause
  , Scope
  , ToIdent(..)

    -- ^ Signals.
  , Signal
  , SignalCMD
  , newSignal
  , newSignal_
  , getSignal
  , setSignal
  , (<==)
  , newPort
  , newGeneric

    -- ^ Variables.
  , Variable
  , VariableCMD
  , newVariable
  , newVariable_
  , getVariable
  , setVariable
  , (==:)

    -- ^ Entities.
  , EntityCMD
  , newEntity

    -- ^ Architectures.
  , ArchitectureCMD
  , newArchitecture

    -- ^ Processes and untyped signals.
  , SignalX
  , ProcessCMD
  , toX
  , newProcess

    -- ^ Conditionals.
  , ConditionalCMD
  , conditional
  , when
  , iff
  ) where

import Language.VHDL (Identifier(..), Mode(..), Expression)
import qualified Language.VHDL as V

import Language.Embedded.VHDL.Monad      (VHDLT, VHDL)
import Language.Embedded.VHDL.Monad.Type (Type, Kind)
import Language.Embedded.VHDL.Interface
import qualified Language.Embedded.VHDL.Monad            as M
import qualified Language.Embedded.VHDL.Monad.Type       as T
import qualified Language.Embedded.VHDL.Monad.Expression as M
import qualified Language.Embedded.VHDL.Expression.Hoist as H

import Control.Arrow (second)
import Control.Monad.Identity           hiding (when)
import Control.Monad.Operational.Higher hiding (when)
import Control.Applicative
import Data.Typeable
import Data.ALaCarte
import Data.Ix

--------------------------------------------------------------------------------
-- * ...
--------------------------------------------------------------------------------

data Clause   = Port    | Generic

data Scope    = Process | Architecture | Entity

class ToIdent a
  where
    toIdent :: a -> V.Identifier

instance ToIdent String where toIdent = V.Ident

--------------------------------------------------------------------------------
-- ** ...

compEM :: forall exp a. (PredicateExp exp a, CompileExp exp) => Maybe (exp a) -> VHDL (Maybe Expression)
compEM e = maybe (return Nothing) (>>= return . Just) $ fmap compE e

compTM :: forall exp a. (PredicateExp exp a, CompileExp exp) => Maybe (exp a) -> VHDL Type
compTM _ = compT (undefined :: exp a)

--------------------------------------------------------------------------------
-- * ... Signals
--------------------------------------------------------------------------------

-- | ...
data Signal a = Signal Integer

instance ToIdent (Signal a)
  where
    toIdent (Signal i) = Ident $ 's' : show i

-- | ...
data SignalCMD (exp :: * -> *) (prog :: * -> *) a
  where
    NewSignal
      :: PredicateExp exp a
      => Clause
      -> Scope
      -> Mode
      -> Maybe (exp a)
      -> SignalCMD exp prog (Signal a)

    GetSignal
      :: PredicateExp exp a
      => Signal a
      -> SignalCMD exp prog (exp a)

    SetSignal
      :: PredicateExp exp a
      => Signal a
      -> exp a
      -> SignalCMD exp prog ()

--------------------------------------------------------------------------------
-- ** ...

type instance IExp (SignalCMD e)       = e
type instance IExp (SignalCMD e :+: i) = e

instance HFunctor (SignalCMD exp)
  where
    hfmap _ (NewSignal c s m e) = NewSignal c s m e
    hfmap _ (GetSignal s)       = GetSignal s
    hfmap _ (SetSignal s e)     = SetSignal s e

instance CompileExp exp => Interp (SignalCMD exp) VHDL
  where
    interp = compileSignal

-- | ...
compileSignal :: forall exp a. CompileExp exp => SignalCMD exp VHDL a -> VHDL a
compileSignal (NewSignal clause scope mode exp) =
  do v <- compEM exp
     t <- compTM exp
     i <- Signal <$> M.freshUnique
     let block     = M.declSignal      (toIdent i)      t v
         interface = M.interfaceSignal (toIdent i) mode t v
     case scope of
       Process      -> M.addLocal  block
       Architecture -> M.addGlobal block
       Entity    -> case clause of
         Port    -> M.addPort    interface
         Generic -> M.addGeneric interface
     return i
compileSignal (GetSignal (Signal s)) =
  do undefined -- Don't know what to put here, something like '... (varE s)' but that loses the 's'..
compileSignal (SetSignal s exp) =
  do M.addSequential =<< M.assignSequentialSignal (toIdent s) <$> compE exp

--------------------------------------------------------------------------------
-- ** ..

-- | Declare a signal.
newSignal  :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a) => IExp i a -> ProgramT i m (Signal a)
newSignal  = singleE . NewSignal Port Architecture InOut . Just

-- | Declare an uninitialized signal.
newSignal_ :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a) => ProgramT i m (Signal a)
newSignal_ = singleE $ NewSignal Port Architecture InOut Nothing

-- | Fetches the current value of a signal.
getSignal :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a) => Signal a -> ProgramT i m (IExp i a)
getSignal = singleE . GetSignal

-- | Update the value of a signal.
setSignal :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a) => Signal a -> IExp i a -> ProgramT i m ()
setSignal s = singleE . SetSignal s

--------------------------------------------------------------------------------

-- | Short-hand for 'setSignal'.
(<==) :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a) => Signal a -> IExp i a -> ProgramT i m ()
(<==) = setSignal

-- | Declare port/generic signals of the given mode.
newPort, newGeneric :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a) => Mode -> ProgramT i m (Signal a)
newPort    m = singleE $ NewSignal Port    Entity m Nothing
newGeneric m = singleE $ NewSignal Generic Entity m Nothing

--------------------------------------------------------------------------------
-- * ... Variables
--------------------------------------------------------------------------------

data Variable a = Variable Integer

instance ToIdent (Variable a)
  where
    toIdent (Variable i) = Ident $ 'v' : show i

data VariableCMD (exp :: * -> *) (prog :: * -> *) a
  where
    NewVariable
      :: PredicateExp exp a
      => Maybe (exp a)
      -> VariableCMD exp prog (Variable a)

    GetVariable
      :: PredicateExp exp a
      => Variable a
      -> VariableCMD exp prog (exp a)

    SetVariable
      :: PredicateExp exp a
      => Variable a
      -> exp a
      -> VariableCMD exp prog ()

--------------------------------------------------------------------------------
-- ** ...

type instance IExp (VariableCMD e)       = e
type instance IExp (VariableCMD e :+: i) = e

instance HFunctor (VariableCMD exp)
  where
    hfmap _ (NewVariable e)   = NewVariable e
    hfmap _ (GetVariable s)   = GetVariable s
    hfmap _ (SetVariable s e) = SetVariable s e

instance CompileExp exp => Interp (VariableCMD exp) VHDL
  where
    interp = compileVariable

compileVariable :: forall exp a. CompileExp exp => VariableCMD exp VHDL a -> VHDL a
compileVariable (NewVariable exp) =
  do v <- compEM exp
     t <- compTM exp
     i <- Variable <$> M.freshUnique
     M.addLocal $ M.declVariable (toIdent i) t v
     return i
compileVariable (GetVariable v) =
  do undefined -- ... same as for signals
compileVariable (SetVariable v exp) =
  do M.addSequential =<< M.assignVariable (toIdent v) <$> compE exp

--------------------------------------------------------------------------------
-- ** ...

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

--------------------------------------------------------------------------------

-- | Short-hand for 'setVariable'.
(==:) :: (VariableCMD (IExp i) :<: i, PredicateExp (IExp i) a) => Variable a -> IExp i a -> ProgramT i m ()
(==:) = setVariable

--------------------------------------------------------------------------------
-- * ... Arrays
--------------------------------------------------------------------------------

-- | ...
data Array i a = Array Integer

instance ToIdent (Array i a)
  where
    toIdent (Array i) = Ident $ 'a' : show i

data ArrayCMD (exp :: * -> *) (prog :: * -> *) a
  where
    -- ^ Creates an array of given length.
    NewArray
      :: ( PredicateExp exp a
         , PredicateExp exp i
         , Integral i
         , Ix i )
      => exp i -> ArrayCMD exp prog (Array i a)

    -- ^ Creates an array with a unspecified length.
    NewArray_
      :: ( PredicateExp exp a
         , PredicateExp exp i
         , Integral i
         , Ix i )
      => ArrayCMD exp prog (Array i a)

    -- ^ Creates an array from the given list of elements.
    InitArray
      :: ( PredicateExp exp a
         , PredicateExp exp i
         , Integral i
         , Ix i )
      => [a] -> ArrayCMD exp prog (Array i a)

    -- ^ ...
    GetArray
      :: ( PredicateExp exp a
         , PredicateExp exp i
         , Integral i
         , Ix i )
      => exp i -> Array i a -> ArrayCMD exp prog (exp a)

    -- ^ ...
    SetArray
      :: ( PredicateExp exp a
         , PredicateExp exp i
         , Integral i
         , Ix i )
      => exp i -> exp a -> Array i a -> ArrayCMD exp prog ()

    -- ^ ...
    CopyArray
      :: ( PredicateExp exp a
         , PredicateExp exp i
         , Integral i
         , Ix i )
      => Array i a -> Array i a -> exp i -> ArrayCMD exp prog ()

    -- ^ ..
    UnsafeGetArray
      :: ( PredicateExp exp a
         , PredicateExp exp i
         , Integral i
         , Ix i )
      => exp i -> Array i a -> ArrayCMD exp prog (exp a)

--------------------------------------------------------------------------------
-- ** ...

type instance IExp (ArrayCMD e)       = e
type instance IExp (ArrayCMD e :+: i) = e

instance HFunctor (ArrayCMD exp)
  where
    hfmap _ (NewArray i)         = NewArray i
    hfmap _ (NewArray_)          = NewArray_
    hfmap _ (InitArray is)       = InitArray is
    hfmap _ (GetArray i a)       = GetArray i a
    hfmap _ (SetArray i e a)     = SetArray i e a
    hfmap _ (CopyArray a b i)    = CopyArray a b i
    hfmap _ (UnsafeGetArray i a) = UnsafeGetArray i a

instance (CompileExp exp, EvaluateExp exp) => Interp (ArrayCMD exp) VHDL
  where
    interp = compileArray

compileArray :: forall exp a. (CompileExp exp, EvaluateExp exp) => ArrayCMD exp VHDL a -> VHDL a
compileArray (NewArray len) =
  do n <- compE  len
     t <- compTA len (undefined :: a)
     a <- freshA
     i <- Array <$> M.freshUnique
     let arr = M.constrainedArray a t (M.downtoZero n)
     M.addType arr
     M.addLocal $ M.declVariable (toIdent i) (M.typeName arr) Nothing
     return i
compileArray (NewArray_) =
  do t <- compTA (undefined :: exp i) (undefined :: a)
     a <- freshA
     i <- Array <$> M.freshUnique
     let arr = M.unconstrainedArray a t
     M.addType arr
     M.addLocal $ M.declVariable (toIdent i) (M.typeName arr) (error "what range!?")
     return i
compileArray (InitArray is) =
  do t <- compTA (undefined :: exp i) (undefined :: a)
     a <- freshA
     i <- Array <$> M.freshUnique
     x <- sequence [compE (litE a :: exp b) | (a :: b) <- is]
     let len = M.downtoZero . H.lift . M.lit . show $ length is
         arr = M.constrainedArray a t len
     M.addType arr
     M.addLocal $ M.declVariable (toIdent i) (M.typeName arr) (Just $ H.lift $ M.aggregate x)
     return i

freshA :: VHDL Identifier
freshA = toIdent . ('t' :) . show <$> M.freshUnique

compTA :: forall exp i a. (PredicateExp exp a, CompileExp exp) => exp i -> Array i a -> VHDL Type
compTA _ _ = compT (undefined :: exp a)

--------------------------------------------------------------------------------
-- * ...



--------------------------------------------------------------------------------
-- * ... Packages
--------------------------------------------------------------------------------

-- ...

--------------------------------------------------------------------------------
-- * ... Entities
--------------------------------------------------------------------------------

-- | ...
data EntityCMD (exp :: * -> *) (prog :: * -> *) a
  where
    NewEntity
      :: String
      -> prog a
      -> EntityCMD exp prog a

--------------------------------------------------------------------------------
-- ** ...

type instance IExp (EntityCMD e)       = e
type instance IExp (EntityCMD e :+: i) = e

instance HFunctor (EntityCMD exp)
  where
    hfmap f (NewEntity e p) = NewEntity e (f p)

instance CompileExp exp => Interp (EntityCMD exp) VHDL
  where
    interp = compileEntity

compileEntity :: forall exp a. CompileExp exp => EntityCMD exp VHDL a -> VHDL a
compileEntity (NewEntity s prog) = M.entity (Ident s) prog

--------------------------------------------------------------------------------
-- ** ...

-- | Declare a new entity by running the given program to initialize ports.
newEntity :: (EntityCMD (IExp i) :<: i) => String -> ProgramT i m a -> ProgramT i m a
newEntity e = singleE . NewEntity e

--------------------------------------------------------------------------------
-- * ... Architectures
--------------------------------------------------------------------------------

-- | ...
data ArchitectureCMD (exp :: * -> *) (prog :: * -> *) a
  where
    NewArchitecture
      :: String
      -> String
      -> prog a
      -> ArchitectureCMD exp prog a

--------------------------------------------------------------------------------
-- ** ...

type instance IExp (ArchitectureCMD e)       = e
type instance IExp (ArchitectureCMD e :+: i) = e

instance HFunctor (ArchitectureCMD exp)
  where
    hfmap f (NewArchitecture e a p) = NewArchitecture e a (f p)

instance CompileExp exp => Interp (ArchitectureCMD exp) VHDL
  where
    interp = compileArchitecture

compileArchitecture :: forall exp a. CompileExp exp => ArchitectureCMD exp VHDL a -> VHDL a
compileArchitecture (NewArchitecture e a prog) = M.architecture (Ident e) (Ident a) prog

--------------------------------------------------------------------------------
-- ** ...

-- | Declare a new architecture by running the given program to produce a body.
newArchitecture :: (ArchitectureCMD (IExp i) :<: i) => String -> String -> ProgramT i m a -> ProgramT i m a
newArchitecture e a = singleE . NewArchitecture e a

--------------------------------------------------------------------------------
-- * ... Processes
--------------------------------------------------------------------------------

-- | Untyped signals.
data SignalX = forall a. SignalX (Signal a)

-- | ...
data ProcessCMD (exp :: * -> *) (prog :: * -> *) a
  where
    NewProcess
      :: [SignalX]
      -> prog ()
      -> ProcessCMD exp prog ()

--------------------------------------------------------------------------------
-- ** ...

type instance IExp (ProcessCMD e)       = e
type instance IExp (ProcessCMD e :+: i) = e

instance HFunctor (ProcessCMD exp)
  where
    hfmap f (NewProcess is p) = NewProcess is (f p)

instance CompileExp exp => Interp (ProcessCMD exp) VHDL
  where
    interp = compileProcess

compileProcess :: forall exp a. CompileExp exp => ProcessCMD exp VHDL a -> VHDL a
compileProcess (NewProcess is prog) =
  do l      <- M.newLabel
     (a, c) <- M.inProcess l (fmap unX is) prog
     M.addConcurrent (V.ConProcess c)
     return a
  where
    unX :: SignalX -> Identifier
    unX (SignalX s) = toIdent s

--------------------------------------------------------------------------------
-- ** ...

-- | ...
toX :: Signal a -> SignalX
toX = SignalX

-- | ...
newProcess :: (ProcessCMD (IExp i) :<: i) => [SignalX] -> ProgramT i m () -> ProgramT i m ()
newProcess is = singleE . NewProcess is

--------------------------------------------------------------------------------
-- * ... Conditionals
--------------------------------------------------------------------------------

-- | ...
data ConditionalCMD (exp :: * -> *) (prog :: * -> *) a
  where
    If :: PredicateExp exp Bool
       => (exp Bool, prog ())   -- if
       -> [(exp Bool, prog ())] -- else-if
       -> Maybe (prog ())       -- else
       -> ConditionalCMD exp prog ()

--------------------------------------------------------------------------------
-- ** ...

type instance IExp (ConditionalCMD e)       = e
type instance IExp (ConditionalCMD e :+: i) = e

instance HFunctor (ConditionalCMD exp)
  where
    hfmap f (If a cs b) = If (fmap f a) (fmap (fmap f) cs) (fmap f b)

instance CompileExp exp => Interp (ConditionalCMD exp) VHDL
  where
    interp = compileConditional

compileConditional :: forall exp a. CompileExp exp => ConditionalCMD exp VHDL a -> VHDL a
compileConditional (If (a, b) cs em) =
  do let (es, ds) = unzip cs
         el       = maybe (return ()) id em
     ae  <- compE a
     ese <- mapM compE es
     s   <- M.inConditional (ae, b) (zip ese ds) el
     M.addSequential $ V.SIf s

--------------------------------------------------------------------------------
-- ** ...

-- | Conditional statements guarded by if and then clauses with an optional else.
conditional
  :: ( ConditionalCMD (IExp i) :<: i
     , PredicateExp   (IExp i) Bool
     )
  =>  (IExp i Bool, ProgramT i m ())
  -> [(IExp i Bool, ProgramT i m ())]
  -> Maybe (ProgramT i m ())
  -> ProgramT i m ()
conditional a bs = singleE . If a bs

-- | Guarded statement.
when
  :: ( ConditionalCMD (IExp i) :<: i
     , PredicateExp   (IExp i) Bool
     )
  => IExp i Bool
  -> ProgramT i m ()
  -> ProgramT i m ()
when e p = conditional (e, p) [] Nothing

-- | Standard if-then-else statement.
iff
  :: ( ConditionalCMD (IExp i) :<: i
     , PredicateExp   (IExp i) Bool
     )
  => IExp i Bool
  -> ProgramT i m ()
  -> ProgramT i m ()
  -> ProgramT i m ()
iff b t e = conditional (b, t) [] (Just e)

--------------------------------------------------------------------------------
