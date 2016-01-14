{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Language.Embedded.VHDL.Command
  ( compile
  , icompile
  , ToIdent(..)

    -- ^ Signals.
  , Signal
  , SignalCMD(..)
  , Scope
  , Clause
  , newSignal
  , newSignal_
  , getSignal
  , setSignal
  , (<==)
  , newPort
  , newPort_
  , newGeneric
  , newGeneric_

    -- ^ Variables.
  , Variable
  , VariableCMD(..)
  , newVariable
  , newVariable_
  , getVariable
  , setVariable
  , (==:)

    -- ^ Arrays.
  , Array
  , ArrayCMD(..)
  , CompArrayIx(..)
  , newArray
  , initArray
  , getArray
  , setArray
  , unsafeGetArray

    -- ^ Loops.
  , LoopCMD(..)
  , for
    
    -- ^ Entities.
  , EntityCMD(..)
  , newEntity

    -- ^ Architectures.
  , ArchitectureCMD(..)
  , newArchitecture

    -- ^ Processes and untyped signals.
  , SignalX
  , ProcessCMD(..)
  , toX
  , newProcess

    -- ^ Conditionals.
  , ConditionalCMD(..)
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
import Data.Array.IO (IOArray)
import Data.IORef    (IORef)
import Data.Typeable
import Data.ALaCarte
import Data.Ix
import qualified Data.Array.IO as IA
import qualified Data.IORef    as IR

--------------------------------------------------------------------------------
-- * ...
--------------------------------------------------------------------------------

-- | Compile a program to VHDL code represented as a string.
compile :: (Interp instr VHDL, HFunctor instr) => Program instr a -> String
compile = show . M.prettyVHDL . interpret

-- | Compile a program to VHDL code and print it on the screen.
icompile :: (Interp instr VHDL, HFunctor instr) => Program instr a -> IO ()
icompile = putStrLn . compile

--------------------------------------------------------------------------------
-- ** ...

compEM :: forall exp a. (PredicateExp exp a, CompileExp exp) => Maybe (exp a) -> VHDL (Maybe Expression)
compEM e = maybe (return Nothing) (>>= return . Just) $ fmap compE e

compTM :: forall exp a. (PredicateExp exp a, CompileExp exp) => Maybe (exp a) -> VHDL Type
compTM _ = compT (undefined :: exp a)

evalEM :: forall exp a. (PredicateExp exp a, EvaluateExp exp) => Maybe (exp a) -> a
evalEM e = maybe (error "empty value") (id) $ fmap evalE e

--------------------------------------------------------------------------------
-- ** ..

class ToIdent a
  where
    toIdent :: a -> V.Identifier

instance ToIdent String
  where
    toIdent = V.Ident

freshVar :: forall exp a. (CompileExp exp, PredicateExp exp a) => VHDL (exp a, Identifier)
freshVar = do
  i <- varE <$> M.freshUnique :: VHDL (exp a)
  n <- dig  <$> compE i
  t <- compT (undefined :: exp a)
  M.addLocal $ M.declVariable n t Nothing
  return (i, n)

freshVar_ :: forall exp a. (CompileExp exp, PredicateExp exp a) => VHDL (exp a)
freshVar_ = fst <$> freshVar

-- diggity dig!
dig :: V.Expression -> V.Identifier
dig (V.ENand
      (V.Relation
        (V.ShiftExpression
          (V.SimpleExpression _
            (V.Term
              (V.FacPrim
                (V.PrimName
                  (V.NSimple i))
     _)_)_)_)_)_) = i

--------------------------------------------------------------------------------
-- * ... Signals
--------------------------------------------------------------------------------

-- | If a signal is declared with a scope of 'Entity' its classified as either a
--   port or generic signal.
data Clause   = Port    | Generic
  deriving (Show)

-- | Scope of a signal.
data Scope    = Process | Architecture | Entity
  deriving (Show)

-- | ...
data Signal a = SignalC Integer | SignalE (IORef a)

instance ToIdent (Signal a)
  where
    toIdent (SignalC i) = Ident $ 's' : show i

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

instance CompileExp  exp => Interp (SignalCMD exp) VHDL where interp = compileSignal
instance EvaluateExp exp => Interp (SignalCMD exp) IO   where interp = runSignal

-- | ...
compileSignal :: forall exp a. CompileExp exp => SignalCMD exp VHDL a -> VHDL a
compileSignal (NewSignal clause scope mode exp) =
  do v <- compEM exp
     t <- compTM exp
     i <- SignalC <$> M.freshUnique
     let block     = M.declSignal      (toIdent i)      t v
         interface = M.interfaceSignal (toIdent i) mode t v
     case scope of
       Process      -> M.addLocal  block
       Architecture -> M.addGlobal block
       Entity    -> case clause of
         Port    -> M.addPort    interface
         Generic -> M.addGeneric interface
     return i
compileSignal (GetSignal s) =
  do (v, i) <- freshVar :: VHDL (a, Identifier)
     e <- compE v
     M.addSequential $ M.assignVariable i (H.lift $ V.PrimName $ V.NSimple $ toIdent s)
     return v
compileSignal (SetSignal s exp) =
  do M.addSequential =<< M.assignSequentialSignal (toIdent s) <$> compE exp

-- | ...
runSignal :: forall exp prog a. EvaluateExp exp => SignalCMD exp prog a -> IO a
runSignal (NewSignal _ _ _ exp)        = fmap SignalE $ IR.newIORef $ evalEM exp
runSignal (GetSignal (SignalE r))      = fmap litE $ IR.readIORef r
runSignal (SetSignal (SignalE r) exp)  = IR.writeIORef r $ evalE exp

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

-- | Declare port signals of the given mode and assign it initial value.
newPort :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a) => Mode -> IExp i a -> ProgramT i m (Signal a)
newPort m = singleE . NewSignal Port Entity m . Just

-- | Declare port signals of the given mode.
newPort_ :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a) => Mode -> ProgramT i m (Signal a)
newPort_ m = singleE $ NewSignal Port Entity m Nothing

--------------------------------------------------------------------------------

-- | Declare generic signals of the given mode and assign it initial value.
newGeneric :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a) => Mode -> IExp i a -> ProgramT i m (Signal a)
newGeneric m = singleE . NewSignal Generic Entity m . Just

-- | Declare generic signals of the given mode.
newGeneric_ :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a) => Mode -> ProgramT i m (Signal a)
newGeneric_ m = singleE $ NewSignal Generic Entity m Nothing

--------------------------------------------------------------------------------

-- | Short-hand for 'setSignal'.
(<==) :: (SignalCMD (IExp i) :<: i, PredicateExp (IExp i) a) => Signal a -> IExp i a -> ProgramT i m ()
(<==) = setSignal

--------------------------------------------------------------------------------
-- * ... Variables
--------------------------------------------------------------------------------

data Variable a = VariableC Integer | VariableE (IORef a)

instance ToIdent (Variable a)
  where
    toIdent (VariableC i) = Ident $ 'v' : show i

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

instance CompileExp  exp => Interp (VariableCMD exp) VHDL where interp = compileVariable
instance EvaluateExp exp => Interp (VariableCMD exp) IO   where interp = runVariable

-- | ...
compileVariable :: forall exp a. CompileExp exp => VariableCMD exp VHDL a -> VHDL a
compileVariable (NewVariable exp) =
  do v <- compEM exp
     t <- compTM exp
     i <- VariableC <$> M.freshUnique
     M.addLocal $ M.declVariable (toIdent i) t v
     return i
compileVariable (GetVariable var) =
  do (v, i) <- freshVar :: VHDL (a, Identifier)
     e <- compE v
     M.addSequential $ M.assignVariable i (H.lift $ V.PrimName $ V.NSimple $ toIdent var)
     return v
compileVariable (SetVariable var exp) =
  do M.addSequential =<< M.assignVariable (toIdent var) <$> compE exp

-- | ...
runVariable :: forall exp prog a. EvaluateExp exp => VariableCMD exp prog a -> IO a
runVariable (NewVariable exp)               = fmap VariableE $ IR.newIORef $ evalEM exp
runVariable (GetVariable (VariableE v))     = fmap litE $ IR.readIORef v
runVariable (SetVariable (VariableE v) exp) = IR.writeIORef v $ evalE exp

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
data Array i a = ArrayC Integer | ArrayE (IOArray i a)

instance ToIdent (Array i a)
  where
    toIdent (ArrayC i) = Ident $ 'a' : show i

class CompArrayIx exp
  where
    compArrayIx :: PredicateExp exp a => exp i -> Array i a -> Maybe (exp a)
    compArrayIx _ _ = Nothing

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

instance (CompileExp exp, EvaluateExp exp, CompArrayIx exp) => Interp (ArrayCMD exp) VHDL where interp = compileArray
instance EvaluateExp exp                                    => Interp (ArrayCMD exp) IO   where interp = runArray

-- | Fresh array type identifier
freshA :: VHDL Identifier
freshA = toIdent . ('t' :) . show <$> M.freshUnique

-- | Compile type of array.
compTA :: forall exp i a. (PredicateExp exp a, CompileExp exp) => exp i -> Array i a -> VHDL Type
compTA _ _ = compT (undefined :: exp a)

-- *** Signal commands can be both sequential and parallel and I shouldn't
--     depend on them always being sequential.
compileArray :: forall exp a. (CompileExp exp, EvaluateExp exp, CompArrayIx exp) => ArrayCMD exp VHDL a -> VHDL a
compileArray (NewArray len) =
  do n <- compE  len
     t <- compTA len (undefined :: a)
     a <- freshA
     i <- ArrayC <$> M.freshUnique
     let arr = M.constrainedArray a t (M.downtoZero n)
     M.addType arr
     M.addLocal $ M.declVariable (toIdent i) (M.typeName arr) Nothing
     return i
compileArray (NewArray_) =
  do error "Needs a range constraint somewhere...even for unconstrained arrays"
compileArray (InitArray is) =
  do t <- compTA (undefined :: exp i) (undefined :: a)
     a <- freshA
     i <- ArrayC <$> M.freshUnique
     x <- sequence [compE (litE a :: exp b) | (a :: b) <- is]
     let len = M.downtoZero . H.lift . M.lit . show $ length is
         arr = M.constrainedArray a t len
     M.addType arr
     M.addLocal $ M.declVariable (toIdent i) (M.typeName arr) (Just $ H.lift $ M.aggregate x)
     return i
compileArray (GetArray ix arr) =
  do (v, i) <- freshVar :: VHDL (a, Identifier)
     e <- compE ix
     M.addSequential $ M.assignVariable i (H.lift $ V.PrimName $ M.index (toIdent arr) e)
     return v
compileArray (SetArray i e arr) =
  do i' <- compE i
     e' <- compE e
     -- this could be concurrent as well.
     M.addSequential $ M.assignArray (M.index (toIdent arr) i') e'
compileArray (CopyArray arr1 arr2 i) =
  do error "Not sure what this would be in VHDL..."
compileArray (UnsafeGetArray ix arr) =
  case compArrayIx ix arr of
      Nothing -> do
        (v, i) <- freshVar :: VHDL (a, Identifier)
        e <- compE ix
        M.addSequential $ M.assignVariable i (H.lift $ V.PrimName $ M.index (toIdent arr) e)
        return v
      Just e -> return e

-- | ...
runArray :: forall exp prog a. EvaluateExp exp => ArrayCMD exp prog a -> IO a
runArray (NewArray len)            = fmap ArrayE $ IA.newArray_ $ (,) 0 $ evalE len
runArray (NewArray_)               = error "?"
runArray (InitArray is)            = fmap ArrayE $ IA.newListArray (0, fromIntegral $ length is) is
runArray (GetArray i (ArrayE a))   = fmap litE $ IA.readArray a $ evalE i
runArray (SetArray i e (ArrayE a)) = IA.writeArray a (evalE i) (evalE e)
runArray (CopyArray (ArrayE l) (ArrayE r) i) = error "?"
runArray (UnsafeGetArray i    (ArrayE a))    = error "?"

--------------------------------------------------------------------------------
-- * ...

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

-- | ...
unsafeGetArray
  :: ( PredicateExp (IExp instr) a
     , PredicateExp (IExp instr) i
     , Integral i, Ix i
     , ArrayCMD (IExp instr) :<: instr )
  => IExp instr i -> Array i a -> ProgramT instr m (IExp instr a)
unsafeGetArray i = singleE . UnsafeGetArray i

--------------------------------------------------------------------------------
-- * Loop statements.
--------------------------------------------------------------------------------

-- | ...
data LoopCMD (exp :: * -> *) (prog :: * -> *) a
  where
    For
      :: (PredicateExp exp n, Integral n)
      => exp n              -- range
      -> (exp n -> prog ()) -- step
      -> LoopCMD exp prog ()

    While
      :: prog (exp Bool)
      -> prog ()
      -> LoopCMD exp prog ()

--------------------------------------------------------------------------------
-- **

type instance IExp (LoopCMD e)       = e
type instance IExp (LoopCMD e :+: i) = e

instance HFunctor (LoopCMD exp)
  where
    hfmap f (For r step)      = For r (f . step)
    hfmap f (While cont step) = While (f cont) (f step)

instance CompileExp  exp => Interp (LoopCMD exp) VHDL where interp = compileLoop
instance EvaluateExp exp => Interp (LoopCMD exp) IO   where interp = runLoop

compileLoop :: forall exp a. CompileExp exp => LoopCMD exp VHDL a -> VHDL a
compileLoop (For r step) =
  do range  <- compE r
     (v, i) <- freshVar
     loop   <- M.inFor i (M.fromZero range) (step v)
     M.addSequential $ V.SLoop $ loop
compileLoop (While b step) =
  do undefined

-- | ...
runLoop :: forall exp prog a. EvaluateExp exp => LoopCMD exp prog a -> IO a
runLoop (For r step) = undefined

--------------------------------------------------------------------------------
-- **

for
  :: ( LoopCMD (IExp instr) :<: instr
     , PredicateExp (IExp instr) n
     , Integral n )
  => IExp instr n
  -> (IExp instr n -> ProgramT instr m ())
  -> ProgramT instr m ()
for range = singleE . For range

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

instance CompileExp exp => Interp (EntityCMD exp) VHDL where interp = compileEntity

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

instance CompileExp exp => Interp (ArchitectureCMD exp) VHDL where interp = compileArchitecture

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

instance CompileExp exp => Interp (ProcessCMD exp) VHDL where interp = compileProcess

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

instance CompileExp exp => Interp (ConditionalCMD exp) VHDL where interp = compileConditional

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
