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
import Language.Embedded.Hardware.Expression.Represent (PrimType, Inhabited, Sized)
import Language.Embedded.Hardware.Expression.Represent.Bit (Bit, Bits)

import Control.Monad.Reader (ReaderT(..), runReaderT, lift)
import Control.Monad.Operational.Higher

import Data.Typeable (Typeable)
import Data.Ix       (Ix)
import Data.IORef    (IORef)
import Data.Array.IO (IOArray)
import qualified Data.Array as Arr

import qualified Language.VHDL as V (Expression, Name, Identifier)

import GHC.TypeLits (KnownNat)
import qualified GHC.Exts as GHC (Constraint)

--------------------------------------------------------------------------------
-- * Hardware commands.
--------------------------------------------------------------------------------

data Name = None | Base VarId | Exact VarId 

-- | ...
swapM :: Monad m => Maybe (m a) -> m (Maybe a)
swapM = maybe (return Nothing) (>>= return . Just)

--------------------------------------------------------------------------------
-- ** Values.

-- | Value representation.
data Val a = ValC String | ValE a

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
    NewSignal :: pred a => Name -> Maybe (exp a) -> SignalCMD (Param3 prog exp pred) (Signal a)
    -- ^ Fetch the contents of a signal.
    GetSignal :: pred a => Signal a -> SignalCMD (Param3 prog exp pred) (Val a)
    -- ^ Write the value to a signal.
    SetSignal :: pred a => Signal a -> exp a -> SignalCMD (Param3 prog exp pred) ()
    -- ^ Unsafe version of fetching a signal.
    UnsafeFreezeSignal :: pred a => Signal a -> SignalCMD (Param3 prog exp pred) (Val a)
    -- *** todo: maybe this should be part of a set for concurrent instructions?
    ConcurrentSetSignal :: pred a => Signal a -> exp a -> SignalCMD (Param3 prog exp pred) ()

instance HFunctor SignalCMD
  where
    hfmap _ (NewSignal n e) = NewSignal n e
    hfmap _ (GetSignal s) = GetSignal s
    hfmap _ (SetSignal s e) = SetSignal s e
    hfmap _ (UnsafeFreezeSignal s) = UnsafeFreezeSignal s
    -- ...
    hfmap _ (ConcurrentSetSignal s e) = ConcurrentSetSignal s e

instance HBifunctor SignalCMD
  where
    hbimap _ f (NewSignal n e) = NewSignal n (fmap f e)
    hbimap _ _ (GetSignal s) = GetSignal s
    hbimap _ f (SetSignal s e) = SetSignal s (f e)
    hbimap _ _ (UnsafeFreezeSignal s) = UnsafeFreezeSignal s
    -- ...
    hbimap _ f (ConcurrentSetSignal s e) = ConcurrentSetSignal s (f e)

instance (SignalCMD :<: instr) => Reexpressible SignalCMD instr env
  where
    reexpressInstrEnv reexp (NewSignal n e) =
      lift . singleInj . NewSignal n =<< swapM (fmap reexp e)
    reexpressInstrEnv reexp (GetSignal s) =
      lift $ singleInj $ GetSignal s
    reexpressInstrEnv reexp (SetSignal s e) =
      lift . singleInj . SetSignal s =<< reexp e
    reexpressInstrEnv reexp (UnsafeFreezeSignal s) =
      lift $ singleInj $ UnsafeFreezeSignal s
    -- ...
    reexpressInstrEnv reexp (ConcurrentSetSignal s e) =
      lift . singleInj . ConcurrentSetSignal s =<< reexp e

--------------------------------------------------------------------------------
-- ** Variables.

-- | Variable representation.
data Variable a = VariableC VarId | VariableE (IORef a)

-- | Commands for variables.
data VariableCMD fs a
  where
    -- ^ Create a new variable.
    NewVariable :: pred a => Name -> Maybe (exp a) -> VariableCMD (Param3 prog exp pred) (Variable a)
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

instance (VariableCMD :<: instr) => Reexpressible VariableCMD instr env
  where
    reexpressInstrEnv reexp (NewVariable n e) =
      lift . singleInj . NewVariable n =<< swapM (fmap reexp e)
    reexpressInstrEnv reexp (GetVariable v) =
      lift $ singleInj $ GetVariable v
    reexpressInstrEnv reexp (SetVariable v e) =
      lift . singleInj . SetVariable v =<< reexp e
    reexpressInstrEnv reexp (UnsafeFreezeVariable v) =
      lift $ singleInj $ UnsafeFreezeVariable v

--------------------------------------------------------------------------------
-- ** Arrays.

-- | Expression types that support compilation of array indexing
class CompArrayIx exp
  where
    -- | Generate code for an array indexing operation
    compArrayIx :: (PredicateExp exp a, Integral i, Ix i) => exp i -> Array i a -> Maybe (exp a)
    compArrayIx _ _ = Nothing

-- | Array reprensentation.
data Array i a = ArrayC VarId | ArrayE (IOArray i a)

-- | Commands for signal arrays.
data ArrayCMD fs a
  where
    -- ^ Creates an array of given length.
    NewArray :: (pred a, Integral i, Ix i) => Name -> exp i -> ArrayCMD (Param3 prog exp pred) (Array i a)
    -- ^ Creates an array from the given list of elements.
    InitArray :: (pred a, Integral i, Ix i) => Name -> [a] -> ArrayCMD (Param3 prog exp pred) (Array i a)
    -- ^ Fetches the array's value at the specified index.
    GetArray :: (pred a, Integral i, Ix i) => Array i a -> exp i -> ArrayCMD (Param3 prog exp pred) (Val a)
    -- ^ Writes a value to an array at some specified index.
    SetArray :: (pred a, Integral i, Ix i) => Array i a -> exp i -> exp a -> ArrayCMD (Param3 prog exp pred) ()
    -- ^ Copies a slice from the second array into the first.
    CopyArray :: (pred a, Integral i, Ix i) => (Array i a, exp i) -> (Array i a, exp i) -> exp i -> ArrayCMD (Param3 prog exp pred) ()
    -- ^ Writes a value to all indicies of the array.
    ResetArray :: (pred a, Integral i, Ix i) => Array i a -> exp a -> ArrayCMD (Param3 prog exp pred) ()

instance HFunctor ArrayCMD
  where
    hfmap _ (NewArray n i) = NewArray n i
    hfmap _ (InitArray n is) = InitArray n is
    hfmap _ (GetArray a i) = GetArray a i
    hfmap _ (SetArray a i e) = SetArray a i e
    hfmap _ (CopyArray a b l) = CopyArray a b l
    -- ...
    hfmap _ (ResetArray a r) = ResetArray a r

instance HBifunctor ArrayCMD
  where
    hbimap _ f (NewArray n i) = NewArray n (f i)
    hbimap _ _ (InitArray n is) = InitArray n is
    hbimap _ f (GetArray a i) = GetArray a (f i)
    hbimap _ f (SetArray a i e) = SetArray a (f i) (f e)
    hbimap _ f (CopyArray (a, oa) (b, ob) l) = CopyArray (a, f oa) (b, f ob) (f l)
    -- ...
    hbimap _ f (ResetArray a r) = ResetArray a (f r)

instance (ArrayCMD :<: instr) => Reexpressible ArrayCMD instr env
  where
    reexpressInstrEnv reexp (NewArray n i)
      = lift . singleInj . NewArray n =<< reexp i
    reexpressInstrEnv reexp (InitArray n is)
      = lift $ singleInj $ InitArray n is
    reexpressInstrEnv reexp (GetArray a i)
      = do i' <- reexp i; lift $ singleInj $ GetArray a i'
    reexpressInstrEnv reexp (SetArray a i e)
      = do i' <- reexp i; e' <- reexp e; lift $ singleInj $ SetArray a i' e'
    reexpressInstrEnv reexp (CopyArray (a, oa) (b, ob) l)
      = do oa' <- reexp oa; ob' <- reexp ob; l' <- reexp l
           lift $ singleInj $ CopyArray (a, oa') (b, ob') l'
    -- ...
    reexpressInstrEnv reexp (ResetArray a r)
      = do r' <- reexp r; lift $ singleInj $ ResetArray a r'

--------------------------------------------------------------------------------
-- ** Virtual arrays.

-- | Virtual array reprensentation.
data VArray i a = VArrayC VarId | VArrayE (IOArray i a)
  deriving (Eq, Typeable)

-- | Immutable arrays.
data IArray i a = IArrayC VarId | IArrayE (Arr.Array i a)
  deriving (Show, Typeable)

-- | Commands for variable arrays.
data VArrayCMD fs a
  where
    -- ^ Creates an array of given length.
    NewVArray :: (pred a, Integral i, Ix i) => Name -> exp i -> VArrayCMD (Param3 prog exp pred) (VArray i a)
    -- ^ Creates an array from the given list of elements.
    InitVArray :: (pred a, Integral i, Ix i) => Name -> [a] -> VArrayCMD (Param3 prog exp pred) (VArray i a)
    -- ^ Fetches the array's value at a specified index.
    GetVArray :: (pred a, Integral i, Ix i) => VArray i a -> exp i -> VArrayCMD (Param3 prog exp pred) (Val a)
    -- ^ Writes a value to an array at some specified index.
    SetVArray :: (pred a, Integral i, Ix i) => VArray i a -> exp i -> exp a -> VArrayCMD (Param3 prog exp pred) ()
    -- ^ ...
    CopyVArray:: (pred a, Integral i, Ix i) => (VArray i a, exp i) -> (VArray i a, exp i) -> exp i -> VArrayCMD (Param3 prog exp pred) ()
    -- ^ ...
    UnsafeFreezeVArray :: (pred a, Integral i, Ix i) => VArray i a -> VArrayCMD (Param3 prog exp pred) (IArray i a)
    -- ^ ...
    UnsafeThawVArray :: (pred a, Integral i, Ix i) => IArray i a -> VArrayCMD (Param3 prog exp pred) (VArray i a)

instance HFunctor VArrayCMD
  where
    hfmap _ (NewVArray n i) = NewVArray n i
    hfmap _ (InitVArray n is) = InitVArray n is
    hfmap _ (GetVArray a i) = GetVArray a i
    hfmap _ (SetVArray a i e) = SetVArray a i e
    hfmap _ (CopyVArray a b l) = CopyVArray a b l
    hfmap _ (UnsafeFreezeVArray a) = UnsafeFreezeVArray a
    hfmap _ (UnsafeThawVArray a) = UnsafeThawVArray a

instance HBifunctor VArrayCMD
  where
    hbimap _ f (NewVArray n i) = NewVArray n (f i)
    hbimap _ _ (InitVArray n is) = InitVArray n is
    hbimap _ f (GetVArray a i) = GetVArray a (f i)
    hbimap _ f (SetVArray a i e) = SetVArray a (f i) (f e)
    hbimap _ f (CopyVArray (a, oa) (b, ob) l) = CopyVArray (a, f oa) (b, f ob) (f l)
    hbimap _ _ (UnsafeFreezeVArray a) = UnsafeFreezeVArray a
    hbimap _ _ (UnsafeThawVArray a) = UnsafeThawVArray a

instance (VArrayCMD :<: instr) => Reexpressible VArrayCMD instr env
  where
    reexpressInstrEnv reexp (NewVArray n i)
      = lift . singleInj . NewVArray n =<< reexp i
    reexpressInstrEnv reexp (InitVArray n is)
      = lift $ singleInj $ InitVArray n is
    reexpressInstrEnv reexp (GetVArray a i)
      = do i' <- reexp i; lift $ singleInj $ GetVArray a i'
    reexpressInstrEnv reexp (SetVArray a i e)
      = do i' <- reexp i; e' <- reexp e; lift $ singleInj $ SetVArray a i' e'
    reexpressInstrEnv reexp (CopyVArray (a, oa) (b, ob) l)
      = do oa' <- reexp oa; ob' <- reexp ob; l' <- reexp l
           lift $ singleInj $ CopyVArray (a, oa') (b, ob') l'
    reexpressInstrEnv reexp (UnsafeFreezeVArray a)
      = lift $ singleInj $ UnsafeFreezeVArray a
    reexpressInstrEnv reexp (UnsafeThawVArray a)
      = lift $ singleInj $ UnsafeThawVArray a

--------------------------------------------------------------------------------
-- ** Looping.

-- | Commands for looping constructs.
data LoopCMD fs a
  where
    -- ^ Creates a new for loop.
    For   :: (pred i, Integral i) => exp i -> exp i -> (Val i -> prog ()) -> LoopCMD (Param3 prog exp pred) ()
    -- ^ Creates a new while loop.
    While :: prog (exp Bool) -> prog () -> LoopCMD (Param3 prog exp pred) ()

instance HFunctor LoopCMD
  where
    hfmap f (For l u step)    = For l u (f . step)
    hfmap f (While cont step) = While (f cont) (f step)

instance HBifunctor LoopCMD
  where
    hbimap g f (For l u step)    = For (f l) (f u) (g . step)
    hbimap g f (While cont step) = While (g $ fmap f cont) (g step)

instance (LoopCMD :<: instr) => Reexpressible LoopCMD instr env
  where
    reexpressInstrEnv reexp (For l u step) = do
      l' <- reexp l
      u' <- reexp u
      ReaderT $ \env -> singleInj $
        For l' u' (flip runReaderT env . step)
    reexpressInstrEnv reexp (While cont step) = do
      ReaderT $ \env -> singleInj $
        While (runReaderT (cont >>= reexp) env)
              (runReaderT step env)

--------------------------------------------------------------------------------
-- ** Conditional statements.

-- | ...
data When a prog = When (Constraint a) (prog ())

-- | ...
data Constraint a where
  Is :: Eq a  => a      -> Constraint a
  To :: Ord a => a -> a -> Constraint a

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

instance (ConditionalCMD :<: instr) => Reexpressible ConditionalCMD instr env
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

-- | Signature description.
data Signature fs a
  where
    Ret  :: prog () -> Signature (Param3 prog exp pred) ()
    SSig :: (pred a, Integral a, PrimType a)
      => Name -> Mode
      -> (Signal a -> Signature (Param3 prog exp pred) b)
      -> Signature (Param3 prog exp pred) (Signal a -> b)
    SArr :: (pred a, Integral a, PrimType a, pred i, Integral i, Ix i, PrimType i)
      => Name -> Mode -> i
      -> (Array i a -> Signature (Param3 prog exp pred) b)
      -> Signature (Param3 prog exp pred) (Array i a -> b)

instance HFunctor Signature
  where
    hfmap f (Ret m)          = Ret (f m)
    hfmap f (SSig n m sig)   = SSig n m (hfmap f . sig)
    hfmap f (SArr n m l arr) = SArr n m l (hfmap f . arr)

instance HBifunctor Signature
  where
    hbimap g f (Ret m)          = Ret (g m)
    hbimap g f (SSig n m sig)   = SSig n m (hbimap g f . sig)
    hbimap g f (SArr n m l sig) = SArr n m l (hbimap g f . sig)

reexpressSignature :: env
  -> Signature (Param3 (ReaderT env (ProgramT instr (Param2 exp2 pred) m)) exp1 pred) a
  -> Signature (Param3              (ProgramT instr (Param2 exp2 pred) m)  exp2 pred) a
reexpressSignature env (Ret prog)      = Ret (runReaderT prog env)
reexpressSignature env (SSig n m sf)   = SSig n m   (reexpressSignature env . sf)
reexpressSignature env (SArr n m l af) = SArr n m l (reexpressSignature env . af)

-- | Signature arguments.
data Argument pred a
  where
    Nil  :: Argument pred ()
    ASig :: (pred a, Integral a, PrimType a)
      => Signal a
      -> Argument pred b
      -> Argument pred (Signal a -> b)
    AArr :: (pred a, Integral a, PrimType a, pred i, Integral i, Ix i, PrimType i)
      => Array i a
      -> Argument pred b
      -> Argument pred (Array i a -> b)

-- | Named components.
data Component fs a = Component String (Signature fs a)

-- | Commands for generating stand-alone components and calling them.
data ComponentCMD fs a
  where
    -- ^ Wraps the given signature in a named component.
    DeclareComponent
      :: Name
      -> Signature (Param3 prog exp pred) a
      -> ComponentCMD (Param3 prog exp pred) String
    -- ^ Call for interfacing with a component.
    PortMap
      :: Component (Param3 prog exp pred) a
      -> Argument pred a
      -> ComponentCMD (Param3 prog exp pred) ()

instance HFunctor ComponentCMD
  where
    hfmap f (DeclareComponent n sig) = DeclareComponent n (hfmap f sig)
    hfmap f (PortMap (Component m sig) as) = PortMap (Component m (hfmap f sig)) as

instance HBifunctor ComponentCMD
  where
    hbimap g f (DeclareComponent n sig) = DeclareComponent n (hbimap g f sig)
    hbimap g f (PortMap (Component m sig) as) = PortMap (Component m (hbimap g f sig)) as

instance (ComponentCMD :<: instr) => Reexpressible ComponentCMD instr env
  where
    reexpressInstrEnv reexp (DeclareComponent n sig) = ReaderT $ \env ->
      singleInj $ DeclareComponent n (reexpressSignature env sig)
    reexpressInstrEnv reexp (PortMap (Component m sig) as) = ReaderT $ \env ->
      singleInj $ PortMap (Component m (reexpressSignature env sig)) as

--------------------------------------------------------------------------------
-- ** Structural entities.

type Signals = [Ident]

data Ident   = Ident VarId

class    ToIdent a            where toIdent :: a -> Ident
instance ToIdent (Val      a) where toIdent (ValC      i) = Ident i
instance ToIdent (Signal   a) where toIdent (SignalC   i) = Ident i
instance ToIdent (Variable a) where toIdent (VariableC i) = Ident i
instance ToIdent (Array  i a) where toIdent (ArrayC    i) = Ident i
instance ToIdent (VArray i a) where toIdent (VArrayC   i) = Ident i

-- | Commands for structural entities.
data ProcessCMD fs (a :: *)
  where
    -- ^ Wraps the program in a process.
    Process
      :: Signal Bool -- ^ Clock signal.
      -> Maybe (Signal Bool, prog ())
                     -- ^ Reset signal and action.
      -> Signals     -- ^ Inputs.
      -> prog ()     -- ^ Main program.
      -> ProcessCMD (Param3 prog exp pred) ()

instance HFunctor ProcessCMD
  where
    hfmap f (Process clk rst xs prog) =
      Process clk (fmap (fmap f) rst) xs (f prog)

instance HBifunctor ProcessCMD
  where
    hbimap g _ (Process clk rst xs prog) =
      Process clk (fmap (fmap g) rst) xs (g prog)

instance (ProcessCMD :<: instr) => Reexpressible ProcessCMD instr env
  where
    reexpressInstrEnv reexp (Process clk rst is prog) =
      ReaderT $ \env -> singleInj $ Process clk
        (fmap (fmap (flip runReaderT env)) rst) is
        (runReaderT prog env)

--------------------------------------------------------------------------------

data VHDLCMD fs a
  where
    DeclarePort :: pred a
      => Name          -- ^ Port name.
      -> Maybe (exp a) -- ^ Initial value (if any).
      -> Mode          -- ^ Direction.
      -> VHDLCMD (Param3 prog exp pred) (Signal a)
    -- todo: We should allow for base types to be treated as arrays of bits
    --       instead of having to explicitly convert them.
    -- todo: The second argument should be over a variable.
    CopyBits :: (pred a, pred b, Integral i, Ix i)
      => (Signal a, exp i)
      -> (Signal b, exp i)
      -> exp i
      -> VHDLCMD (Param3 prog exp pred) ()
    CopyVBits :: (pred a, pred b, Integral i, Ix i)
      => (Variable a, exp i)
      -> (Signal   b, exp i)
      -> exp i
      -> VHDLCMD (Param3 prog exp pred) ()
    -- todo: These two should be expressions instead.
    GetBit :: (pred a, pred Bit, Integral i, Ix i)
      => Signal a
      -> exp i
      -> VHDLCMD (Param3 prog exp pred) (Val Bit)
    SetBit :: (pred a, pred Bit, Integral i, Ix i)
      => Signal a
      -> exp i
      -> exp Bit
      -> VHDLCMD (Param3 prog exp pred) ()
    -- todo: same as above two?...
    -- todo: result should be i?...
    GetBits :: (pred i, Integral i, Ix i)
      => Signal (Bits n)
      -> exp i
      -> exp i
      -> VHDLCMD (Param3 prog exp pred) (Val i)

instance HFunctor VHDLCMD
  where
    hfmap _ (DeclarePort n e m) = DeclarePort n e m
    hfmap _ (CopyBits a b l)    = CopyBits a b l
    hfmap _ (CopyVBits a b l)   = CopyVBits a b l
    hfmap _ (GetBit s i)        = GetBit s i
    hfmap _ (SetBit s i b)      = SetBit s i b
    hfmap _ (GetBits s l u)     = GetBits s l u

instance HBifunctor VHDLCMD
  where
    hbimap _ f (DeclarePort n e m)           = DeclarePort n (fmap f e) m
    hbimap _ f (CopyBits (a, oa) (b, ob) l)  = CopyBits (a, f oa) (b, f ob) (f l)
    hbimap _ f (CopyVBits (a, oa) (b, ob) l) = CopyVBits (a, f oa) (b, f ob) (f l)
    hbimap _ f (GetBit s i)                  = GetBit s (f i)
    hbimap _ f (SetBit s i b)                = SetBit s (f i) (f b)
    hbimap _ f (GetBits s l u)               = GetBits s (f l) (f u)

instance (VHDLCMD :<: instr) => Reexpressible VHDLCMD instr env
  where
    reexpressInstrEnv reexp (DeclarePort n e m)
      = do e' <- swapM (fmap reexp e)
           lift $ singleInj $ DeclarePort n e' m
    reexpressInstrEnv reexp (CopyBits (a, oa) (b, ob) l)
      = do oa' <- reexp oa; ob' <- reexp ob; l' <- reexp l
           lift $ singleInj $ CopyBits (a, oa') (b, ob') l'
    reexpressInstrEnv reexp (CopyVBits (a, oa) (b, ob) l)
      = do oa' <- reexp oa; ob' <- reexp ob; l' <- reexp l
           lift $ singleInj $ CopyVBits (a, oa') (b, ob') l'
    reexpressInstrEnv reexp (GetBit s i)
      = do i' <- reexp i
           lift $ singleInj $ GetBit s i'
    reexpressInstrEnv reexp (SetBit s i b)
      = do i' <- reexp i; b' <- reexp b
           lift $ singleInj $ SetBit s i' b'
    reexpressInstrEnv reexp (GetBits s l u)
      = do l' <- reexp l; u' <- reexp u
           lift $ singleInj $ GetBits s l' u'

--------------------------------------------------------------------------------
