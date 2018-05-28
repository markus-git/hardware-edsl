{-# LANGUAGE GADTs                #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE FlexibleContexts     #-}

module Language.Embedded.Hardware.Expression.Represent where

import Language.Embedded.Hardware.Expression.Represent.Bit

import Language.Embedded.VHDL (VHDL, MonadV)
import Language.Embedded.VHDL.Monad (newSym, addLibrary, addImport)
import Language.Embedded.VHDL.Monad.Type
import Language.Embedded.VHDL.Monad.Util (printBits)

import Data.Char (isDigit)
import Data.Int
import Data.Word
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable)

import GHC.TypeLits

--------------------------------------------------------------------------------
-- * Inhabited types.
--------------------------------------------------------------------------------

-- | Inhabited types, that is, types with a base element.
class Inhabited a
  where
    -- | Ground value.
    reset :: a

instance Inhabited Bool    where reset = False
instance Inhabited Int8    where reset = 0
instance Inhabited Int16   where reset = 0
instance Inhabited Int32   where reset = 0
instance Inhabited Int64   where reset = 0
instance Inhabited Word8   where reset = 0
instance Inhabited Word16  where reset = 0
instance Inhabited Word32  where reset = 0
instance Inhabited Word64  where reset = 0
instance Inhabited Int     where reset = 0
instance Inhabited Integer where reset = 0
instance Inhabited Float   where reset = 0
instance Inhabited Double  where reset = 0

instance forall n . KnownNat n => Inhabited (Bits n)
  where
    reset = bitFromInteger 0

--------------------------------------------------------------------------------
-- * Sized types.
--------------------------------------------------------------------------------

-- | Types with a known size.
class Sized a
  where
    -- | Bits required to represent values of type 'a'.
    bits :: proxy a -> Integer

instance Sized Bool    where bits _ = 1
instance Sized Int8    where bits _ = 8
instance Sized Int16   where bits _ = 16
instance Sized Int32   where bits _ = 32
instance Sized Int64   where bits _ = 64
instance Sized Word8   where bits _ = 8
instance Sized Word16  where bits _ = 16
instance Sized Word32  where bits _ = 32
instance Sized Word64  where bits _ = 64
instance Sized Int     where bits _ = 32
instance Sized Integer where bits _ = 64
instance Sized Float   where bits _ = 32
instance Sized Double  where bits _ = 64

instance forall n . KnownNat n => Sized (Bits n)
  where
    bits _ = ni (Proxy :: Proxy n)

--------------------------------------------------------------------------------
-- * Representable types.
--------------------------------------------------------------------------------

-- | Representation of primitive hardware types.
data TypeRep a
  where
    -- booleans.
    BoolT    :: TypeRep Bool
    -- signed numbers.
    Int8T    :: TypeRep Int8
    Int16T   :: TypeRep Int16
    Int32T   :: TypeRep Int32
    Int64T   :: TypeRep Int64
    -- unsigned numbers.
    Word8T   :: TypeRep Word8
    Word16T  :: TypeRep Word16
    Word32T  :: TypeRep Word32
    Word64T  :: TypeRep Word64
    -- integers.
    IntT     :: TypeRep Int
    IntegerT :: TypeRep Integer
    -- floating point numbers.
    FloatT   :: TypeRep Float
    DoubleT  :: TypeRep Double
    -- variable sized bit values (todo).
    BitsT    :: KnownNat n => TypeRep (Bits n)

deriving instance Eq       (TypeRep a)
deriving instance Show     (TypeRep a)
deriving instance Typeable (TypeRep a)

--------------------------------------------------------------------------------

-- | Primitive hardware types.
class (Eq a, Show a, Typeable a, Inhabited a, Sized a) => PrimType a
  where
    typeRep :: TypeRep a

instance PrimType Bool    where typeRep = BoolT
instance PrimType Int8    where typeRep = Int8T
instance PrimType Int16   where typeRep = Int16T
instance PrimType Int32   where typeRep = Int32T
instance PrimType Int64   where typeRep = Int64T
instance PrimType Word8   where typeRep = Word8T
instance PrimType Word16  where typeRep = Word16T
instance PrimType Word32  where typeRep = Word32T
instance PrimType Word64  where typeRep = Word64T
instance PrimType Int     where typeRep = IntT
instance PrimType Integer where typeRep = IntegerT
instance PrimType Float   where typeRep = FloatT
instance PrimType Double  where typeRep = DoubleT

instance forall n . KnownNat n => PrimType (Bits n)
  where
    typeRep = BitsT

--------------------------------------------------------------------------------

-- | Print a value.
primTypeVal :: forall a . PrimType a => a -> String
primTypeVal a = case typeRep :: TypeRep a of
  BoolT    -> if a then "\'1\'" else "\'0\'"
  t        -> show a

-- | Print a value as its bit representation.
primTypeBits :: forall a . PrimType a => a -> String
primTypeBits a = case typeRep :: TypeRep a of
  BoolT    -> primTypeVal a
  Int8T    -> printBits 8  a
  Int16T   -> printBits 16 a
  Int32T   -> printBits 32 a
  Int64T   -> printBits 64 a
  Word8T   -> printBits 8  a
  Word16T  -> printBits 16 a
  Word32T  -> printBits 32 a
  Word64T  -> printBits 64 a
  IntT     -> error "todo: print ints as bits."
  IntegerT -> error "todo: print integers as bits."
  FloatT   -> error "todo: print floats as bits."
  DoubleT  -> error "todo: print doubles as bits."

-- | Hardware type representation of a primitive type.
primTypeRep :: forall a . PrimType a => Proxy a -> Type
primTypeRep _ = case typeRep :: TypeRep a of
  BoolT    -> std_logic
  Int8T    -> signed8
  Int16T   -> signed16
  Int32T   -> signed32
  Int64T   -> signed64
  Word8T   -> usigned8
  Word16T  -> usigned16
  Word32T  -> usigned32
  Word64T  -> usigned64
  IntT     -> integer Nothing -- todo: might be wrong.
  IntegerT -> integer Nothing -- todo: migth be wrong.
  FloatT   -> float
  DoubleT  -> double
  BitsT    -> primTypeRepBits (Proxy :: Proxy a)

primTypeRepBits :: forall n. KnownNat n => Proxy (Bits n) -> Type
primTypeRepBits _ = std_logic_vector size
  where size = fromInteger (ni (undefined :: Bits n))

-- | Declare the necessary imports/packages to support a primitive type.
primTypeDeclare :: forall m a. (MonadV m, PrimType a) => Proxy a -> m ()
primTypeDeclare p = case typeRep :: TypeRep a of
  BoolT    -> declareBoolean
  Int8T    -> declareNumeric
  Int16T   -> declareNumeric
  Int32T   -> declareNumeric
  Int64T   -> declareNumeric
  Word8T   -> declareNumeric
  Word16T  -> declareNumeric
  Word32T  -> declareNumeric
  Word64T  -> declareNumeric
  IntT     -> declareNumeric
  IntegerT -> declareNumeric
  FloatT   -> declareFloating
  DoubleT  -> declareFloating
  BitsT    -> declareBoolean

-- | Declare a primitive hardware type and get back its representation.
declareType :: (MonadV m, PrimType a) => Proxy a -> m Type
declareType proxy = primTypeDeclare proxy >> return (primTypeRep proxy)

-- | Declare the necessary libraries to support boolean operations.
declareBoolean :: MonadV m => m ()
declareBoolean =
  do addLibrary "IEEE"
     addImport  "IEEE.std_logic_1164"

-- | Declare the necessary libraries to support numerical operations.
declareNumeric :: MonadV m => m ()
declareNumeric =
  do addLibrary "IEEE"
     addImport  "IEEE.std_logic_1164"
     addImport  "IEEE.numeric_std"

-- | Declare the necessary libraries to support floating point operations.
declareFloating :: MonadV m => m ()
declareFloating =
  do addLibrary "IEEE"
     addImport  "IEEE.float_pkg"

--------------------------------------------------------------------------------
