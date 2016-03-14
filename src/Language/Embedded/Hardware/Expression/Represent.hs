{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Embedded.Hardware.Expression.Represent
  ( Rep (..)
  , Bit, Bit2,  Bit4,  Bit8,  Bit16,  Bit32,  Bit64
  ,      Int2,  Int4,  module Data.Int
  ,      Word2, Word4, module Data.Word
  
  ) where

import qualified Language.VHDL as V

import Language.Embedded.VHDL            (VHDL)
import Language.Embedded.VHDL.Monad      (newSym, newLibrary, newImport, constrainedArray)
import Language.Embedded.VHDL.Monad.Type

import Data.Int
import Data.Word
import Data.Ix
import Data.Char   (intToDigit)
import Data.Bits   (shiftR)
import Text.Printf
import Numeric     (showIntAtBase)

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

--------------------------------------------------------------------------------
-- * Representable types (until I come up with a solution free of VHDL stuff).
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** Std_logic.

type Bit = Bool

newtype Bit2  = B2  { unB2  :: Word2 }
 deriving (Eq, Ord, Enum, Num, Real, Integral, Ix, Show, Read)

newtype Bit4  = B4  { unB4  :: Word4 }
  deriving (Eq, Ord, Enum, Num, Real, Integral, Ix, Show, Read)
           
newtype Bit8  = B8  { unB8  :: Word8 }
  deriving (Eq, Ord, Enum, Num, Real, Integral, Ix, Show, Read)

newtype Bit16 = B16 { unB16 :: Word16 }
  deriving (Eq, Ord, Enum, Num, Real, Integral, Ix, Show, Read)

newtype Bit32 = B32 { unB32 :: Word32 }
  deriving (Eq, Ord, Enum, Num, Real, Integral, Ix, Show, Read)

newtype Bit64 = B64 { unB64 :: Word64 }
  deriving (Eq, Ord, Enum, Num, Real, Integral, Ix, Show, Read)
  
--------------------------------------------------------------------------------
-- ** Signed.

data Int2 = I2 Int
  deriving (Eq, Ord, Show, Read)

data Int4 = I4 Int
  deriving (Eq, Ord, Show, Read)
  
--------------------------------------------------------------------------------
-- ** Unsigned.

data Word2 = W2 Word
  deriving (Eq, Ord, Show, Read)

data Word4 = W4 Word
  deriving (Eq, Ord, Show, Read)

--------------------------------------------------------------------------------
-- * Representation of types.
--------------------------------------------------------------------------------

-- | A 'rep'resentable value.
class Rep a
  where
    declare :: proxy a -> VHDL Type
    format  :: a       -> String
  
--------------------------------------------------------------------------------
-- ** Boolean

printBits :: (PrintfArg a, PrintfType b) => Int -> a -> b
printBits zeroes = printf ("\"%0" ++ show zeroes ++ "b\"")

instance Rep Bit where
  declare _    = declareBoolean >> return std_logic
  format True  = "\'1\'"
  format False = "\'0\'"

instance Rep Bit2 where
  declare _ = declareBoolean >> return (std_logic_vector 2)
  format (B2 (W2 w)) = printBits 2 w

instance Rep Bit4 where
  declare _ = declareBoolean >> return (std_logic_vector 4)
  format (B4 (W4 w)) = printBits 4 w

instance Rep Bit8 where
  declare _ = declareBoolean >> return (std_logic_vector 8)
  format    = printBits 8 . unB8

instance Rep Bit16 where
  declare _ = declareBoolean >> return (std_logic_vector 16)
  format    = printBits 16 . unB16

instance Rep Bit32 where
  declare _ = declareBoolean >> return (std_logic_vector 32)
  format    = printBits 32 . unB32

instance Rep Bit64 where
  declare _ = declareBoolean >> return (std_logic_vector 64)
  format    = printBits 64 . unB64

--------------------------------------------------------------------------------
-- ** Signed

instance Rep Int2 where
  declare _ = declareNumeric >> return signed2
  format (I2 i) = printBits 2 i

instance Rep Int4 where
  declare _ = declareNumeric >> return signed4
  format (I4 i) = printBits 4 i

instance Rep Int8 where
  declare _ = declareNumeric >> return signed8
  format    = printBits 8

instance Rep Int16 where
  declare _ = declareNumeric >> return signed16
  format    = printBits 16

instance Rep Int32 where
  declare _ = declareNumeric >> return signed32
  format    = printBits 32

instance Rep Int64 where
  declare _ = declareNumeric >> return signed64
  format    = printBits 64

--------------------------------------------------------------------------------
-- ** Unsigned

instance Rep Word2 where
  declare _ = declareNumeric >> return usigned2
  format (W2 w) = printBits 2 w

instance Rep Word4 where
  declare _     = declareNumeric >> return usigned4
  format (W4 w) = printBits 4 w

instance Rep Word8 where
  declare _ = declareNumeric >> return usigned8
  format    = printBits 8

instance Rep Word16 where
  declare _ = declareNumeric >> return usigned16
  format    = printBits 16

instance Rep Word32 where
  declare _ = declareNumeric >> return usigned32
  format    = printBits 32

instance Rep Word64 where
  declare _ = declareNumeric >> return usigned64
  format    = printBits 64

--------------------------------------------------------------------------------
-- ** Floating point.

instance Rep Float where
  declare _ = declareFloating >> return float
  format    = error "todo: format float."

instance Rep Double where
  declare _ = declareFloating >> return double
  format    = error "todo: format double."

--------------------------------------------------------------------------------
-- ** ...

instance Rep Integer where
  declare _ = return (integer Nothing)
  format    = show

--------------------------------------------------------------------------------
  
declareBoolean :: VHDL ()
declareBoolean =
  do newLibrary "IEEE"
     newImport  "IEEE.std_logic_1164"

declareNumeric :: VHDL ()
declareNumeric =
  do newLibrary "IEEE"
     newImport  "IEEE.std_logic_1164"
     newImport  "IEEE.numeric_std"

declareFloating :: VHDL ()
declareFloating =
  do newLibrary "IEEE"
     newImport  "IEEE.float_pkg"

--------------------------------------------------------------------------------
-- ...
--------------------------------------------------------------------------------

instance Num Bit where
  True  + True  = False
  False + False = False
  _     + _     = True
  (-)    = error "todo: bit (-)"
  (*)    = error "todo: bit (*)"
  abs    = error "todo: bit abs"
  signum = error "todo: bit signum"
  fromInteger 1 = True
  fromInteger 0 = False
  fromInteger x = error $ show x

--------------------------------------------------------------------------------
-- ** ...

instance Enum Int2 where
  toEnum   i = error "todo"
  fromEnum i = error "todo"

instance Num Int2 where
  (I2 w) + (I2 v) = I2 (w + v `mod` 4)
  (I2 w) - (I2 v) = I2 (w - v `mod` 4)
  (I2 w) * (I2 v) = I2 (w * v `mod` 4)
  abs      (I2 w) = I2 (abs w)
  signum   (I2 w) = I2 (signum w)
  fromInteger i   = I2 (fromInteger i)

instance Real Int2 where
  toRational (I2 w) = toRational w

instance Integral Int2 where
  quotRem   (I2 w) (I2 v) = error "todo"
  toInteger (I2 w)        = toInteger w

instance Ix Int2 where
  range   = error "todo"
  index   = error "todo"
  inRange = error "todo"

--------------------------------------------------------------------------------
-- ** ...

instance Enum Int4 where
  toEnum   i = error "todo"
  fromEnum i = error "todo"

instance Num Int4 where
  (I4 w) + (I4 v) = I4 (w + v `mod` 16)
  (I4 w) - (I4 v) = I4 (w - v `mod` 16)
  (I4 w) * (I4 v) = I4 (w * v `mod` 16)
  abs      (I4 w) = I4 (abs w)
  signum   (I4 w) = I4 (signum w)
  fromInteger i   = I4 (fromInteger i)

instance Real Int4 where
  toRational (I4 w) = toRational w

instance Integral Int4 where
  quotRem   (I4 w) (I4 v) = error "todo"
  toInteger (I4 w)        = toInteger w

instance Ix Int4 where
  range   = error "todo"
  index   = error "todo"
  inRange = error "todo"

--------------------------------------------------------------------------------
-- ** ...

instance Enum Word2 where
  toEnum   i = error "todo"
  fromEnum i = error "todo"

instance Num Word2 where
  (W2 w) + (W2 v) = W2 (w + v `mod` 4)
  (W2 w) - (W2 v) = W2 (w - v `mod` 4)
  (W2 w) * (W2 v) = W2 (w * v `mod` 4)
  abs      (W2 w) = W2 (abs w)
  signum   (W2 w) = W2 (signum w)
  fromInteger i   = W2 (fromInteger i)

instance Real Word2 where
  toRational (W2 w) = toRational w

instance Integral Word2 where
  quotRem   (W2 w) (W2 v) = error "todo"
  toInteger (W2 w)        = toInteger w

instance Ix Word2 where
  range   = error "todo"
  index   = error "todo"
  inRange = error "todo"

--------------------------------------------------------------------------------
-- ** ...

instance Enum Word4 where
  toEnum   i = error "todo"
  fromEnum i = error "todo"

instance Num Word4 where
  (W4 w) + (W4 v) = W4 (w + v `mod` 16)
  (W4 w) - (W4 v) = W4 (w - v `mod` 16)
  (W4 w) * (W4 v) = W4 (w * v `mod` 16)
  abs      (W4 w) = W4 (abs w)
  signum   (W4 w) = W4 (signum w)
  fromInteger i   = W4 (fromInteger i)

instance Real Word4 where
  toRational (W4 w) = toRational w

instance Integral Word4 where
  quotRem   (W4 w) (W4 v) = error "todo"
  toInteger (W4 w)        = toInteger w

instance Ix Word4 where
  range   = error "todo"
  index   = error "todo"
  inRange = error "todo"

--------------------------------------------------------------------------------
