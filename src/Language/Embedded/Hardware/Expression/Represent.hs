{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Embedded.Hardware.Expression.Represent
  ( HType(..)
  , Inhabited(..)
  , Rep(..)
  
  , declareBoolean
  , declareNumeric
  , declareFloating
    
  , module Data.Int
  , module Data.Word
  ) where

import qualified Language.VHDL as V

import Language.VHDL (Expression)

import Language.Embedded.VHDL (VHDL)
import Language.Embedded.VHDL.Monad (newSym, newLibrary, newImport)
import Language.Embedded.VHDL.Monad.Expression (lit)
import Language.Embedded.VHDL.Monad.Type hiding (literal)
import Language.Embedded.VHDL.Monad.Util (printBits)

import Language.Embedded.Hardware.Expression.Hoist (lift)

import Data.Char (isDigit)
import Data.Int
import Data.Word
import Data.Typeable
import Text.Printf

--------------------------------------------------------------------------------
-- * Representation of types.
--------------------------------------------------------------------------------

-- | Collection of required classes for hardware expressions.
class    (Typeable a, Rep a, Eq a) => HType a
instance (Typeable a, Rep a, Eq a) => HType a

-- | A 'rep'resentable value.
class Rep a
  where
    declare :: proxy a -> VHDL Type
    format  :: a -> String
    bits    :: a -> String

-- | Bit-length of a representable type.
bitSize :: Rep a => a -> Int
bitSize = length . filter isDigit . bits

--------------------------------------------------------------------------------
-- ** Boolean

instance Rep Bool where
  declare _    = declareBoolean >> return std_logic
  format True  = "\'1\'"
  format False = "\'0\'"
  bits         = format

--------------------------------------------------------------------------------
-- ** Signed

instance Rep Int8 where
  declare _ = declareNumeric >> return signed8
  format    = show
  bits      = printBits 8

instance Rep Int16 where
  declare _ = declareNumeric >> return signed16
  format    = show
  bits      = printBits 16

instance Rep Int32 where
  declare _ = declareNumeric >> return signed32
  format    = show
  bits      = printBits 32

instance Rep Int64 where
  declare _ = declareNumeric >> return signed64
  format    = show
  bits      = printBits 64

--------------------------------------------------------------------------------
-- ** Unsigned

instance Rep Word8 where
  declare _ = declareNumeric >> return usigned8
  format    = show
  bits      = printBits 8

instance Rep Word16 where
  declare _ = declareNumeric >> return usigned16
  format    = show
  bits      = printBits 16

instance Rep Word32 where
  declare _ = declareNumeric >> return usigned32
  format    = show
  bits      = printBits 32

instance Rep Word64 where
  declare _ = declareNumeric >> return usigned64
  format    = show
  bits      = printBits 64

--------------------------------------------------------------------------------
-- ** Floating point.

instance Rep Float where
  declare _ = declareFloating >> return float
  format    = show
  bits      = error "hardware-edsl.bits: float."

instance Rep Double where
  declare _ = declareFloating >> return double
  format    = show
  bits      = error "hardware-edsl.bits: double."

--------------------------------------------------------------------------------
-- ** ...

instance Rep Int where
  declare _ = return (integer Nothing)
  format  i = show i
  bits      = error "hardware-edsl.bits: int."

instance Rep Integer where
  declare _ = return (integer Nothing)
  format  i = show i
  bits      = error "hardware-edsl.bits: integer."

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

instance Num Bool where
  (+)    = error "(+) not implemented for Bool"
  (-)    = error "(-) not implemented for Bool"
  (*)    = error "(*) not implemented for Bool"
  abs    = id
  signum = id
  fromInteger 0 = False
  fromInteger 1 = True
  fromInteger _ = error "bool-num: >1"

--------------------------------------------------------------------------------
-- todo : I should move this to its own module.

-- | ...
class HType a => Inhabited a
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
instance Inhabited Integer where reset = 0
instance Inhabited Float   where reset = 0
instance Inhabited Double  where reset = 0

--------------------------------------------------------------------------------
