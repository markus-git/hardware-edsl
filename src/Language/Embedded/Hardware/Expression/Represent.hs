{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Language.Embedded.Hardware.Expression.Represent
  ( Rep(..)
  , Inhabited(..)
  , Sized(..)
  , HType(..)
  
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
import Language.Embedded.VHDL.Monad.Type
import qualified Language.Embedded.VHDL.Monad.Util as Util (printBits)

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

--------------------------------------------------------------------------------
-- ** Representable types.

-- | 'Rep'resentable types.
class Rep a
  where
    declare   :: proxy a -> VHDL Type
    printVal  :: a -> String
    printBits :: a -> String

instance Rep Bool where
  declare  _     = declareBoolean >> return std_logic
  printVal True  = "\'1\'"
  printVal False = "\'0\'"
  printBits      = printVal

instance Rep Int8 where
  declare _ = declareNumeric >> return signed8
  printVal  = show
  printBits = Util.printBits 8

instance Rep Int16 where
  declare _ = declareNumeric >> return signed16
  printVal  = show
  printBits = Util.printBits 16

instance Rep Int32 where
  declare _ = declareNumeric >> return signed32
  printVal  = show
  printBits = Util.printBits 32

instance Rep Int64 where
  declare _ = declareNumeric >> return signed64
  printVal  = show
  printBits = Util.printBits 64

instance Rep Word8 where
  declare _ = declareNumeric >> return usigned8
  printVal  = show
  printBits = Util.printBits 8

instance Rep Word16 where
  declare _ = declareNumeric >> return usigned16
  printVal  = show
  printBits = Util.printBits 16

instance Rep Word32 where
  declare _ = declareNumeric >> return usigned32
  printVal  = show
  printBits = Util.printBits 32

instance Rep Word64 where
  declare _ = declareNumeric >> return usigned64
  printVal  = show
  printBits = Util.printBits 64

instance Rep Int where
  declare _ = return (integer Nothing)
  printVal  = show
  printBits = error "hardware-edsl.printBits: int."

instance Rep Integer where
  declare _ = return (integer Nothing)
  printVal  = show
  printBits = error "hardware-edsl.printBits: integer."

instance Rep Float where
  declare _ = declareFloating >> return float
  printVal  = show
  printBits = error "hardware-edsl.printBits: float."

instance Rep Double where
  declare _ = declareFloating >> return double
  printVal  = show
  printBits = error "hardware-edsl.printBits: double."

-- | Declare the necessary libraries to support boolean operations.
declareBoolean :: VHDL ()
declareBoolean =
  do newLibrary "IEEE"
     newImport  "IEEE.std_logic_1164"

-- | Declare the necessary libraries to support numerical operations.
declareNumeric :: VHDL ()
declareNumeric =
  do newLibrary "IEEE"
     newImport  "IEEE.std_logic_1164"
     newImport  "IEEE.numeric_std"

-- | Declare the necessary libraries to support floating point operations.
declareFloating :: VHDL ()
declareFloating =
  do newLibrary "IEEE"
     newImport  "IEEE.float_pkg"

--------------------------------------------------------------------------------
-- ** Inhabited types.

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

--------------------------------------------------------------------------------
-- ** Sized types.

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

--------------------------------------------------------------------------------
-- ** Hmm...

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
