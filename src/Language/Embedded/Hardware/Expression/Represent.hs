{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}

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

-- | A 'rep'resentable value.
class Rep a
  where
    declare   :: proxy a -> VHDL Type
    bits      :: proxy a -> Integer
    printVal  :: a -> String
    printBits :: a -> String
    
--------------------------------------------------------------------------------
-- ** Boolean

instance Rep Bool where
  declare  _     = declareBoolean >> return std_logic
  bits     _     = 1
  printVal True  = "\'1\'"
  printVal False = "\'0\'"
  printBits      = printVal

--------------------------------------------------------------------------------
-- ** Signed

instance Rep Int8 where
  declare _ = declareNumeric >> return signed8
  bits    _ = 8
  printVal  = show
  printBits = Util.printBits 8

instance Rep Int16 where
  declare _ = declareNumeric >> return signed16
  bits    _ = 16
  printVal  = show
  printBits = Util.printBits 16

instance Rep Int32 where
  declare _ = declareNumeric >> return signed32
  bits    _ = 32
  printVal  = show
  printBits = Util.printBits 32

instance Rep Int64 where
  declare _ = declareNumeric >> return signed64
  bits    _ = 64
  printVal  = show
  printBits = Util.printBits 64

--------------------------------------------------------------------------------
-- ** Unsigned

instance Rep Word8 where
  declare _ = declareNumeric >> return usigned8
  bits    _ = 8
  printVal  = show
  printBits = Util.printBits 8

instance Rep Word16 where
  declare _ = declareNumeric >> return usigned16
  bits    _ = 16
  printVal  = show
  printBits = Util.printBits 16

instance Rep Word32 where
  declare _ = declareNumeric >> return usigned32
  bits    _ = 32
  printVal  = show
  printBits = Util.printBits 32

instance Rep Word64 where
  declare _ = declareNumeric >> return usigned64
  bits    _ = 64
  printVal  = show
  printBits = Util.printBits 64

--------------------------------------------------------------------------------
-- ** Floating point.

instance Rep Float where
  declare _ = declareFloating >> return float
  bits      = error "hardware-edsl.bits: float."
  printVal  = show
  printBits = error "hardware-edsl.printBits: float."

instance Rep Double where
  declare _ = declareFloating >> return double
  bits    _ = error "hardware-edsl.bits: double."
  printVal  = show
  printBits = error "hardware-edsl.printBits: double."

--------------------------------------------------------------------------------
-- ** ...

instance Rep Int where
  declare _ = return (integer Nothing)
  bits    _ = error "hardware-edsl.bits: int."
  printVal  = show
  printBits = error "hardware-edsl.printBits: int."

instance Rep Integer where
  declare _ = return (integer Nothing)
  bits    _ = error "hardware-edsl.bits: integer."
  printVal  = show
  printBits = error "hardware-edsl.printBits: integer."

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
