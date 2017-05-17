{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Embedded.Hardware.Expression.Represent
  ( HType(..)
  , Rep(..)
  , compT
  , literal
  
  , declareBoolean
  , declareNumeric
  , declareFloating
    
  , module Data.Int
  , module Data.Word
  ) where

import qualified Language.VHDL as V

import Language.VHDL (Expression)

import Language.Embedded.VHDL (VHDL)
import Language.Embedded.VHDL.Monad (newSym, newLibrary, newImport, constrainedArray)
import Language.Embedded.VHDL.Monad.Expression (lit)
import Language.Embedded.VHDL.Monad.Type hiding (literal)

import Language.Embedded.Hardware.Expression.Hoist (lift)

import Data.Int
import Data.Word
import Data.Ix
import Data.Char   (intToDigit)
import Data.Bits   (shiftR)
import Data.Typeable
import Text.Printf
import Numeric     (showIntAtBase)

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

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
    format  :: a       -> String

-- | ...
compT :: HType a => proxy a -> VHDL Type
compT = declare

-- | ...
literal :: HType a => a -> VHDL Expression
literal = return . lift . lit . format

--------------------------------------------------------------------------------
-- ** Boolean

instance Rep Bool where
  declare _    = declareBoolean >> return std_logic
  format True  = "\'1\'"
  format False = "\'0\'"

--------------------------------------------------------------------------------
-- ** Signed

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

instance Rep Int where
  declare _ = return (integer Nothing)
  format  i = show i --"10#" ++ show i ++ "#"

instance Rep Integer where
  declare _ = return (integer Nothing)
  format  i = show i --"10#" ++ show i ++ "#"

--------------------------------------------------------------------------------

printBits :: (PrintfArg a, PrintfType b) => Int -> a -> b
printBits zeroes = printf ("\"%0" ++ show zeroes ++ "b\"")

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
