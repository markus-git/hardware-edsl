module Language.Embedded.Hardware.Expression.Represent
  ( Tagged (..)
  , Rep    (..)
  ) where

import qualified Language.VHDL as V

import Language.Embedded.VHDL            (VHDL)
import Language.Embedded.VHDL.Monad      (newSym, newLibrary, newImport, constrainedArray)
import Language.Embedded.VHDL.Monad.Type

import Data.Int
import Data.Word

import Data.Char   (intToDigit)
import Data.Bits   (shiftR)
import Text.Printf (printf)
import Numeric     (showIntAtBase)

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

--------------------------------------------------------------------------------
-- * Representable types (until I come up with a solution free of VHDL stuff).
--------------------------------------------------------------------------------

-- | Tag a value with some (possibly) interesting information
newtype Tagged s b = Tag { unTag :: b }

-- | A 'rep'resentable value.
class Rep a
  where
    declare :: proxy a -> VHDL Type
    format  :: a       -> String

--------------------------------------------------------------------------------
-- ** Boolean

instance Rep Bool where
  declare _    = declareBoolean >> return std_logic
  format True  = "1"
  format False = "0"

--------------------------------------------------------------------------------
-- ** Signed

instance Rep Int8 where
  declare _ = declareNumeric >> return signed8
  format    = convert

instance Rep Int16 where
  declare _ = declareNumeric >> return signed16
  format    = convert

instance Rep Int32 where
  declare _ = declareNumeric >> return signed32
  format    = convert

instance Rep Int64 where
  declare _ = declareNumeric >> return signed64
  format    = convert

--------------------------------------------------------------------------------
-- ** Unsigned

instance Rep Word8 where
  declare _ = declareNumeric >> return usigned8
  format    = convert

instance Rep Word16 where
  declare _ = declareNumeric >> return usigned16
  format    = convert

instance Rep Word32 where
  declare _ = declareNumeric >> return usigned32
  format    = convert

instance Rep Word64 where
  declare _ = declareNumeric >> return usigned64
  format    = convert

--------------------------------------------------------------------------------
-- ** Floating point.

instance Rep Float where
  declare _ = declareFloating >> return float
  format    = error "todo: format float."

instance Rep Double where
  declare _ = declareFloating >> return double
  format    = error "todo: format double."

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
-- * Converting Integers to their Binrary representation
--------------------------------------------------------------------------------

-- | Convert an Integral to its binary representation
convert :: Integral a => a -> String
convert = foldr1 (++) . fmap w2s . B.unpack . i2bs . toInteger

-- | Go over an Integer and convert it into a bytestring containing its
--   binary representation
i2bs :: Integer -> ByteString
i2bs x = B.reverse . B.unfoldr (fmap chunk) . Just $ sign x
  where
    sign :: (Num a, Ord a) => a -> a
    sign | x < 0     = subtract 1 . negate
         | otherwise = id

    chunk :: Integer -> (Word8, Maybe Integer)
    chunk x = (b, i)
      where
        b = sign (fromInteger x)
        i | x >= 128  = Just (x `shiftR` 8)
          | otherwise = Nothing

-- | Shows a word with zero padding
--
-- I assum the negative numbers to already be padded with ones
w2s :: Word8 -> String
w2s w = printf "%08s" $ showIntAtBase 2 intToDigit w ""

--------------------------------------------------------------------------------
