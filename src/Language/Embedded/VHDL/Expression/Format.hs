{-# LANGUAGE ScopedTypeVariables #-}

module Language.Embedded.VHDL.Expression.Format
  ( Tagged(..)
  , Rep(..)
  ) where

import Language.Embedded.VHDL.Expression.Type

import Data.Bits
import Data.Char   (intToDigit)
import Data.Word
import Data.Int
import Text.Printf (printf)
import Numeric     (showIntAtBase)
import qualified Data.ByteString as B

--------------------------------------------------------------------------------
-- * Kludge - until I come up with a better solution
--------------------------------------------------------------------------------

-- | Tag a value with some (possibly) interesting information
newtype Tagged s b = Tag { unTag :: b }

-- | A 'rep'resentable value.
class Rep a
  where
    width  :: Tagged a Int
    typed  :: Tagged a Type
    format :: a -> String

--------------------------------------------------------------------------------
-- ** Boolean

instance Rep Bool where
  width        = Tag 1
  typed        = Tag std_logic
  format True  = "1"
  format False = "0"

--------------------------------------------------------------------------------
-- ** Signed

instance Rep Int8 where
  width  = Tag 8
  typed  = Tag signed8
  format = convert

instance Rep Int16 where
  width  = Tag 16
  typed  = Tag signed16
  format = convert

instance Rep Int32 where
  width  = Tag 32
  typed  = Tag signed32
  format = convert

instance Rep Int64 where
  width  = Tag 64
  typed  = Tag signed64
  format = convert

--------------------------------------------------------------------------------
-- ** Unsigned

instance Rep Word8 where
  width  = Tag 8
  typed  = Tag usigned8
  format = convert

instance Rep Word16 where
  width  = Tag 16
  typed  = Tag usigned16
  format = convert

instance Rep Word32 where
  width  = Tag 32
  typed  = Tag usigned32
  format = convert

instance Rep Word64 where
  width  = Tag 64
  typed  = Tag usigned64
  format = convert

--------------------------------------------------------------------------------
-- ** Records

instance (Rep a, Rep b) => Rep (a, b) where
  width  = undefined
  typed  = undefined
  format = undefined

-- ...

--------------------------------------------------------------------------------
-- * Converting Integers to their Binrary representation
--------------------------------------------------------------------------------

-- | Convert an Integral to its binary representation
convert :: Integral a => a -> String
convert = foldr1 (++) . fmap w2s . B.unpack . i2bs . toInteger

-- | Go over an Integer and convert it into a bytestring containing its
--   binary representation
i2bs :: Integer -> B.ByteString
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
