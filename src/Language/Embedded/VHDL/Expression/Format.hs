module Language.Embedded.VHDL.Expression.Format where

import Language.VHDL (
    Expression(..)
  , Relation(..)
  , ShiftExpression(..)
  , SimpleExpression(..)
  , Term(..)
  , Factor(..)
  , Primary(..)
  , Identifier(..)
  )

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

class Kludge a
  where
    format :: a -> String

--------------------------------------------------------------------------------
-- ** Boolean

instance Kludge Bool where
  format True  = "1"
  format False = "0"

--------------------------------------------------------------------------------
-- ** Signed

instance Kludge Int8 where
  format = convert

instance Kludge Int16 where
  format = convert

instance Kludge Int32 where
  format = convert

instance Kludge Int64 where
  format = convert

--------------------------------------------------------------------------------
-- ** Unsigned

instance Kludge Word8 where
  format = convert

instance Kludge Word16 where
  format = convert

instance Kludge Word32 where
  format = convert

instance Kludge Word64 where
  format = convert

--------------------------------------------------------------------------------
-- * Converting Integers to their Binrary representation
--------------------------------------------------------------------------------

convert :: Integral a => a -> String
convert = foldr1 (++) . fmap w2s . B.unpack . i2bs . toInteger

--------------------------------------------------------------------------------

-- Found on SO
i2bs :: Integer -> B.ByteString
i2bs x = B.reverse . B.unfoldr (fmap go) . Just $ sign x
  where
    sign :: (Num a, Ord a) => a -> a
    sign | x < 0     = subtract 1 . negate
         | otherwise = id

    go :: Integer -> (Word8, Maybe Integer)
    go x = (b, i)
      where
        b = sign (fromInteger x)
        i | x >= 128  = Just (x `shiftR` 8)
          | otherwise = Nothing

-- I assum the negative ones to already be padded
w2s :: Word8 -> String
w2s w = printf "%08s" $ showIntAtBase 2 intToDigit w ""

--------------------------------------------------------------------------------
