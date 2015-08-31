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
import Text.Printf (printf)
import Numeric     (showIntAtBase)
import qualified Data.ByteString as B

--------------------------------------------------------------------------------
-- *
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
