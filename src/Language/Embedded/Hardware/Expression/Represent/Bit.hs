{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Embedded.Hardware.Expression.Represent.Bit
  ( Bit
  , bitFromInteger
  , bitToInteger
  , bitAdd
  , bitAdd'
  , bitSub
  , bitSub'
  , bitMul
  , bitMul'
  , bitQuotRem
  , bitNeg
  , bitReadsPrec
  , bitMinBound
  , bitMaxBound
  , bitSigNum
  , bitAnd
  , bitOr
  , bitXor
  , bitComplement
  , bitSplit
  , bitJoin
  , bitCoerce
  , bitShiftR
  , bitShiftL
  , bitTestBit
  , bitRotate
  , bitToList
  , bitShowBin
  , bitShowHex
  )
  where

import Data.Bits       (Bits(..))
import Control.Monad   (guard)
import Control.DeepSeq (NFData(..))

import qualified Numeric as N

import GHC.TypeLits

--------------------------------------------------------------------------------
-- * ...
--------------------------------------------------------------------------------

newtype Bit (n :: Nat) = B Integer

--------------------------------------------------------------------------------
-- ** ...

ni :: KnownNat n => proxy n -> Integer
ni = fromIntegral . natVal

norm :: KnownNat n => Bit n -> Bit n
norm b@(B n) = B (n .&. ((1 `shiftL` fromInteger (ni b)) - 1))

bitFromInteger :: KnownNat n => Integer -> Bit n
bitFromInteger i = norm (B i)

bitToInteger :: Bit n -> Integer
bitToInteger (B i) = i

--------------------------------------------------------------------------------
-- ** ...

lift1 :: KnownNat n => (Integer -> Integer) -> Bit n -> Bit n
lift1 f (B i) = norm (B (f i))

lift2 :: KnownNat n => (Integer -> Integer -> Integer) -> Bit n -> Bit n -> Bit n
lift2 f (B i) (B j) = norm (B (f i j))

--------------------------------------------------------------------------------
-- ** ...

bitAdd :: KnownNat n => Bit n -> Bit n -> Bit n
bitAdd = lift2 (+)

bitSub :: KnownNat n => Bit n -> Bit n -> Bit n
bitSub = lift2 (-)

bitMul :: KnownNat n => Bit n -> Bit n -> Bit n
bitMul = lift2 (*)

bitAdd' :: Bit n -> Bit n -> Bit (n + 1)
bitAdd' (B i) (B j) = B (i + j)

bitSub' :: Bit n -> Bit n -> Bit (n + 1)
bitSub' (B i) (B j) = B (i - j)

bitMul' :: Bit n -> Bit n -> Bit (n + n)
bitMul' (B i) (B j) = B (i * j)

bitQuotRem :: Bit n -> Bit n -> (Bit n, Bit n)
bitQuotRem (B i) (B j) = let (a, b) = quotRem i j in (B a, B b)

bitNeg :: KnownNat n => Bit n -> Bit n
bitNeg = lift1 negate

bitMinBound :: Bit n
bitMinBound = B 0

bitMaxBound :: KnownNat n => Bit n
bitMaxBound = norm (B (-1))

bitSigNum :: Bit n -> Bit n
bitSigNum (B i) = B (signum i)

bitAnd :: Bit n -> Bit n -> Bit n
bitAnd (B i) (B j) = B (i .&. j)

bitOr :: Bit n -> Bit n -> Bit n
bitOr (B i) (B j) = B (i .|. j)

bitXor :: Bit n -> Bit n -> Bit n
bitXor (B i) (B j) = B (i .|. j)

bitComplement :: KnownNat n => Bit n -> Bit n
bitComplement = lift1 complement

bitSplit :: (KnownNat m, KnownNat n) => proxy m -> Bit (m + n) -> (Bit m, Bit n)
bitSplit m (B i) = (a, b)
  where a = B (i `shiftR` fromInteger (ni m))
        b = bitFromInteger i

bitJoin :: KnownNat n => Bit m -> Bit n -> Bit (m + n)
bitJoin (B i) b@(B j) = B (shiftL i (fromInteger (ni b)) .|. j)

bitCoerce :: forall n m. (KnownNat n, KnownNat m) => Bit n -> Maybe (Bit m)
bitCoerce b@(B i) = guard (ni b == ni d) >> return (B i)
  where d = undefined :: Bit m

bitShiftR :: Bit n -> Int -> Bit n
bitShiftR (B i) n = B (shiftR i n)

bitShiftL :: KnownNat n => Bit n -> Int -> Bit n
bitShiftL b n = lift1 (`shiftL` n) b

bitTestBit :: Bit n -> Int -> Bool
bitTestBit (B i) n = testBit i n

bitRotate :: KnownNat n => Bit n -> Int -> Bit n
bitRotate b@(B i) n
  | si < 2    = b
  | otherwise = bitOr (bitFromInteger (shiftL i n)) (bitFromInteger (shiftR i (si - n)))
  where n' = mod n si
        si = fromInteger (ni b)

bitToList :: KnownNat n => Bit n -> [Bool]
bitToList b = map (bitTestBit b) [start, start - 1 .. 0]
  where start = fromInteger (ni b)

bitReadsPrec :: KnownNat n => Int -> ReadS (Bit n)
bitReadsPrec p txt = [ (bitFromInteger b, cs) | (b, cs) <- readsPrec p txt ]

bitShowBin :: KnownNat n => Bit n -> String
bitShowBin = map sh . bitToList
  where sh x = if x then '1' else '0'

bitShowHex :: KnownNat n => Bit n -> String
bitShowHex b@(B i) = zeros (N.showHex i "")
  where zeros n = replicate (len - length n) '0' ++ n
        len     = div (fromInteger (ni b) + 3) 4

--------------------------------------------------------------------------------

instance Show (Bit n) where
  showsPrec p (B x) = showsPrec p x

instance KnownNat n => Read (Bit n) where
  readsPrec = bitReadsPrec

instance Eq (Bit n) where
  B i == B j = i == j

instance NFData (Bit n) where
  rnf (B i) = seq i ()

instance Ord (Bit n) where
  compare (B i) (B j) = compare i j

instance KnownNat n => Bounded (Bit n) where
  minBound = bitMinBound
  maxBound = bitMaxBound

instance KnownNat n => Num (Bit n) where
  (+)           = bitAdd
  (-)           = bitSub
  (*)           = bitMul
  negate        = bitNeg
  abs           = id
  signum        = bitSigNum
  fromInteger   = bitFromInteger

instance KnownNat n => Bits (Bit n) where
  isSigned _    = False
  bit           = bitFromInteger . (2 ^)
  bitSize       = fromInteger . ni
  bitSizeMaybe  = Just . fromInteger . ni
  (.&.)         = bitAnd
  (.|.)         = bitOr
  xor           = bitXor
  complement    = bitComplement
  shiftR        = bitShiftR
  shiftL        = bitShiftL
  testBit       = bitTestBit
  rotate        = bitRotate
  popCount      = length . filter id . bitToList

instance KnownNat n => Real (Bit n) where
  toRational (B i) = toRational i

instance KnownNat n => Enum (Bit n) where
  toEnum i        = norm $ B $ toEnum i
  fromEnum (B i)  = fromEnum i
  succ i          = i + 1
  pred i          = if i == minBound then maxBound else i - 1
  enumFrom i      = enumFromTo i maxBound
  enumFromTo i j
    | i < j       = enumFromThenTo i (succ i) j
    | i == j      = [i]
    | otherwise   = []

  enumFromThen x y = enumFromThenTo x y bound
      where
        bound | x <= y    = maxBound
              | otherwise = minBound

  enumFromThenTo (B i) (B j) (B k) = map B (enumFromThenTo i j k)

instance KnownNat n => Integral (Bit n) where
  toInteger = bitToInteger
  quotRem   = bitQuotRem

--------------------------------------------------------------------------------
