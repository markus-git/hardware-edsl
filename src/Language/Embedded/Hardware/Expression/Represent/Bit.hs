{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}

module Language.Embedded.Hardware.Expression.Represent.Bit
  ( Bit
  , Bits
  , ni
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

  , UBits
  , forgetBits
  , recallBits
  )
  where

import Language.Embedded.Hardware.Expression.Represent

import Language.Embedded.VHDL            (VHDL)
import Language.Embedded.VHDL.Monad      (newSym, newLibrary, newImport)
import Language.Embedded.VHDL.Monad.Type

import Data.Ix
import Data.Typeable
import Data.Bits hiding (Bits)
import qualified Data.Bits as Bit (Bits)

import Control.Monad   (guard)
import Control.DeepSeq (NFData(..))

import Data.Char (intToDigit)
import qualified Numeric as N

import GHC.TypeLits

--------------------------------------------------------------------------------
-- * ...
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** Single bit.

type Bit = Bool

--------------------------------------------------------------------------------
-- These aren't great to have..

instance Real Bool
  where
    toRational = error "toRational not implemented for bit."

instance Integral Bool
  where
    toInteger True  = 1
    toInteger False = 0
    quotRem         = error "quotRem not implemented for bit."

--------------------------------------------------------------------------------
-- ** Bit vectors of known lenght.

newtype Bits (n :: Nat) = B Integer

instance forall n. KnownNat n => Inhabited (Bits n)
  where
    reset = bitFromInteger 0

instance forall n. KnownNat n => Rep (Bits n)
  where
    declare  = declareBits
    format   = show . bitToInteger
    bits b   = '\"' : (tail $ bitShowBin b) ++ ['\"'] -- *** why tail?

deriving instance Typeable (Bits n)

declareBits :: forall proxy n. KnownNat n => proxy (Bits n) -> VHDL Type
declareBits _ = declareBoolean >> return (std_logic_vector size)
  where size = fromInteger (ni (undefined :: Bits n))
        
--------------------------------------------------------------------------------
-- *** ...

ni :: KnownNat n => proxy n -> Integer
ni = fromIntegral . natVal

norm :: KnownNat n => Bits n -> Bits n
norm b@(B n) = B (n .&. ((1 `shiftL` fromInteger (ni b)) - 1))

bitFromInteger :: KnownNat n => Integer -> Bits n
bitFromInteger i = norm (B i)

bitToInteger :: Bits n -> Integer
bitToInteger (B i) = i

--------------------------------------------------------------------------------
-- *** ...

lift1 :: KnownNat n => (Integer -> Integer) -> Bits n -> Bits n
lift1 f (B i) = norm (B (f i))

lift2 :: KnownNat n => (Integer -> Integer -> Integer) -> Bits n -> Bits n -> Bits n
lift2 f (B i) (B j) = norm (B (f i j))

--------------------------------------------------------------------------------
-- *** ...

bitAdd :: KnownNat n => Bits n -> Bits n -> Bits n
bitAdd = lift2 (+)

bitSub :: KnownNat n => Bits n -> Bits n -> Bits n
bitSub = lift2 (-)

bitMul :: KnownNat n => Bits n -> Bits n -> Bits n
bitMul = lift2 (*)

bitAdd' :: Bits n -> Bits n -> Bits (n + 1)
bitAdd' (B i) (B j) = B (i + j)

bitSub' :: Bits n -> Bits n -> Bits (n + 1)
bitSub' (B i) (B j) = B (i - j)

bitMul' :: Bits n -> Bits n -> Bits (n + n)
bitMul' (B i) (B j) = B (i * j)

bitQuotRem :: Bits n -> Bits n -> (Bits n, Bits n)
bitQuotRem (B i) (B j) = let (a, b) = quotRem i j in (B a, B b)

bitNeg :: KnownNat n => Bits n -> Bits n
bitNeg = lift1 negate

bitMinBound :: Bits n
bitMinBound = B 0

bitMaxBound :: KnownNat n => Bits n
bitMaxBound = norm (B (-1))

bitSigNum :: Bits n -> Bits n
bitSigNum (B i) = B (signum i)

bitAnd :: Bits n -> Bits n -> Bits n
bitAnd (B i) (B j) = B (i .&. j)

bitOr :: Bits n -> Bits n -> Bits n
bitOr (B i) (B j) = B (i .|. j)

bitXor :: Bits n -> Bits n -> Bits n
bitXor (B i) (B j) = B (i .|. j)

bitComplement :: KnownNat n => Bits n -> Bits n
bitComplement = lift1 complement

bitSplit :: (KnownNat m, KnownNat n) => proxy m -> Bits (m + n) -> (Bits m, Bits n)
bitSplit m (B i) = (a, b)
  where a = B (i `shiftR` fromInteger (ni m))
        b = bitFromInteger i

bitJoin :: KnownNat n => Bits m -> Bits n -> Bits (m + n)
bitJoin (B i) b@(B j) = B (shiftL i (fromInteger (ni b)) .|. j)

bitCoerce :: forall n m. (KnownNat n, KnownNat m) => Bits n -> Maybe (Bits m)
bitCoerce b@(B i) = guard (ni b == ni d) >> return (B i)
  where d = undefined :: Bits m

bitShiftR :: Bits n -> Int -> Bits n
bitShiftR (B i) n = B (shiftR i n)

bitShiftL :: KnownNat n => Bits n -> Int -> Bits n
bitShiftL b n = lift1 (`shiftL` n) b

bitTestBit :: Bits n -> Int -> Bool
bitTestBit (B i) n = testBit i n

bitRotate :: KnownNat n => Bits n -> Int -> Bits n
bitRotate b@(B i) n
  | si < 2    = b
  | otherwise = bitOr (bitFromInteger (shiftL i n)) (bitFromInteger (shiftR i (si - n)))
  where n' = mod n si
        si = fromInteger (ni b)

bitToList :: KnownNat n => Bits n -> [Bool]
bitToList b = map (bitTestBit b) [start, start - 1 .. 0]
  where start = fromInteger (ni b)

bitReadsPrec :: KnownNat n => Int -> ReadS (Bits n)
bitReadsPrec p txt = [ (bitFromInteger b, cs) | (b, cs) <- readsPrec p txt ]

bitShowBin :: KnownNat n => Bits n -> String
bitShowBin = map sh . bitToList
  where sh x = if x then '1' else '0'

bitShowHex :: KnownNat n => Bits n -> String
bitShowHex b@(B i) = zeros (N.showHex i "")
  where zeros n = replicate (len - length n) '0' ++ n
        len     = div (fromInteger (ni b) + 3) 4

--------------------------------------------------------------------------------

instance Show (Bits n) where
  showsPrec p (B x) = showsPrec p x

instance KnownNat n => Read (Bits n) where
  readsPrec = bitReadsPrec

instance Eq (Bits n) where
  B i == B j = i == j

instance NFData (Bits n) where
  rnf (B i) = seq i ()

instance Ord (Bits n) where
  compare (B i) (B j) = compare i j

instance KnownNat n => Bounded (Bits n) where
  minBound = bitMinBound
  maxBound = bitMaxBound

instance KnownNat n => Num (Bits n) where
  (+)           = bitAdd
  (-)           = bitSub
  (*)           = bitMul
  negate        = bitNeg
  abs           = id
  signum        = bitSigNum
  fromInteger   = bitFromInteger

instance KnownNat n => Bit.Bits (Bits n) where
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

instance KnownNat n => Real (Bits n) where
  toRational (B i) = toRational i

instance KnownNat n => Enum (Bits n) where
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

instance KnownNat n => Integral (Bits n) where
  toInteger = bitToInteger
  quotRem   = bitQuotRem

instance KnownNat n => Ix (Bits n) where
  range   = undefined
  index   = undefined
  inRange = undefined

--------------------------------------------------------------------------------
-- ** Bit vectors of unknown lenght.

newtype UBits = UB Integer
  deriving (Eq, Enum, Ord, Num, Real, Integral)

instance Rep UBits
  where
    declare = declareUBits
    format  = show
    -- *** This is bad and produces a warning in vhdl as there's no guarantee
    --     that the lenght of the printed binary will be the expected one.
    --     Give UB an extra 'Maybe Integer' for storing the length whenever its
    --     available.
    bits (UB i) = '\"' : (N.showIntAtBase 2 intToDigit i "") ++ ['\"']
    

declareUBits :: proxy UBits -> VHDL Type
declareUBits _ = declareBoolean >> return std_logic

--------------------------------------------------------------------------------

forgetBits :: Bits n -> UBits
forgetBits b = UB (bitToInteger b)

recallBits :: KnownNat n => UBits -> Bits n
recallBits (UB i) = (B i)

--------------------------------------------------------------------------------

instance Show UBits where
  showsPrec p (UB x) = showsPrec p x

--------------------------------------------------------------------------------
