module PcgPure where

import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Generic as Vec
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Bits (finiteBitSize, FiniteBits, xor, unsafeShiftR,
                  unsafeShiftL, (.|.), (.&.))
import System.Random (randomIO)
import Control.Monad.Primitive (PrimMonad, PrimState)

newtype PcgState m = P (UM.MVector (PrimState m) Word64)

pcg32_self_init_vec :: IO (PcgState IO)
pcg32_self_init_vec = do
    initial <- randomIO
    increment <- (.|. 0x01) <$> randomIO
    let initial' = pcg32_advance_vec (initial + increment) increment
    pcg32_init initial' increment

pcg32_init :: PrimMonad m => Word64 -> Word64 -> m (PcgState m)
pcg32_init state increment = do
    v <- UM.unsafeNew 2
    UM.unsafeWrite v 0 state
    UM.unsafeWrite v 1 increment
    return $ P v

pcg32_advance_vec :: Word64 -> Word64 -> Word64
pcg32_advance_vec st inc = multiplier * st + inc
    where multiplier = 6364136223846793005

pcg32_int32 :: PrimMonad m => (Word64 -> Word32) -> PcgState m -> m Word32
pcg32_int32 permute (P st) = do
    state <- UM.unsafeRead st 0
    inc <- UM.unsafeRead st 1
    UM.unsafeWrite st 0 $! pcg32_advance_vec state inc
    return $! permute state
{-# SPECIALIZE pcg32_int32 :: (Word64 -> Word32) -> PcgState IO -> IO Word32 #-}

gen32 :: PrimMonad m => PcgState m -> m Word32
gen32 pcg = pcg32_int32 rxs_m_xs pcg
{-# SPECIALIZE gen32 :: PcgState IO -> IO Word32 #-}

-- xs . m . rxs
rxs_m_xs :: Word64 -> Word32
rxs_m_xs n = (`xorshift` funny) . (mcg *) . (`xorshift` (4 + top_bits n 4)) $ n
    where
    funny = (2 * 32 + 2) `quot` 3  -- from pcg_random; rxs_m_xs_mixin::output
    mcg = 12605985483714917081 :: Word64
{-# INLINE rxs_m_xs #-}

u32_from_bit :: Int -> Word64 -> Word32
u32_from_bit nbitsof p = fromIntegral $ p `unsafeShiftR` nbitsof

xsh_rr :: Word64 -> Word32
xsh_rr n =
    let xsh = xorshift n shift
        xsh_32 = u32_from_bit (64 - 32 - 5) xsh
    in xsh_32 `unsafe_rotr` top_bits xsh 5
    where shift = (64 - 32 + 5) `quot` 2
{-# INLINE xsh_rr #-}

-- | note the onus on caller to ensure that 0 < pos < bit_width a in
-- @unsafe_rotr a pos@
unsafe_rotr :: FiniteBits a => a -> Int -> a
unsafe_rotr a pos = shift_left .|. shift_right
    where
    shift_right = a `unsafeShiftR` pos
    shift_left = a `unsafeShiftL` (finiteBitSize a - pos)

xorshift :: FiniteBits a => a -> Int -> a
xorshift n steps = n `xor` (n `unsafeShiftR` steps)

top_bits :: (Integral a, FiniteBits a, Num b) => a -> Int -> b
top_bits n bits = fromIntegral $ n `unsafeShiftR` (finiteBitSize n - bits)

-- adhere to mwc-random's api
rand_bounded :: (PrimMonad m, Variate a, Integral a, Bounded a) =>
                (a, a) -> PcgState m -> m a
rand_bounded (i, j) st = case upper - lower + 1 of
    0 -> uniform st
    range ->
        let clamp = maxBound - (maxBound `rem` range)
            go = do
                r <- uniform st
                if r >= clamp then go else return $ r `rem` range + lower
        in go
    where (lower, upper) = if i < j then (i, j) else (j, i)
{-# INLINE rand_bounded #-}

-- adhere to mwc-random's api
class Variate a where
    uniform :: PrimMonad m => PcgState m -> m a
    uniformR :: PrimMonad m => (a, a) -> PcgState m -> m a

instance Variate Word8 where
    uniform pcg = fromIntegral <$> gen32 pcg
    {-# INLINE uniform #-}

    uniformR = rand_bounded
    {-# INLINE uniformR #-}

instance Variate Word16 where
    uniform pcg = fromIntegral <$> gen32 pcg
    {-# INLINE uniform #-}

    uniformR = rand_bounded
    {-# INLINE uniformR #-}

instance Variate Word32 where
    uniform = gen32
    {-# INLINE uniform #-}

    uniformR = rand_bounded
    {-# INLINE uniformR #-}

instance Variate Word64 where
    uniform pcg = do
        u <- fromIntegral <$> gen32 pcg
        l <- fromIntegral <$> gen32 pcg
        return $! (u `unsafeShiftL` 32) .|. l
    {-# INLINE uniform #-}

    uniformR = rand_bounded
    {-# INLINE uniformR #-}

instance Variate Int8 where
    uniform pcg = fromIntegral <$> gen32 pcg
    {-# INLINE uniform #-}

    uniformR = rand_bounded
    {-# INLINE uniformR #-}

instance Variate Int16 where
    uniform pcg = fromIntegral <$> gen32 pcg
    {-# INLINE uniform #-}

    uniformR = rand_bounded
    {-# INLINE uniformR #-}

instance Variate Int32 where
    uniform pcg = fromIntegral <$> gen32 pcg
    {-# INLINE uniform #-}

    uniformR = rand_bounded
    {-# INLINE uniformR #-}

instance Variate Int64 where
    uniform pcg = from_word64 <$> uniform pcg
    {-# INLINE uniform #-}

    uniformR = rand_bounded
    {-# INLINE uniformR #-}

instance Variate Float where
    -- | canonical [0, 1)
    uniform pcg = (/ max_) . fromIntegral <$> gen32 pcg
        where max_ = fromIntegral (maxBound :: Word32)
    {-# INLINE uniform #-}

    -- | [a, b). does not check that a <= b.
    uniformR (a, b) pcg = (\n -> n * (b - a) + a) <$> uniform pcg
    {-# INLINE uniformR #-}

instance Variate Double where
    -- | canonical [0, 1)
    uniform pcg = (/ max_) . from_word64 <$> uniform pcg
        where max_ = fromIntegral (maxBound :: Word64)
    {-# INLINE uniform #-}

    -- | [a, b). does not check that a <= b.
    uniformR (a, b) pcg = (\n -> n * (b - a) + a) <$> uniform pcg
    {-# INLINE uniformR #-}

instance Variate Bool where
    uniform pcg = toEnum . fromIntegral . (.&. 0x01) <$> gen32 pcg
    {-# INLINE uniform #-}

    -- | TODO: do calls to this even make sense?
    uniformR (a, b) pcg = to_bool <$> rand_bounded (from_bool a, from_bool b) pcg
        where
        to_bool :: Word32 -> Bool
        to_bool = toEnum . fromIntegral
        from_bool :: Bool -> Word32
        from_bool = fromIntegral . fromEnum
    {-# INLINE uniformR #-}

from_word64 :: Num a => Word64 -> a
from_word64 = fromIntegral

uniformVector :: (PrimMonad m, Vec.Vector v a, Variate a) =>
                 PcgState m -> Int -> m (v a)
uniformVector pcg veclen = Vec.replicateM veclen $ uniform pcg
