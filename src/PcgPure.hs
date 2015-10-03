module PcgPure where

import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word32, Word64)
import Data.Bits (finiteBitSize, FiniteBits, xor, unsafeShiftR, rotateR, (.|.))
import System.Random (randomIO)

type PcgState = UM.IOVector Word64

pcg32_self_init_vec :: IO PcgState
pcg32_self_init_vec = do
    initial <- randomIO
    increment <- (.|. 0x01) <$> randomIO
    let initial' = pcg32_advance_vec (initial + increment) increment
    pcg32_init initial' increment

pcg32_init :: Word64 -> Word64 -> IO PcgState
pcg32_init state increment = do
    v <- UM.unsafeNew 2
    UM.unsafeWrite v 0 state
    UM.unsafeWrite v 1 increment
    return v

pcg32_advance_vec :: Word64 -> Word64 -> Word64
pcg32_advance_vec st inc = multiplier * st + inc
    where multiplier = 6364136223846793005

pcg32_int32_io :: (Word64 -> Word32) -> PcgState -> IO Word32
pcg32_int32_io adv st = do
    state <- UM.unsafeRead st 0
    inc <- UM.unsafeRead st 1
    UM.unsafeWrite st 0 $ pcg32_advance_vec state inc
    return $ adv state

rxs_m_xs :: Word64 -> Word32
rxs_m_xs n =
    let rxs_m_32 = u32_from_bit 32 $ mcg * xorshift n (4 + top_bits n 4)
    in xorshift rxs_m_32 funny
    where
    funny = (2 * 32 + 2) `quot` 3
    mcg = 12605985483714917081 :: Word64

u32_from_bit :: Int -> Word64 -> Word32
u32_from_bit nbitsof p = fromIntegral $ p `unsafeShiftR` nbitsof

xsh_rr :: Word64 -> Word32
xsh_rr n =
    let xsh = xorshift n shift
        xsh_32 = u32_from_bit (64 - 32 - 5) xsh
    in xsh_32 `rotateR` top_bits xsh 5
    where shift = (64 - 32 + 5) `quot` 2

xorshift :: FiniteBits a => a -> Int -> a
xorshift n steps = n `xor` (n `unsafeShiftR` steps)

top_bits :: (Integral a, FiniteBits a, Num b) => a -> Int -> b
top_bits n bits = fromIntegral $ n `unsafeShiftR` (finiteBitSize n - bits)

rand_range :: (Word32, Word32) -> PcgState -> IO Word32
rand_range (i, j) gen = go
    where
    go = do
        r <- pcg32_int32_io rxs_m_xs gen
        if r >= clamp then go else return $ r `quot` buckets + lower

    (lower, upper) = if i < j then (i, j) else (j, i)
    range = upper - lower + 1
    -- bos' technique
    buckets = maxBound `quot` range
    clamp = buckets * range
