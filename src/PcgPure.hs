{-# LANGUAGE BangPatterns #-}

module PcgPure where

--import System.IO (withFile, IOMode(ReadMode), Handle)
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.IORef (IORef, readIORef, modifyIORef')
import Data.Word (Word32, Word64)
import Data.Bits (finiteBitSize, FiniteBits, xor, unsafeShiftR, rotateR, (.|.))
import qualified Data.ByteString as B
import Control.Monad.State (get, put, State, runState)
import System.Random

{-
class Pcgable st where
    width_state :: Int
    advance :: st -> st
    pcg_self_init :: IO (Pcg64State st)
    pcg_init :: st -> Pcg64State st

newtype Pcg64State st = Pcg64State st
-}

data Pcg64State
    = Pcg64State
        {-# UNPACK #-} !Word64  -- state
        {-# UNPACK #-} !Word64  -- inc

type Pcg64 = State Pcg64State

pcg32_self_init :: IO Pcg64State
pcg32_self_init = do
    initial <- randomIO
    increment <- randomIO
    return $ Pcg64State initial increment

pcg32_self_init_vec :: IO (UM.IOVector Word64)
pcg32_self_init_vec = do
    initial <- randomIO
    increment <- (.|. 0x01) <$> randomIO
    let initial' = pcg32_advance_vec (initial + increment) increment
    pcg32_init initial' increment

pcg32_init :: Word64 -> Word64 -> IO (UM.IOVector Word64)
pcg32_init state increment = do
    v <- UM.unsafeNew 2
    UM.unsafeWrite v 0 state
    UM.unsafeWrite v 1 increment
    return v

{-
pcg32_init = withFile "/dev/urandom" ReadMode $ \h -> do
    st <- B.hGetSome h 8
    -- multiplier <- B.hGetSome h 8
    let multiplier = 6364136223846793005
    increment <- B.hGetSome h 8
    return . Pcg64State st $ \st -> st * multiplier + increment

get_some :: FiniteBits a => Handle -> a -> IO a
get_some handle numty = foldl' g
    B.unpack B.hGetSome handle width
-}

pcg32_advance :: Pcg64State -> Pcg64State
pcg32_advance (Pcg64State st inc) = Pcg64State st inc
    where
    multiplier = 6364136223846793005
    st' = multiplier * st + inc

pcg32_advance_vec :: Word64 -> Word64 -> Word64
pcg32_advance_vec st inc = multiplier * st + inc
    where multiplier = 6364136223846793005

pcg32_int32 :: Pcg64 Word32
pcg32_int32 = do
    st <- get
    let !st'@(Pcg64State state _) = {- pcg32_advance -} st
    --put st'
    return $ fromIntegral {- permute -}state

pcg32_int32_io :: (Word64 -> Word32) -> UM.IOVector Word64 -> IO Word32
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
{-
    where
    !rv = xored `rotateR` rot
    !xored = xorshift n ((64 - 32 + 5) `quot` 2)
    !rot = fromIntegral (top_bits n 5)
-}

xorshift :: FiniteBits a => a -> Int -> a
xorshift n steps = n `xor` (n `unsafeShiftR` steps)

top_bits :: (Integral a, FiniteBits a, Num b) => a -> Int -> b
top_bits n bits = fromIntegral $ n `unsafeShiftR` (finiteBitSize n - bits)

rand_range :: (Word32, Word32) -> UM.IOVector Word64 -> IO Word32
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
