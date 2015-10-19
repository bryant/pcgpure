module RefTest where

import qualified PcgPure as P

import Data.Word (Word32, Word64)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr, newForeignPtr)
import Foreign.Ptr (FunPtr, Ptr)
import Control.Monad (void, when)
import Data.IORef (readIORef, writeIORef, newIORef)

data XshRr

foreign import ccall "init_xsh_rr" init_xsh_rr :: Word64 -> IO (Ptr XshRr)
foreign import ccall "&free_xsh_rr" free_xsh_rr :: FunPtr (Ptr XshRr -> IO ())
foreign import ccall "next_xsh_rr" next_xsh_rr :: Ptr XshRr -> IO Word32

data RxsMXs

foreign import ccall "init_rxs_m_xs" init_rxs_m_xs :: Word64 -> IO (Ptr RxsMXs)
foreign import ccall "&free_rxs_m_xs" free_rxs_m_xs :: FunPtr (Ptr RxsMXs -> IO ())
foreign import ccall "next_rxs_m_xs" next_rxs_m_xs :: Ptr RxsMXs -> IO Word32

data Variant ffiptr
    = Variant
    { ref_init :: Word64 -> IO (ForeignPtr ffiptr)
    , ref_next :: ForeignPtr ffiptr -> IO Word32
    , permuter :: Word64 -> Word32
    }

rxs_m_xs :: Variant RxsMXs
rxs_m_xs = Variant
    { ref_init = \seed -> init_rxs_m_xs seed >>= newForeignPtr free_rxs_m_xs
    , ref_next = \fptr -> withForeignPtr fptr next_rxs_m_xs
    , permuter = P.rxs_m_xs
    }

xsh_rr :: Variant XshRr
xsh_rr = Variant
    { ref_init = \seed -> init_xsh_rr seed >>= newForeignPtr free_xsh_rr
    , ref_next = \fptr -> withForeignPtr fptr next_xsh_rr
    , permuter = P.xsh_rr
    }

default_increment :: Word64
default_increment = 1442695040888963407

test_variant :: Int -> Variant k -> Word64 -> IO (IO (Maybe (Word32, Word32)))
test_variant rounds variant seed = do
    refgen <- ref_init variant seed
    pcgnext <- mk_pcg (permuter variant) seed
    remaining <- newIORef rounds  -- what a hack
    let gen = do
        n <- readIORef remaining
        if n <= 0 then return Nothing else do
            writeIORef remaining $ n - 1
            Just <$> ((,) <$> ref_next variant refgen <*> pcgnext)
    return gen

mk_pcg permute state = do
    let st = state + default_increment  -- from pcg_random.hpp
    gen <- P.pcg32_init st default_increment
    let next = P.pcg32_int32 permute gen
    void next  -- "state := bump(state + increment)" from pcg_random.hpp
    return next

main :: IO ()
main = do
    putStrLn $ unlines ["rxs_m_xs" ,"========"]
    runner <- test_variant 256 rxs_m_xs 23
    loop runner verbosity

    putStrLn "\n"

    putStrLn $ unlines ["xsh_rr" ,"========"]
    runner <- test_variant 256 xsh_rr 23
    loop runner verbosity
    where
    verbosity = True
    loop r v = r >>= \res -> case res of
        Nothing -> return ()
        Just (expected, a)
            | expected == a -> when v (putStrLn $ "✓ " ++ case_) >> loop r v
            | otherwise -> putStrLn $ "✗ " ++ case_
            where case_ = show expected ++ "\t" ++ show a
