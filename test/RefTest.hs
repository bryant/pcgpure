module RefTest where

import Data.Word (Word32, Word64)
import Foreign.ForeignPtr (withForeignPtr, newForeignPtr)
import Foreign.Ptr (FunPtr, Ptr)
import PcgPure (pcg32_init, pcg32_int32, rxs_m_xs, xsh_rr)
import Control.Monad (void)

data XshRr

foreign import ccall "init_xsh_rr" init_xsh_rr :: Word64 -> IO (Ptr XshRr)
foreign import ccall "&free_xsh_rr" free_xsh_rr :: FunPtr (Ptr XshRr -> IO ())
foreign import ccall "next_xsh_rr" next_xsh_rr :: Ptr XshRr -> IO Word32

new_xsh_rr seed = init_xsh_rr seed >>= newForeignPtr free_xsh_rr

data RxsMXs

foreign import ccall "init_rxs_m_xs" init_rxs_m_xs :: Word64 -> IO (Ptr RxsMXs)
foreign import ccall "&free_rxs_m_xs" free_rxs_m_xs :: FunPtr (Ptr RxsMXs -> IO ())
foreign import ccall "next_rxs_m_xs" next_rxs_m_xs :: Ptr RxsMXs -> IO Word32

new_rxs_m_xs seed = init_rxs_m_xs seed >>= newForeignPtr free_rxs_m_xs

default_increment :: Word64
default_increment = 1442695040888963407

compare_gens :: Int -> IO Word32 -> IO Word32 -> IO [(Word32, Word32)]
compare_gens rounds gen gen' =
    zip <$> sequence (replicate rounds gen) <*> sequence (replicate rounds gen')

mk_pcg permute state = do
    let st = state + default_increment  -- from pcg_random.hpp
    gen <- pcg32_init st default_increment
    let next = pcg32_int32 permute gen
    void next  -- "state := bump(state + increment)" from pcg_random.hpp
    return next

main = do
    pcgpure <- mk_pcg rxs_m_xs 23
    reference <- new_rxs_m_xs 23

    outputs <- compare_gens 1024 pcgpure (withForeignPtr reference next_rxs_m_xs)

    putStrLn "rxs_m_xs (actual, expected):"
    sequence_ [log actual expected | (actual, expected) <- outputs]
    where
    log a e
        | a == e = putStrLn $ "✓ " ++ case_
        | otherwise = putStrLn $ "✗ " ++ case_
        where case_ = show a ++ "\t" ++ show e
