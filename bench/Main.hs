{-# LANGUAGE BangPatterns #-}

module BenchMain where

import Data.IORef
import Data.Word (Word32)
import Criterion.Main (defaultMain, bgroup, bench, nfIO, nf)
import Control.Monad.State (runState)
import PcgPure
import qualified System.Random.MWC as MWC
import qualified Data.Vector.Unboxed as Unboxed

main = do
    mwcgen <- MWC.createSystemRandom
    pcggen <- pcg32_self_init_vec
    bench_main mwcgen pcggen

pcgvec len ref = Unboxed.replicateM len (pcg32_int32_io ref)

bench_main mwc pcg = defaultMain
    [ bgroup (show veclen) [
        bench "pcgpure" . nfIO $ do
            pcgvec veclen pcg
        , bench "mwc-random" . nfIO $ do
            MWC.uniformVector mwc veclen :: IO (Unboxed.Vector Word32)
        ]
    | veclen <- map (10 ^) [3..4]]
