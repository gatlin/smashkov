{-# LANGUAGE OverloadedStrings #-}

import Tasks
import Criterion.Main
import FreeStream

import Prelude hiding (map, filter)
import System.IO
import Data.IORef
import Data.Functor.Identity

import Control.Monad (forever)

brown_bigrams :: IO Int
brown_bigrams = do
    n <- newIORef 0
    withFile "brown.txt" ReadMode $ \hndl -> do
        run $ readChunks hndl
           >< toChars
           >< tokenize
           >< bigrams
           >< count n
           >< cat
        return ()
    total <- readIORef n
    return total

freestreamTest n = run $ each [1..n] >< map (+1) >< filter even >< cat

main :: IO ()
main = defaultMain
    [ bgroup "IO"
        [ bench "bigrams" $ whnfIO brown_bigrams  ]

    , bgroup "Identity"
        [ bench "freestream" $ whnf (runIdentity . freestreamTest) 10000 ]
    ]

