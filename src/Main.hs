{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Main where

import Tasks
import Bigram
import FreeStream hiding (for)
import System.IO (Handle(..), withFile, IOMode(..))
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let text = if length args == 0 then "sample.txt" else (args !! 0)
    withFile text ReadMode $ \hndl -> do
        mp <- readBigrams hndl >>= computeProbs
        putStrLn . show $ mp
        return ()
