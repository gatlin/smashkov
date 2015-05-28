{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Main where

import Prelude hiding (take, map)
import Tasks
import Bigram

import FreeStream hiding (for)
import System.IO (Handle(..), withFile, IOMode(..), stdout)
import System.Environment (getArgs)
import qualified Data.Map.Strict as M
import Control.Monad (forever)
import qualified Data.Text as T

main :: IO ()
main = do
    args <- getArgs
    let text = if length args == 0 then "sample.txt" else (args !! 0)
    mp <- withFile text ReadMode $ \hndl -> do
        mp <- readBigrams hndl
        return mp
    run $ walk mp >< take 10 >< map T.unpack >< display
    return ()
