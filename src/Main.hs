{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

{-
This module is just the 'main' function and some boring administrative code.

The real meat is in the `Bigram.hs` and `Tasks.hs` modules, and a separate
project of mine that I'm using called [FreeStream][1]. It is extensively
documented freely licensed.

[1]: http://niltag.net/FreeStream
-}

module Main where

import Prelude hiding (take, map)
import Tasks
import Bigram

import FreeStream hiding (for)
import System.IO (Handle(..), withFile, IOMode(..), stdout)
import System.Environment (getArgs)
import qualified Data.Map.Strict as M
import Control.Monad (forever, forM_)
import qualified Data.Text as T
import Control.Monad.Random

-- * Command line option parsing stuff. Nothing interesting here.

data CmdOpts = CmdOpts
    { _file :: String
    , _genHowMany :: Int
    }

defaultOpts :: CmdOpts
defaultOpts = CmdOpts { _file = "sample.txt", _genHowMany = 5 }

parseOpts :: [String] -> IO CmdOpts
parseOpts args = case length args of
    0       -> return defaultOpts
    _       -> let f = args !! 0
                   n = args !! 1
               in  return $ CmdOpts { _file = f, _genHowMany = read n }

-- * Somebody once told me ...

main :: IO ()
main = do
    args <- getArgs             -- \
    opts <- parseOpts args      --  +- parse command line arguments

    -- open the file and read the bigrams into the appropriate structure
    mp <- withFile (_file opts) ReadMode $ \hndl -> do
        mp <- readBigrams hndl
        return mp

    -- now, generate sentences of random length and write them to stdout
    forM_ [1..(_genHowMany opts)] $ \_ -> do
        howMany <- evalRandIO $ getRandomR (4, 20)
        run $ walk mp >< take howMany >< map (`T.snoc` ' ') >< writeText stdout
        putStrLn "\n---"
