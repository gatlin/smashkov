{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}

{-
This module is just the 'main' function and some boring administrative code.

The real meat is in the `Bigram.hs` and `Parsing.hs` modules, and a separate
project of mine that I'm using called [Tubes][1]. It is extensively
documented freely licensed.

[1]: http://niltag.net/tubes
-}

module Main where

import Prelude hiding (take, map)
import Parsing
import Bigram

import Tubes hiding (for)
import System.IO (Handle(..), withFile, IOMode(..), stdout)
import System.Environment (getArgs)
import qualified Data.Map.Strict as M
import Control.Monad (forever, forM_)
import qualified Data.Text as T
import Control.Monad.Random
import Options.Applicative

-- * Command line option parsing stuff. Nothing interesting here.

data CmdOpts = CmdOpts
    { _file :: String
    , _maxLen :: String
    , _minLen :: String
    }

opts :: ParserInfo CmdOpts
opts = info (parser <**> helper) (fullDesc <> progDesc d)
    where
        parser = CmdOpts
            <$> strOption
                    (   long "input"
                    <>  short 'i'
                    <>  value "input.txt")
            <*> strOption
                    (   long "max"
                    <>  value "20" )
            <*> strOption
                    (   long "min"
                    <>  value "10" )

        d = "a shitty tool for making shitty markov models"

-- * Somebody once told me ...

start :: CmdOpts -> IO ()
start CmdOpts{..} = do
    -- open the file and read the bigrams into the appropriate structure
    mp <- withFile _file ReadMode $ \hndl -> do
        mp <- readBigrams hndl
        return mp

    let min = read _minLen
    let max = read _maxLen
    -- now, generate sentences of random length and write them to stdout
    howMany <- evalRandIO $ getRandomR (min, max)
    run $ walk mp >< take howMany >< map (`T.snoc` ' ') >< writeText stdout
    putStrLn ""

main :: IO ()
main = execParser opts >>= start
