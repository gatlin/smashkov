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
import Options.Applicative

-- * Command line option parsing stuff. Nothing interesting here.

data CmdOpts = CmdOpts
    { _file :: String
    , _genHowMany :: String
    }

opts :: ParserInfo CmdOpts
opts = info (parser <**> helper) (fullDesc <> progDesc d)
    where
        parser = CmdOpts
            <$> strOption
                    (   long "input"
                    <>  short 'i'
                    <>  value "sample.txt")
            <*> strOption
                    (   long "howmany"
                    <>  short 'n'
                    <>  value "5")
        d = "a shitty tool for making shitty markov models"

-- * Somebody once told me ...

main :: IO ()
main = do
    opts' <- execParser opts
    -- open the file and read the bigrams into the appropriate structure
    mp <- withFile (_file opts') ReadMode $ \hndl -> do
        mp <- readBigrams hndl
        return mp

    -- now, generate sentences of random length and write them to stdout
    forM_ [1..(read $ _genHowMany opts')] $ \_ -> do
        howMany <- evalRandIO $ getRandomR (4, 20)
        run $ walk mp >< take howMany >< map (`T.snoc` ' ') >< writeText stdout
        putStrLn "\n---"
