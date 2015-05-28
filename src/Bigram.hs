{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module Bigram where

import Tasks
import FreeStream

import Prelude hiding (map, take)
import System.IO
import Data.Text (Text(..))
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.Foldable
import Data.Traversable
import System.Random
import Control.Monad.Random as R
import Control.Monad (forever)

type Bigram    = (Text, Text)
type BigramMap = M.Map Text (M.Map Text Rational)

-- | Grab all the bigrams from the given file handle
readBigrams :: Handle -> IO BigramMap
readBigrams hndl = reduce (\mp x -> insertBigram x mp) M.empty id go
    where
        go = readText hndl >< toChars >< tokenize >< bigrams
        {-# INLINE go #-}

-- | Insert a new bigram into a map of bigram counts
insertBigram :: Bigram -> BigramMap -> BigramMap
insertBigram (a, b) mp =
    case M.lookup a mp of
        Just bs -> case M.lookup b bs of
            Just count -> M.insert a (M.adjust (+1) b bs) mp
            Nothing    -> M.insert a (M.insert b 1 bs) mp
        Nothing -> M.insert a (M.singleton b 1) mp

-- | Given a 'BigramMap' generates steps in a walk through the markov chain
walk :: BigramMap -> Source Text IO ()
walk mp = do
    counts <- traverse (\mp' -> return $ M.foldl (+) 0 mp') mp
    start  <- lift $ R.fromList $ M.toList counts
    loop start

    where
        loop wrd = do
            yield wrd
            let nexts = mp M.! wrd
            next <- lift $ R.fromList $ M.toList nexts
            loop next
