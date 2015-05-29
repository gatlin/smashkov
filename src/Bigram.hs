{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module Bigram where

import Tubes
import Tubes

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

{-
A bigram is a sequence of two words which occurred, one after the other, in a
text document.

Using bigrams we can count which words followed which and how often, yielding
a probability distribution called a "language model."

Then from a given word *w* we can choose a next word, *x*, based on how
frequently it followed *w* in the source text.

Doing this in a loop generates new sentences.
-}
type Bigram    = (Text, Text)
type BigramMap = M.Map Text (M.Map Text Rational)

-- | Grab all the bigrams from the given file handle and put them in a
-- 'BigramMap'. Uses functions from the Tubes module.
readBigrams :: Handle -> IO BigramMap
readBigrams hndl = reduce (\mp x -> insertBigram x mp) M.empty id go
    where
        go = readText hndl >< toChars >< tokenize >< bigrams
        {-# INLINE go #-}

        insertBigram :: Bigram -> BigramMap -> BigramMap
        insertBigram (a, b) mp =
            case M.lookup a mp of
                Just bs -> case M.lookup b bs of
                    Just count -> M.insert a (M.adjust (+1) b bs) mp
                    Nothing    -> M.insert a (M.insert b 1 bs) mp
                Nothing -> M.insert a (M.singleton b 1) mp
        {-# INLINE insertBigram #-}

-- | Given a 'BigramMap', generates steps in a walk through the markov chain
walk :: BigramMap -> Source Text IO ()
walk mp = do
    -- to pick a start word, I just count up how many bigrams each word
    -- was the start of and pick from that distribution
    counts <- traverse (\mp' -> return $ M.foldl (+) 0 mp') mp
    start  <- lift $ R.fromList $ M.toList counts
    loop start

    where
        loop wrd = do
            yield wrd
            let nexts = mp M.! wrd
            next <- lift $ R.fromList $ M.toList nexts
            loop next
