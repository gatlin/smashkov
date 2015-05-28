{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

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

type Bigram    = (Text, Text)
type BigramMap = M.Map Text (M.Map Text Double)

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

-- | Convert a map of bigram counts to succession probabilities
computeProbs :: Monad m => BigramMap -> m BigramMap
computeProbs mp = do
    totals <- traverse (\mp' -> return $ M.foldl (+) 0 mp') mp
    freqMap <- forWithKey mp $ \k mp' -> do
        let total = totals M.! k
        return $ M.map (/ total) mp'
    return freqMap
    where
        forWithKey :: Applicative t => M.Map k a -> (k -> a -> t b) -> t (M.Map k b)
        forWithKey = flip M.traverseWithKey
        {-# INLINE forWithKey #-}
