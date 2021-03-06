{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

{-
This module contains code for streaming text from files and transforming them
into streams of bigrams.

The Tube, Source, and Sink types and streaming utilities come from another
thing I wrote, FreeStream: http://niltag.net/tubes

-}

module Parsing where

import Prelude hiding (map, filter)
import qualified Prelude as P
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO
import Control.Monad
import Data.Char

import Tubes

-- | Emits chunks of 'T.Text' values from the supplied file handle, and
-- terminates with 'T.empty'.
readText :: Handle -- ^ IO handle to read from
         -> Source T.Text IO ()
readText hndl = loop where
    loop = do
        ch <- lift $! T.hGetChunk hndl
        if T.null ch
            then yield ch >> return ()
            else yield ch >> loop

-- | Breaks a 'T.Text' value into a stream of 'Char's.
toChars :: Tube T.Text Char IO ()
toChars = do
    ch <- await
    if T.null ch
        then return ()
        else do
            each $ T.unpack ch
            toChars

-- | Groups a stream of 'Char's into a 'T.Text' string based on the supplied
-- predicate
groupBy :: (Char -> Bool) -- ^ predicate function. Values giving @True@ are the separators)
        -> Tube Char T.Text IO ()
groupBy pred = loop T.empty where
    loop acc = do
        ch <- await
        if (pred ch)
            then do
                unless (T.null acc) $
                    yield $ T.reverse acc
                loop T.empty
            else loop $ T.cons ch acc

-- | Convenience function to group a stream of 'Char's along whitespace
-- boundaries
tokenize :: Tube Char T.Text IO ()
tokenize = groupBy isSpace >< map T.toLower
{-# INLINE tokenize #-}

-- | Converts a stream of arbitrary values into a stream of bigrams
bigrams :: Tube a (a, a) IO ()
bigrams = do
    first <- await
    loop first
    where
        loop prev = do
            curr <- await
            yield (prev, curr)
            loop curr

-- | Writes 'T.Text' values to the supplied file handle.
writeText :: Handle -> Sink T.Text IO ()
writeText hndl = loop where
    loop = do
        ch <- await
        if T.null ch
            then return ()
            else do
                lift $ T.hPutStr hndl ch
                loop
