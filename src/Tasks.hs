{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Tasks where

import Prelude hiding (map, filter)
import qualified Prelude as P
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO
import Control.Monad
import Data.Char
import Data.IORef

import FreeStream

readChunks :: Handle -> Source T.Text IO ()
readChunks hndl = loop where
    loop = do
        ch <- lift $! T.hGetChunk hndl
        if T.null ch
            then yield ch >> return ()
            else yield ch >> loop

toChars :: Task T.Text Char IO ()
toChars = do
    ch <- await
    if T.null ch
        then return ()
        else do
            each $ T.unpack ch
            toChars

groupBy :: (Char -> Bool) -> Task Char T.Text IO ()
groupBy pred = loop T.empty where
    loop acc = do
        ch <- await
        if (pred ch)
            then do
                unless (T.null acc) $
                    yield $ T.reverse acc
                loop T.empty
            else loop $ T.cons ch acc

tokenize :: Task Char T.Text IO ()
tokenize = groupBy isSpace
{-# INLINE tokenize #-}

count :: IORef Int -> Sink a IO ()
count r = forever $! do
    it <- await
    lift $ modifyIORef' r $ \n -> n + 1

bigrams :: Task T.Text (T.Text, T.Text) IO ()
bigrams = do
    first <- await
    loop first
    where
        loop prev = do
            curr <- await
            yield (prev, curr)
            loop curr

writeChunks :: Handle -> Sink T.Text IO ()
writeChunks hndl = loop where
    loop = do
        ch <- await
        if T.null ch
            then return ()
            else do
                lift $ T.hPutStr hndl ch
                loop
