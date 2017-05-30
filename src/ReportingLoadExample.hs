{-# LANGUAGE BangPatterns #-}
module Main where

-- Heros of the day: http://learnyouahaskell.com/input-and-output
-- and the docs

import System.IO
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.DeepSeq (deepseq, NFData)

import System.FilePath ((</>))

main :: IO ()
main = do
	jacky <- newTChanIO :: IO (TChan Bool)
	francis <- newTVarIO [] :: IO (TVar [[Int]])
	forkIO $ reedyFn ("graphs"</>"GraphEdgeInds.txt") francis jacky
	-- loopReport jacky >> fmap length (readTVarIO francis) >>= print
	-- If you're not convinced,
	loopReport jacky >> fmap (!!402) (readTVarIO francis) >>= print
	where
		howmany = 1024 {- Effectively the number of bytes in a chunk (/ 4)d. -}
		loopReport :: TChan Bool -> IO ()
		loopReport talker = do
			flag <- atomically $ readTChan talker
			if not flag then return () else print flag >> loopReport talker
		-- (String rep of arg) (string's read-val) (storage on finish) (status chan)
		-- We use the String rep so we can match words in buffer w/ elems to eval.
		loopConsume :: String -> a -> TVar a -> TChan Bool -> IO ()
		loopConsume "" x sink talker = atomically $ do
			writeTVar sink x	-- (x) has been processed, so output.
			writeTChan talker False -- and tell people about it
		loopConsume xstr x sink talker = (atomically $ writeTChan talker True)
			>> loopConsume (tearOffChunk howmany xstr) x sink talker
		tearOffChunk :: NFData a => Int -> [a] -> [a]
		tearOffChunk = ((uncurry deepseq) .) . splitAt
		reedyFn :: FilePath -> TVar [[Int]] -> TChan Bool -> IO ()
		reedyFn file dest talker = withFile file ReadMode $ \handle -> do
			-- (Char)s are 32-bit words internally, last time I checked.
			hSetBuffering handle $ BlockBuffering (Just $ 4*howmany)
			contents <- hGetContents handle
			loopConsume contents (read contents :: [[Int]]) dest talker
