{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad

import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.FilePath ((</>))

main :: IO ()
main = do
	jacky <- newTChanIO :: IO (TChan Bool)
	forkIO $ do
		let biglist = take 20000 $ map (foldl (+) 2) $ iterate (3:) []
		loopConsume jacky biglist
	loopReport jacky
	where
		loopConsume :: TChan Bool -> [Integer] -> IO ()
		loopConsume talker []
			= atomically $ writeTChan talker False
		loopConsume talker xs = let !xs' = drop 2000 xs in
			(atomically $ writeTChan talker True)
			-- Report contents of work to the screen, since drop won't thunk.
			-- If you remove this line, printout is determined, else disorder.
			>> print (xs!!5)
			>> loopConsume talker xs'
		loopReport :: TChan Bool -> IO ()
		loopReport talker = do
			flag <- atomically $ readTChan talker
			if not flag then return () else print flag >> loopReport talker
