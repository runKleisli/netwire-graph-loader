Netwire graph loader - Progress reporting example
======

> {-# LANGUAGE BangPatterns #-}
> module Main where

We write a program that checks the status of a progressive computation of a list, & is able to do other things like interact with the user in the mean time, by having one thread calculate parts of the list and report major progress while the main thread checks that status.

> import Control.Concurrent (forkIO)

Using (STM), we can express means of passing information between threads. Chiefly:

* The types of `TVar` for taking a value from a different thread & `TChan` for letting threads build a sequence together, or in our case to let the main thread read a sequence of status signals from the other fork. Importantly, `TChan`s are added to one value at a time, & unlike with consing onto a `TVar [a]`, a `TChan a` doesn't need to be resequenced each time a value is added. We will only use `TChan` in this example.
* An `STM a` converted to an `IO a` has the full `a` computed at a time, so the thunking is synchronized -- for instance, we don't have the logical inconsistencies in an `STM (a,b)` that would arise if the `a` and `b` components were computed in terms of values that other threads could modify between computing one or the other. This is why they're called atomic transactions, and the `STM a -> IO a` conversion is `atomically`. In other language, to compute `atomically x`, all terms w/c `x` depends on & are accessible to modification by multiple threads are threadlocked, then the value expressed by `x` is computed, then what it depends on is unlocked and the value is given over `IO`.

> import Control.Concurrent.STM

Asking for the value of `loopReport` at a channel means asking for `False` to appear on the channel and until then to print `True` whenever that appears; in our case, to wait for all chunks of a list to be computed and to print `False` once each chunk is.

It hangs until a `Bool` appears on the channel, then finishes if `True` but prints it and repeats if `False`.

> loopReport :: TChan Bool -> IO ()
> loopReport talker = do
> 	flag <- atomically $ readTChan talker
> 	if not flag then return () else print flag >> loopReport talker

We ask for `main` to compute this value given the `Bool`s on the `TChan Bool` come from a separate thread undertaking a significant computation of a list of values one chunk (so many entries of the list) at a time. `True` emitted represents a chunk computed, and `False` emitted represents the whole list computed.

> main :: IO ()
> main = do
> 	jacky <- newTChanIO :: IO (TChan Bool)
> 	forkIO $ do
> 		let biglist = take 20000 $ map (foldl (+) 2) $ iterate (3:) []
> 		loopConsume jacky biglist
> 	loopReport jacky

To have the list computed is for our purposes to compute or thunk every entry. Some understanding of strictness and Haskell's laziness is required, here.

We can't obtain the value of `x` in `forkIO x` from `main`, & the typing would always be `x :: ... -> IO ()`.

> loopConsume :: TChan Bool -> [Integer] -> IO ()

`main` could already be altered to reference the `[Integer]` computed. One might try printing `biglist` after `loopReport jacky` completes, & observe the print doesn't hang on values that are already computed. Hence, extracting the computation done by the forked thread isn't an issue here. In the load reporting example, we will see how to deal with the case where the value isn't accessible to `main`, only to the forked-off thread.

To compute in chunks, we shall treat the empty `[Integer]` as completely computed, consisting of no chunks. After the final chunk is computed, we write `False` onto the `TChan Bool` passed into `loopConsume` to communicate to the other threads with. This is how `loopReport` terminates.

> loopConsume talker [] = atomically $ writeTChan talker False

Otherwise, we want to take a chunk of 2000 entries to compute over, then treat whatever's left over as something to chunk and consume in the same fashion. Roughly,

< loopConsume talker xs = let !xs' = drop 2000 xs in
< 	loopConsume talker xs'

where using the bang pattern `!xs'` means the place the rest of the list starts from must be known before what follows `in` is computed. While strictifying, the bang pattern doesn't actually compute all the entries of a list, hence the remainder of the list isn't computed in all entries before `loopConsume` can proceed. Computing all such entries is putting the list in normal form (NF), whereas bang patterns only put the list in weak head normal form (WHNF).

We do want to evaluate all the entries in every chunk, in practice. However, for this demonstration it suffices to compute the first 5 entries of the chunk. We don't need to have the chunk represented as its own list to do this, since its entries are just the first 2000 of those of `xs`, the input.

< loopConsume talker xs = let !xs' = drop 2000 xs in
< 	print (xs!!5)
< 	>> loopConsume talker xs'

Lastly, to report progress, we report it each time a chunk is formed, writing `True` on the channel.

> loopConsume talker xs = let !xs' = drop 2000 xs in
> 	(atomically $ writeTChan talker True)
> 	-- Report contents of work to the screen, since drop won't thunk.
> 	-- If you remove this line, printout is determined, else disorder.
> 	>> print (xs!!5)
> 	>> loopConsume talker xs'

Note that the `Bool` gets printed from `main`, not this thread that prints `xs!!5`, and these actions are not synced in any way. So, one sees reports of chunks being demarcated out of sync with their 5th value being computed & printed. Multithreading!

Addendum
-----

We did not make any effort here to catch `STM` errors. Doing so is covered in [RWH - STM](http://book.realworldhaskell.org/read/software-transactional-memory.html).

Exercizes:
* Are the later byproducts of a forked-off thread still computed when the main thread is not expressed in terms of them, when their computation isn't needed to compute the value of `main`? One can `print` from the forked-off thread to detect if it's still computing. Does introducing `print` affect whether it's garbage collected?

Extra questions:
* Can a value unnecessary to `main` still lock a value `main` needs, or does having that value lock imply its value & hence that of `main` is dependent on how it comes to be unlocked, as far as STM admits in model?

References and background
-----

* [Documentation for the BangPatterns language extension](https://prime.haskell.org/wiki/BangPatterns)
* [Documentation for `seq`](https://hackage.haskell.org/package/base-4.9.0.0/docs/Prelude.html#v:seq)
* [Documentation for Control.DeepSeq](https://hackage.haskell.org/package/deepseq-1.4.2.0/docs/Control-DeepSeq.html), describing normal form & WHNF
* O'Sullivan, Stewart, Goerzen. "[Real World Haskell - Chapter 28. Software transactional memory](http://book.realworldhaskell.org/read/software-transactional-memory.html)"
* [STM documentation](https://hackage.haskell.org/package/stm-2.4.2)
