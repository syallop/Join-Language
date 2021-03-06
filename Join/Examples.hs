{-# LANGUAGE
    DataKinds
  , FlexibleContexts
  , MultiWayIf
  , RankNTypes
  , ScopedTypeVariables
  #-}
{-|
Module      : Join.Examples
Copyright   : (c) Samuel A. Yallop, 2014
Maintainer  : syallop@gmail.com
Stability   : experimental

This module gives several small example programs, most testing the data structures
defined in "Join.Data".

-}
module Join.Examples where

import Prelude hiding (take, read)

import Join
import Join.Pattern.Pass

import Join.Data.Barrier
import Join.Data.Buffer
import Join.Data.Count
import Join.Data.Counter
import Join.Data.JVar
import Join.Data.Lock

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad (liftM,replicateM_,replicateM)



-- | Countdown all values to 0.
--
-- >countDown n = do
-- >    -- Get a new Channel, inferred to be of type 'Chan Int'.
-- >    intChannel <- newChannel
-- >
-- >    -- Make a join definition:
-- >    --  When there's an int on intChannel: 
-- >    --    If 0, do nothing.
-- >    --    Otherwise, print and send the de-incremented int.
-- >    def $ intChannel&=0 |> inert
-- >       |$ intChannel    |> \i -> do ioAction $ print i
-- >                                    send intChannel (i-1)
-- >
-- >    -- Send the starting number on intChannel and initiate the countdown.
-- >    send intChannel n
--
-- @ run $ countDown 5 @
--
-- > 5
-- > 4
-- > 3
-- > 2
-- > 1
countDown :: Int -> Process ()
countDown n = do
    -- Get a new Channel, inferred to be of type 'Chan Int'.
    intChannel <- newChannel

    -- Make a join definition:
    --  When there's an int on intChannel: 
    --    If 0, do nothing.
    --    Otherwise, print and send the de-incremented int.
    def $ intChannel&=0 |> inert
       |$ intChannel    |> \i -> do ioAction $ print i
                                    send intChannel (i-1)

    -- Send the starting number on intChannel and initiate the countdown.
    send intChannel n

-- | Parallel computation of the fibonacci function.
--
-- >  fibonacci i = do
-- >      fib <- newChannel
-- >      def $ fib&=0 |> reply fib 1
-- >         |$ fib&=1 |> reply fib 1
-- >         |$ fib    |> \n -> do i <- sync fib (n-1)
-- >                               j <- sync fib (n-2)
-- >                               reply fib (readResponse i + readResponse j)
-- >
-- >      sync' fib i
--
-- @ run $ fibonacci 15 @
--
-- > 987
fibonacci :: Int -> Process Int
fibonacci i = do
    fib <- newChannel

    def $ fib&=0 |> reply fib 1
       |$ fib&=1 |> reply fib 1
       |$ fib    |> \n -> do i <- sync fib (n-1)
                             j <- sync fib (n-2)
                             reply fib (readResponse i + readResponse j)

    sync' fib i

-- | 'Count' example:
--
-- @
--  countExample = do
--    ioAction $ putStrLn "Initialising Count to 100"
--    c <- mkCount 100
--
--    waitForZero c `with` tickDown c
--
--  where
--    waitForZero c = do
--        waitZero c
--        ioAction $ putStrLn "Counter reached zero"
--
--    tickDown c = do
--        replicateM_ 99 $ tick c
--        ioAction $ putStrLn "Sent 99 ticks, waiting."
--                 threadDelay 100000
--                 putStrLn "Sending 100th tick."
--        tick c
-- @
--
-- @ run countExample @
--
-- >Initialising Count to 100
-- >Sent 99 ticks, waiting.
-- >
-- >Sending 100th tick.
-- >Counter reached zero
countExample :: Process ()
countExample = do
    ioAction $ putStrLn "Initialising Count to 100"
    c <- mkCount 100

    waitForZero c `with` tickDown c

  where
    waitForZero c = do
        waitZero c
        ioAction $ putStrLn "Counter reached zero"

    tickDown c = do
        replicateM_ 99 $ tick c
        ioAction $ do putStrLn "Sent 99 ticks, waiting."
                      threadDelay 100000
                      putStrLn "Sending 100th tick."
        tick c

-- | Increment and query a 'Counter' with implicit mutex.
--
-- @
-- counterExample = do
--    c <- mkCounter
--
--    -- Make one inc, then check the value:
--    inc c
--    ioAction $ putStrLn "After one inc: "
--    get c >>= ioAction . print
--
--    -- Make 100 inc's, and check the value in 5 seconds
--    replicateM_ 100 (inc c)
--    ioAction $ putStrLn "After at most 100 inc, and 5 seconds: " >> threadDelay 5000000
--    get c >>= ioAction . print
--
--    inert
-- @
--
-- @ run counterExample @
--
-- > After one inc: 2
-- > After at most 100 inc, and 5 seconds: 102
counterExample :: Process ()
counterExample = do
    c <- mkCounter

    -- Make one inc, then check the value:
    inc c
    ioAction $ putStrLn "After one inc: "
    get c >>= ioAction . print

    -- Make 100 inc's, and check the value in 5 seconds
    replicateM_ 100 (inc c)
    ioAction $ putStrLn "After at most 100 inc, and 5 seconds: " >> threadDelay 5000000
    get c >>= ioAction . print

    inert

-- | Store some items in a buffer, retrieve them later. Simulates state.
--
-- @
-- bufferExample = do
--    -- Create a new Buffer
--    b <- mkBuffer
--
--    -- Put values in
--    put b 1
--    put b 2
--
--    -- Do things, wait a bit
--    ioAction $ putStrLn "Waiting..." >> threadDelay 100000
--
--    -- Get values out and do something with them
--    i <- take b
--    j <- take b
--
--    return $ readResponse i + readResponse j
-- @
--
-- @ run bufferExample @
--
-- > 3
bufferExample :: Process Int
bufferExample = do
    -- Create a new Buffer
    b <- mkBuffer

    -- Put values in
    put b 1
    put b 2

    -- Do things, wait a bit
    ioAction $ putStrLn "Waiting..." >> threadDelay 100000

    -- Get values out and do something with them
    i <- take b
    j <- take b

    return $ readResponse i + readResponse j

-- | Only one subprocess may hold the lock at a time.
--
-- @
-- lockExample :: Process ()
-- lockExample = mkLock >>= \l -> withLock l (ioAction $ putStrLn "One")
--                          `with` withLock l (ioAction $ putStrLn "two")
--
-- @
--
-- @ run lockExample @
--
-- > One
-- > Two
--
-- OR
--
-- > Two
-- > One
--
-- Printing from the two separate Process's is never intermingled.
lockExample :: Process ()
lockExample = mkLock >>= \l -> withLock l (ioAction $ putStrLn "One")
                        `with` withLock l (ioAction $ putStrLn "two")

-- | Use a 'Barrier' to co-ordinate two processes into printing
-- either (lr) or (rl).
--
-- > barrierExample = do
-- >     b <- mkBarrier
-- >     procLeft b `with` procRight b
-- >   where
-- >     procLeft b = do
-- >         ioAction$ putStrLn "("
-- >         signalLeft b
-- >         ioAction $ putStrLn "l"
-- >         signalLeft b
-- >         ioAction$ putStrLn ")"
-- >
-- >     procRight b = do
-- >         signalRight b
-- >         ioAction $ putStrLn "r"
-- >         signalRight b
--
-- @ run barrierExample @
--
-- > (
-- > l
-- > r
-- > )
--
-- OR
--
-- > (
-- > r
-- > l
-- > )
barrierExample :: Process ()
barrierExample = do
    b <- mkBarrier
    procLeft b `with` procRight b
  where
    procLeft :: Barrier -> Process ()
    procLeft b = do
        ioAction $ putStrLn "("
        signalLeft b
        ioAction $ putStrLn "l"
        signalLeft b
        ioAction $ putStrLn ")"

    procRight :: Barrier -> Process ()
    procRight b = do
        signalRight b
        ioAction $ putStrLn "r"
        signalRight b

-- | Use '&~' patterns to filter 5..15 on <10 / >=10
--
--
-- > predExample = do
-- >   intChannel <- newChannel :: Process (Chan Int)
-- >   printLock  <- mkLock
-- >
-- >   def $ (intChannel&~(<10) |> \i -> withLock printLock $
-- >                                       ioAction $ putStrLn $ show i ++ " is less than 10")
-- >      |$ (intChannel        |> \i -> withLock printLock $
-- >                                       ioAction $ putStrLn $ show i ++ " is greater than or equal to 10")
-- >
-- >   ioAction $ putStrLn "Sending numbers 5..15"
-- >   sendAll [(intChannel,i) | i <- [5..15]]
--
-- @ run predExample @
--
-- > 5 is less than or equal to 10
-- > ...
-- > 10 is less than or equal to 10
-- > 11 is greater than or equal to 10.
-- > ...
-- > 15 is greater than or equal to 10.
predExample :: Process ()
predExample = do
  intChannel <- newChannel :: Process (Chan Int)
  printLock  <- mkLock

  def $ (intChannel&~(<10) |> \i -> withLock printLock $
                                      ioAction $ putStrLn $ show i ++ " is less than 10")
     |$ (intChannel        |> \i -> withLock printLock $
                                      ioAction $ putStrLn $ show i ++ " is greater than or equal to 10")

  ioAction $ putStrLn "Sending numbers 5..15"
  sendAll [(intChannel,i) | i <- [5..15]]

-- | Concurrently run two Processes reading and writing to a JVar for 'n'
-- iterations.
--
-- @ run (jVarExample 2) @
--
-- > left: took "right"
-- > right: took "left"
-- > left: took "right"
-- > right: took "left"
jVarExample :: Int -> Process ()
jVarExample n = do
  jvar <- newJVar "initial value"

  spawn $ jvarLeft jvar n
  spawn $ jvarRight jvar n

  where
    jvarLeft jvar n = do
      val <- takeJVar jvar
      ioAction $ putStrLn $ "left: took " ++ show val
      putJVar jvar "left"
      if n < 0 then return () else jvarLeft jvar (n - 1)

    jvarRight jvar n = do
      val <- takeJVar jvar
      ioAction $ putStrLn $ "right: took " ++ show val
      putJVar jvar "right"
      if n < 0 then return () else jvarRight jvar (n - 1)

