{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}

module Join.Examples where

import Prelude hiding (take, read)

import Join

import Join.Interpretation.Basic

import Join.Data.Barrier
import Join.Data.Buffer
import Join.Data.Counter
import Join.Data.Lock

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad (liftM,replicateM_,replicateM)
import Control.Monad.IO.Class

-- | Countdown all values to 0.
countDown :: Int -> Process ()
countDown n = do
    -- Get a new Channel, inferred to be of type 'Channel Int'.
    intChannel <- newChannel

    -- Make a join definition:
    --  When there's an int on intChannel: 
    --    print it. If 0, do nothing.
    --    Otherwise spawn a de-incremented int on intChannel.
    intChannel |> \i -> do liftIO $ print i
                           if i == 0
                             then inert
                             else send intChannel (i-1)

    -- Spawn the starting number on intChannel.
    send intChannel n

    -- Become inert.
    inert

-- | Parallel computation of the fibonacci function.
fibonacci :: Int -> Process Int
fibonacci i = do
    fib <- newChannel
    fib |> \n -> if n <= 1 then reply fib 1
                           else do i <- sync fib (n-1)
                                   j <- sync fib (n-2)
                                   reply fib (read i + read j)
    sync' fib i


{- 'Counter' example: -}
-- | Increment and query a counter with implicit mutex.
counterExample :: Process ()
counterExample = do
    c <- mkCounter

    -- Make one inc, then check the value:
    inc c
    liftIO $ putStrLn "After one inc: "
    get c >>= liftIO . print

    -- Make 100 inc's, and check the value in 5 seconds
    replicateM_ 100 (inc c)
    liftIO $ putStrLn "After at most 100 inc, and 5 seconds: " >> threadDelay 5000000
    get c >>= liftIO . print

    inert

-- | Store some items in a buffer, retrieve them later. Simulates state.
-- pred> bufferExample == 3
bufferExample :: Process Int
bufferExample = do
    -- Create a new Buffer
    b <- mkBuffer

    -- Put values in
    put b 1
    put b 2

    -- Do things, wait a bit
    liftIO $ putStrLn "Waiting..." >> threadDelay 100000

    -- Get values out and do something with them
    i <- take b
    j <- take b

    return $ read i + read j

-- | Only one subprocess may hold the lock at a time.
-- =>"One" "Two" or "Two" "One". No intermingling.
lockExample :: Process ()
lockExample = mkLock >>= \l -> withLock l (liftIO $ putStrLn "One")
                        `with` withLock l (liftIO $ putStrLn "two")

{- Barrier example -}
barrierExample :: Process ()
barrierExample = do
    b <- mkBarrier
    procLeft b `with` procRight b
  where
    procLeft :: Barrier -> Process ()
    procLeft b = do
        liftIO $ putStrLn "("
        signalLeft b
        liftIO $ putStrLn "l"
        signalLeft b
        liftIO $ putStrLn ")"

    procRight :: Barrier -> Process ()
    procRight b = do
        signalRight b
        liftIO $ putStrLn "r"
        signalRight b

