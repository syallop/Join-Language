{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DataKinds #-}

{-|
Module      : Join.Language.Examples
Copyright   : (c) Samuel A. Yallop, 2014
Maintainer  : syallop@gmail.com
Stability   : experimental

This module contains example "Join.Language" programs.

-}
module Join.Language.Examples where

import Join.Language
import Join.Types

import Prelude hiding (take)

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad (liftM,replicateM_,replicateM)
import Control.Monad.IO.Class
import Data.Serialize (Serialize)

countDown :: Int -> ProcessM ()
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

fibonacci :: Int -> ProcessM Int
fibonacci i = do
    fib <- newChannel
    fib |> \n -> if n <= 1 then reply fib 1
                           else do i <- sync fib (n-1)
                                   j <- sync fib (n-2)
                                   reply fib (wait i + wait j)
    wait <$> sync fib i

{- 'Counter' example: -}

newtype Counter = Counter (SyncChan () (), SyncChan () Int)
mkCounter :: ProcessM Counter
mkCounter = do
    count <- newChannel -- :: Chan Int
    inc   <- newChannel -- :: SyncChan () ()
    get   <- newChannel -- :: SyncChan () Int

    inc & count |> \_ n -> reply inc () `with` send count (n+1)
    get & count |> \_ n -> reply get n  `with` send count n

    send count 1

    return $ Counter (inc,get)

-- | Increment counter, waiting until complete.
inc :: Counter -> ProcessM ()
inc (Counter (i,_)) = wait <$> sync i ()

-- | Get current value, waiting until complete.
get :: Counter -> ProcessM Int
get (Counter (_,g)) = wait <$> sync g ()

counterExample :: ProcessM ()
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

{- Buffer example: -}

newtype Buffer a = Buffer (Chan a, SyncChan () a)
mkBuffer :: Serialize a => ProcessM (Buffer a)
mkBuffer = do
    p <- newChannel       -- put channel  :: Chan a
    t <- newChannel       -- take channel :: SyncChan a ()
    t & p |> \_-> reply t -- reply put's to take's
    return $ Buffer (p,t)

-- | Asynchronously put a message on the buffer.
put :: Serialize a => Buffer a -> a -> ProcessM ()
put (Buffer (p,_)) = send p

-- | Synchronously take a message on the buffer.
take :: Serialize a => Buffer a -> ProcessM (SyncVal a)
take (Buffer (_,t)) = sync t ()

bufferExample :: ProcessM Int
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

    return $ wait i + wait j

