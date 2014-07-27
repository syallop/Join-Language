{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DataKinds #-}


module Join.Examples where

import Join.Language
import Join.Types
{-import Join.Interpretation.Simple-}
import Join.Interpretation.Complex

import Prelude hiding (take, read)

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad (liftM,replicateM_,replicateM)
import Control.Monad.IO.Class
import Data.Serialize hiding (get,put)
import System.IO

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
newtype Counter = Counter (SyncChan () (), SyncChan () Int)
mkCounter :: Process Counter
mkCounter = do
    count <- newChannel -- :: Chan Int
    inc   <- newChannel -- :: SyncChan () ()
    get   <- newChannel -- :: SyncChan () Int

    inc & count |> \_ n -> reply inc () `with` send count (n+1)
    get & count |> \_ n -> reply get n  `with` send count n

    send count 1

    return $ Counter (inc,get)

-- | Increment counter, waiting until complete.
inc :: Counter -> Process ()
inc (Counter (i,_)) = sync i () >> inert

-- | Get current value, waiting until complete.
get :: Counter -> Process Int
get (Counter (_,g)) = sync' g ()

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




{- Buffer example: -}
newtype Buffer a = Buffer (Chan a, SyncChan () a)
mkBuffer :: Serialize a => Process (Buffer a)
mkBuffer = do
    p <- newChannel       -- put channel  :: Chan a
    t <- newChannel       -- take channel :: SyncChan a ()
    t & p |> \_-> reply t -- reply put's to take's
    return $ Buffer (p,t)

-- | Asynchronously put a message on the buffer.
put :: Serialize a => Buffer a -> a -> Process ()
put (Buffer (p,_)) = send p

-- | Synchronously take a message on the buffer.
take :: Serialize a => Buffer a -> Process (SyncVal a)
take (Buffer (_,t)) = syncSignal t



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




{- Primitive Lock example -}
newtype Lock = Lock (SyncSignal (),SyncSignal ())
mkLock :: Process Lock
mkLock = do
    free   <- newChannel -- Enforce mutex      :: Chan ()
    lock   <- newChannel -- Request for locks  :: SyncChan () ()
    unlock <- newChannel -- Request for unlock :: SyncChan () ()

    -- Only when free, reply to a lock request.
    free & lock |> \_ -> reply lock

    -- When unlock request, set free.
    unlock      |> \_ -> signal free `with` reply unlock()

    signal free
    return $ Lock (lock,unlock)

-- | Block until a lock is acquired.
lock :: Lock -> Process ()
lock (Lock (l,_)) = sync' l ()

-- | Release a lock.
unlock :: Lock -> Process ()
unlock (Lock (_,u)) = sync' u ()

-- | Acquire lock before running a process. (unlocking afterward).
withLock :: Lock -> Process () -> Process ()
withLock l p = lock l >> p >> unlock l

-- | Only one subprocess may hold the lock at a time.
-- =>"One" "Two" or "Two" "One". No intermingling.
lockExample :: Process ()
lockExample = mkLock >>= \l -> withLock l (liftIO $ putStrLn "One")
                        `with` withLock l (liftIO $ putStrLn "two")





{- Barrier example -}
newtype Barrier = Barrier (SyncChan () (), SyncChan () ())
mkBarrier :: Process Barrier
mkBarrier = do
    l <- newChannel
    r <- newChannel
    l & r |> \_ _ -> reply l () `with` reply r ()
    return $ Barrier (l,r)

-- | 'Left side' waits at barrier.
signalLeft :: Barrier -> Process ()
signalLeft (Barrier (l,_)) = syncSignal' l

-- | 'Right side' waits at barrier.
signalRight :: Barrier -> Process ()
signalRight (Barrier (_,r)) = syncSignal' r

-- | Barriers enforce a subProcesses move in step.
-- => Result is "(lr)" or "(rl)".
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

