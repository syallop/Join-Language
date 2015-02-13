{-# LANGUAGE FlexibleContexts, RankNTypes #-}
{-|
Module      : Join.Data.Lock
Copyright   : (c) Samuel A. Yallop, 2014
Maintainer  : syallop@gmail.com
Stability   : experimental

A 'Lock' allows processes to coordinate exclusive access to a resource.

'lock' waits for a 'Lock' to be available while 'unlock' releases a held lock.

The 'withLock' convenience function ensures locks are released at the end of an operation.
-}
module Join.Data.Lock
    ( Lock()
    , mkLock
    , lock
    , unlock
    , withLock
    ) where

import Join

-- | A Lock allows Process's to coordinate access of a resource
newtype Lock = Lock (SyncSignal (),SyncSignal ())

mkLock :: Process Lock
mkLock = do
    free   <- newChannel -- Enforce mutex      :: Chan ()
    lock   <- newChannel -- Request for locks  :: SyncChan () ()
    unlock <- newChannel -- Request for unlock :: SyncChan () ()

    -- Only when free, reply to a lock request.
    def $ free & lock |> acknowledge lock

    -- When unlock request, set free.
       |$ unlock      |> signal free `with` acknowledge unlock

    signal free
    return $ Lock (lock,unlock)

-- | Block until a lock is acquired.
lock :: Lock -> Process ()
lock (Lock (l,_)) = syncSignal' l

-- | Release a lock.
unlock :: Lock -> Process ()
unlock (Lock (_,u)) = syncSignal' u

-- | Acquire lock before running a process. (unlocking afterward).
withLock :: Lock -> Process () -> Process ()
withLock l p = lock l >> p >> unlock l

