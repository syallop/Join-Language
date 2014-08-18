module Join.Data.Lock
    ( Lock()
    , mkLock
    , lock
    , unlock
    , withLock
    ) where

import Join

{- Primitive Lock example -}
newtype Lock = Lock (SyncSignal (),SyncSignal ())
mkLock :: Process Lock
mkLock = do
    free   <- newChannel -- Enforce mutex      :: Chan ()
    lock   <- newChannel -- Request for locks  :: SyncChan () ()
    unlock <- newChannel -- Request for unlock :: SyncChan () ()

    -- Only when free, reply to a lock request.
    free & lock |> \_ _ -> acknowledge lock

    -- When unlock request, set free.
    unlock      |> \_ -> signal free `with` acknowledge unlock

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

