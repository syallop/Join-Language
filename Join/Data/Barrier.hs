module Join.Data.Barrier
    ( Barrier()
    , mkBarrier
    , signalLeft
    , signalRight
    ) where

import Join

-- | Barriers enforce a subProcesses move in step.
-- => Result is "(lr)" or "(rl)".
newtype Barrier = Barrier (SyncChan () (), SyncChan () ())
mkBarrier :: Process Barrier
mkBarrier = do
    l <- newChannel
    r <- newChannel
    def $ l & r |> acknowledge l `with` acknowledge r
    return $ Barrier (l,r)

-- | 'Left side' waits at barrier.
signalLeft :: Barrier -> Process ()
signalLeft (Barrier (l,_)) = syncSignal' l

-- | 'Right side' waits at barrier.
signalRight :: Barrier -> Process ()
signalRight (Barrier (_,r)) = syncSignal' r

