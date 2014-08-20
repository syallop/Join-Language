module Join.Data.Count
    ( mkCount
    , waitZero
    , tick
    ) where

import Join

newtype Count = Count (SyncSignal (), Signal)

mkCount :: Int -> Process Count
mkCount i = do
    count <- newChannel
    tick  <- newChannel
    zero  <- newChannel

    count    & tick |> \n -> reply count (n-1)
    count&=0 & zero |> acknowledge zero

    sync count i
    return $ Count (zero,tick)

waitZero :: Count -> Process ()
waitZero (Count (z,_)) = syncSignal' z

tick :: Count -> Process ()
tick (Count (_,t)) = signal t

