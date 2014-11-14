{-# LANGUAGE ExtendedDefaultRules #-}
{-|
Module      : Join.Data.Count
Copyright   : (c) Samuel A. Yallop, 2014
Maintainer  : syallop@gmail.com
Stability   : experimental

A 'Count' structure may be used to wait for a number of events to finish.

-}
module Join.Data.Count
    ( mkCount
    , waitZero
    , tick
    ) where

import Join

newtype Count = Count (SyncSignal (), Signal)

-- | The 'Count' structure may wait for i events.
mkCount :: Int -> Process Count
mkCount i = do
    count <- newChannel
    tick  <- newChannel
    zero  <- newChannel

    def $ (count    & tick |> \n -> send count (n-1))
       |$ (count&=0 & zero |> acknowledge zero)

    send count i
    return $ Count (zero,tick)

-- | Wait until the 'Count' has recieved the expected number of events.
waitZero :: Count -> Process ()
waitZero (Count (z,_)) = syncSignal' z

-- | Indicate an event to a 'Count'.
tick :: Count -> Process ()
tick (Count (_,t)) = signal t

