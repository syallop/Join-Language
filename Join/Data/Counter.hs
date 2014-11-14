{-|
Module      : Join.Data.Counter
Copyright   : (c) Samuel A. Yallop, 2014
Maintainer  : syallop@gmail.com
Stability   : experimental

A 'Counter' counts a number of events signalled by 'inc'
, which can be queried with 'get'.

-}
module Join.Data.Counter
    ( Counter()
    , mkCounter
    , inc
    , get
    ) where

import Join

-- | Track a number of signals.
newtype Counter = Counter (SyncChan () (), SyncChan () Int)

mkCounter :: Process Counter
mkCounter = do
    count <- newChannel -- :: Chan Int
    inc   <- newChannel -- :: SyncChan () ()
    get   <- newChannel -- :: SyncChan () Int

    def $ inc & count |> (\n -> acknowledge inc `with` send count (n+1))
       |$ get & count |> \n -> reply get n     `with` send count n

    send count 1

    return $ Counter (inc,get)

-- | Increment counter
inc :: Counter -> Process ()
inc (Counter (i,_)) = syncSignal i >> inert

-- | Get current value
get :: Counter -> Process Int
get (Counter (_,g)) = syncSignal' g

