module Join.Data.Counter
    ( Counter()
    , mkCounter
    , inc
    , get
    ) where

import Join

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

