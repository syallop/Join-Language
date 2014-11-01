{-# LANGUAGE DataKinds, GADTs #-}
module Join.Data.Buffer
    ( Buffer()
    , mkBuffer
    , put
    , take
    ) where

import Prelude hiding (take)

import Join

{- Buffer example:
 - A 'Buffer' is isomorphic to a Pi calculus channel.
 -}
newtype Buffer a = Buffer (Chan a, SyncSignal a)
mkBuffer :: (MessageType a,MessagePassed a) => Process (Buffer a)
mkBuffer = do
    p <- newChannel  -- put channel  :: Chan a
    t <- newChannel  -- take channel :: SyncChan a ()
    def $ t & p |> reply t -- reply put's to take's
    return $ Buffer (p,t)

-- | Asynchronously put a message on the buffer.
put :: MessageType a => Buffer a -> a -> Process ()
put (Buffer (p,_)) = send p

-- | Synchronously take a message on the buffer.
take :: MessageType a => Buffer a -> Process (Response a)
take (Buffer (_,t)) = syncSignal t

