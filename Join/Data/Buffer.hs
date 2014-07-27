module Join.Data.Buffer
    ( Buffer()
    , mkBuffer
    , put
    , take
    ) where

import Prelude hiding (take)

import Join.Language
import Join.Types

import Data.Serialize (Serialize)

{- Buffer example:
 - A 'Buffer' is isomorphic to a Pi calculus channel.
 -}
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

