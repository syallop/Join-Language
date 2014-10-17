{-# LANGUAGE GADTs #-}
module Join.Data.Timeout
    ( mkTimeout
    ) where

import Join

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class
import Data.Serialize

mkTimeout :: (Serialize r, MessagePassed r) => Int -> (a -> r) -> a -> Process (Maybe r)
mkTimeout t f x = do
    wait     <- newChannel -- SyncSignal (Maybe fin)
    finished <- newChannel -- Chan fin
    timeout  <- newChannel -- Signal

    def $ (wait & finished |> \r  -> reply wait (Just r))
       |$ wait & timeout  |> reply wait Nothing

    send finished (f x) `with` (do liftIO $ threadDelay t
                                   signal timeout)
    syncSignal' wait

