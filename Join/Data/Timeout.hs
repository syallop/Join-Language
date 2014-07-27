module Join.Data.Timeout
    ( mkTimeout
    ) where

import Join

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class
import Data.Serialize

mkTimeout :: Serialize r => Int -> (a -> r) -> a -> Process (Maybe r)
mkTimeout t f x = do
    wait     <- newChannel
    finished <- newChannel
    timeout  <- newChannel

    wait & finished |> \() r  -> reply wait (Just r)
    wait & timeout  |> \() () -> reply wait Nothing

    send finished (f x) `with` (do liftIO $ threadDelay t
                                   signal timeout)
    syncSignal' wait

