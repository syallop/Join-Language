module Join.Language.Examples where

import Join.Language

import Control.Monad.IO.Class

countDown :: Int -> ProcessM ()
countDown n = do
    -- Get a new Channel, inferred to be of type 'Channel Int'.
    intChannel <- newChannel

    -- Make a join definition:
    --  When there's an int on intChannel: 
    --    print it. If 0, do nothing.
    --    Otherwise spawn a de-incremented int on intChannel.
    def (on $ All intChannel) (\i -> do liftIO $ print i
                                        if i == 0
                                          then inert
                                          else send intChannel (i-1))

    -- Spawn the starting number on intChannel.
    send intChannel n

    -- Become inert.
    inert

    -- Instructions below ignored <= previous 'inert' instruction.
    send intChannel 1000

