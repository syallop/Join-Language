{-# LANGUAGE MultiWayIf #-}
module Join.Language.Examples where

import Join.Language
import Join.Language.Types

import Control.Applicative
import Control.Monad.IO.Class

countDown :: Int -> ProcessM ()
countDown n = do
    -- Get a new Channel, inferred to be of type 'Channel Int'.
    intChannel <- newChannel

    -- Make a join definition:
    --  When there's an int on intChannel: 
    --    print it. If 0, do nothing.
    --    Otherwise spawn a de-incremented int on intChannel.
    intChannel |- (\i -> do liftIO $ print i
                            if i == 0
                              then inert
                              else send intChannel (i-1))

    -- Spawn the starting number on intChannel.
    send intChannel n

    -- Become inert.
    inert

fibonacci :: Int -> ProcessM Int
fibonacci i = do
    fib <- newChannel
    fib |- (\n -> if n <= 1 then reply fib 1
                            else do i <- sync fib (n-1)
                                    j <- sync fib (n-2)
                                    reply fib (wait i + wait j))
    wait <$> sync fib i

