module Join.Data.Pair
    ( Pair()
    , mkPair
    , getPair
    , getFst
    , getSnd
    ) where

import Join

import Control.Applicative

type Pair a b = SyncSignal (a,b)
mkPair :: (MessageType a, MessageType b) => a -> b -> Process (Pair a b)
mkPair a b = do
    unPair <- newChannel
    def $ unPair |> reply unPair (a,b)

    return unPair

getPair :: (MessageType a, MessageType b) => Pair a b -> Process (a,b)
getPair c = syncSignal' c

getFst :: (MessageType a, MessageType b) => Pair a b -> Process a
getFst c = fst <$> getPair c

getSnd :: (MessageType a, MessageType b) => Pair a b -> Process b
getSnd c = snd <$> getPair c

