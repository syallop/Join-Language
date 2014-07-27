module Join.Data.Pair
    ( Pair()
    , mkPair
    , getPair
    , getFst
    , getSnd
    ) where

import Join

import Control.Applicative
import Data.Serialize

type Pair a b = SyncSignal (a,b)
mkPair :: (Serialize a, Serialize b) => a -> b -> Process (Pair a b)
mkPair a b = do
    unPair <- newChannel
    unPair |> \_ -> reply unPair (a,b)

    return unPair

getPair :: (Serialize a, Serialize b) => Pair a b -> Process (a,b)
getPair c = syncSignal' c

getFst :: (Serialize a, Serialize b) => Pair a b -> Process a
getFst c = fst <$> getPair c

getSnd :: (Serialize a, Serialize b) => Pair a b -> Process b
getSnd c = snd <$> getPair c

