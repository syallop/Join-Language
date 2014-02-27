{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Join.Language.Types.SyncChannel
    (SyncChannel(..)
    ) where

import Join.Language.Types.Channel

import Data.Serialize

-- | A typed Synchronous channel on which messages may be sent, and a reply
-- is synchronously waited for.
data SyncChannel a = SyncChannel (Channel a)
    deriving (Eq,Ord)

instance Show (SyncChannel a) where
    show (SyncChannel (Channel i)) = "SyncChannel-" ++ show i

instance Serialize a => ChannelLike SyncChannel a where
    getChannel (SyncChannel c) = c

