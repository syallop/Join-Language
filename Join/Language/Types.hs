{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Join.Language.Types
    (Channel(..)
    ,SyncChannel(..)
    ,Spawnable
    ) where

-- | Describes Channel-like types on which messages may be asynchronously
-- spawned.
class Show (c a) => Spawnable c a where getChannel :: c a -> Channel a

-- | A typed Channel, uniquely identified by an Int.
data Channel a = Channel Int
    deriving (Eq,Ord)

instance Show (Channel a) where
    show (Channel i) = "Channel-" ++ show i

instance Spawnable Channel a where
    getChannel = id

-- | A typed Synchronous channel. Internally encapsulates two channels
-- where the first represents the Channel sent to, and the second the
-- channel replied on.
data SyncChannel a = SyncChannel (Channel a) (Channel a)
    deriving (Eq,Ord)

instance Show (SyncChannel a) where
    show (SyncChannel (Channel i) (Channel j)) = "SyncChannel-(" ++ show i ++ "," ++ show j ++ ")"

instance Spawnable SyncChannel a where
    getChannel (SyncChannel c _) = c
