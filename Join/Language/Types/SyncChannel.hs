{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Join.Language.Types.SyncChannel
    (SyncChannel(..)
    ) where

import Join.Language.Types.Channel

-- | A typed Synchronous channel. Internally encapsulates two channels
-- where the first represents the Channel sent to, and the second the
-- channel replied on.
data SyncChannel a = SyncChannel (Channel a) (Channel a)
    deriving (Eq,Ord)

instance Show (SyncChannel a) where
    show (SyncChannel (Channel i) (Channel j)) = "SyncChannel-(" ++ show i ++ "," ++ show j ++ ")"

instance Spawnable SyncChannel a where
    getChannel (SyncChannel c _) = c

