{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Join.Language.Types.Channel
    (Channel(..)
    ,ChannelLike(..)
    ) where

-- | Describes Channel-like types on which messages may be asynchronously
-- sent.
class Show (c a) => ChannelLike c a where getChannel :: c a -> Channel a

-- | A typed Channel, uniquely identified by an Int.
data Channel a = Channel Int
    deriving (Eq,Ord)

instance Show (Channel a) where
    show (Channel i) = "Channel-" ++ show i

instance ChannelLike Channel a where
    getChannel = id
