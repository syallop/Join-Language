module Join.Language.Types
    (Channel(Channel)
    )where

-- | A typed Channel, uniquely identified by an Int
data Channel a = Channel Int
    deriving (Eq, Ord)
instance Show (Channel a) where
    show (Channel i) = "Channel-" ++ show i

