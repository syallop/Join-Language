{-# LANGUAGE GADTs #-}
module Join.Language.Types where

-- | A typed Channel, uniquely identified by an Int
data Channel a = Channel Int

instance Show (Channel a) where
    show (Channel i) = "Channel-" ++ show i

-- | A combination of one or many Channel's
data Channels a where
    SingleChannel :: Channel a -> Channels a
    ConsChannel   :: Channel a -> Channels b -> Channels (a,b)

instance Show (Channels a) where
    show (SingleChannel c)  = show c
    show (ConsChannel c cs) = show c ++ "&" ++ show cs

-- | Infix, cons a Channel to Channels
(&) :: Channel a -> Channels b -> Channels (a,b)
c & cs = c `ConsChannel` cs

infixr 7 &

-- | Infix, cons a Channel to a final Channel.
(&|) :: Channel a -> Channel b -> Channels (a,b)
c &| d = c `ConsChannel` SingleChannel d
