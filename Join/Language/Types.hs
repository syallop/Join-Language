{-# LANGUAGE GADTs #-}
module Join.Language.Types where

-- | A typed Channel, uniquely identified by an Int
data Channel a = Channel Int

-- | A combination of one or many Channel's
data Channels a where
    SingleChannel :: Channel a -> Channels a
    ConsChannel   :: Channel a -> Channels b -> Channels (a,b)

-- | Infix, cons a Channel to Channels
(&) :: Channel a -> Channels b -> Channels (a,b)
c & cs = c `ConsChannel` cs

infixr 7 &

-- | Infix, cons a Channel to a final Channel.
(&|) :: Channel a -> Channel b -> Channels (a,b)
c &| d = c `ConsChannel` SingleChannel d
