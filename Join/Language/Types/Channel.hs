{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module Join.Language.Types.Channel
    (Channel()
    ,Synchronicity(..)
    ,getId
    ,InferSync()
    ,inferSync
    ) where

import Data.Serialize

-- | Synchronicity tag Type & Kind.
data Synchronicity
    = A -- ^ Asynchronous.
    | S -- ^ Synchronous.

-- | Identifies Channel's on which messages may be sent.
-- The intended semantics of a Channel are:
-- - The type parameter s denotes whether the channel should be Asynchronous (A) or Synchronous (S).
-- - The type parameter a denotes the type of values the channel
--   transports.
-- - The Int parameter to all constructors contains an Id unique per each
--   Channel.
data Channel s a where
    Channel  :: Serialize a => Int -> Channel A a
    SChannel :: Serialize a => Int -> Channel S a

instance Show (Channel s a) where
    show (Channel  i) = "Channel-"  ++ show i
    show (SChannel i) = "SChannel-" ++ show i

-- | Extract the unique Id of a Channel.
getId :: Channel s a -> Int
getId (Channel i) = i
getId (SChannel i) = i

-- | Closed Class of Synchronicity-kinded types which may be used to infer
-- how to construct a corresponding Channel.
-- => Calling 'inferSync INT :: Channel s a' in a context where 's' is
-- known (Either A/S) produces the corresponding type of Channel.
class    InferSync s where inferSync :: Serialize a => Int -> Channel s a
instance InferSync A where inferSync = Channel
instance InferSync S where inferSync = SChannel

