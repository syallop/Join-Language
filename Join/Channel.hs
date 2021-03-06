{-# LANGUAGE
    DataKinds
  , DeriveDataTypeable
  , FlexibleInstances
  , GADTs
  , GeneralizedNewtypeDeriving
  , KindSignatures
  , MultiParamTypeClasses
  , PolyKinds
  , RankNTypes
  , StandaloneDeriving
  #-}
{-|
Module      : Join.Channel
Copyright   : (c) Samuel A. Yallop, 2014
Maintainer  : syallop@gmail.com
Stability   : experimental

This module defines a 'Channel' type, a representation of typed Join-Calculus Channels.

Channels themselves do not provide a means of queuing messages, what they provide is a means
for a Join-Calculus program to:

 (1) Track where it should route messages sent on the channel.

  2. Enforce the type of messages allowed.

  3. Determine whether reply-values are allowed/ expected.

The first point is facilitated by the id each Channel is constructed with.
The second and third, by the type variables.

'inferChannel' allows an implementation to infer whether a newly created Channel
should be Asynchronous or Synchronous based upon it's usage.
For example, a reply function could be defined to operate exclusively on Synchronous Channels,
then, if reply is called on a channel, it can be inferred that it is intended to be Synchronous
and the user can omit type signatures. By constraining the synchronicity type variable in an
implementation sufficiently, type annotations may be able to be avoided entirely.
-}

module Join.Channel
    (ChanId(..)
    ,Channel()
    ,Chan
    ,SyncChan
    ,Synchronicity(..)
    ,getId
    ,InferChannel(inferChannel)

    ,SignalChannel
    ,Signal
    ,SyncSignal
    ,module Join.Message
    ) where

import Data.Typeable
import Join.Message

-- | System-wide unique channel id.
newtype ChanId = ChanId {unChanId :: Int} deriving (Show,Eq,Ord,Enum,Num)

-- | Synchronicity tag Type & Kind.
data Synchronicity (r :: *)
    = A   -- ^ Asynchronous.
    | S r -- ^ Synchronous, with return type.
  deriving Typeable

deriving instance Typeable A
deriving instance Typeable S

-- | Class of valid synchronous reply types.
class SyncType s
instance SyncType A
instance MessageType r
      => SyncType (S r)

-- | A Channel uniquely identifies a port of communication.
--
-- The type parameter 'a' denotes the type of values accepted.
--
-- The type parameter 's' is of kind 'Synchronicity' and denotes whether
-- values should be sent 'A'synchronously (a regular channel as defined by
-- the core calculus) or 'S'ynchronously, where a return value is expected.
--
-- Channels are constructed with an ChanId parameter which serves as it's
-- unique ID. Interpreters should ensure these are unique.
data Channel (s :: Synchronicity *) (a :: *) where
    -- Asynchronous channel.
    AChannel
      :: MessageType a
      => ChanId -> Channel A a

    -- Synchronous channel.
    SChannel
      :: (MessageType a,MessageType r)
      => ChanId -> Channel (S r) a

-- | Synonym for asynchronous 'Channel's.
type Chan (a :: *) = Channel A a

-- | Synonym for synchronous 'Channel's.
type SyncChan a r = Channel (S r) a

instance Show (Channel s a) where
    show (AChannel (ChanId i)) = "AChannel-" ++ show i
    show (SChannel (ChanId i)) = "SChannel-" ++ show i

-- | Extract the Id of a Channel.
getId :: Channel s a -> ChanId
getId (AChannel i) = i
getId (SChannel i) = i

-- | Infer the constructor for a 'Channel' from the required 'Synchronicity'-kinded
-- type.
class (MessageType a,SyncType s) => InferChannel s a where
  inferChannel :: ChanId -> Channel s a

instance MessageType a => InferChannel A a where
  inferChannel = AChannel

instance (MessageType a,MessageType r) => InferChannel (S r) a where
  inferChannel = SChannel

-- | Synonym for 'Channel's which receive signals - the unit value ().
type SignalChannel s = Channel s ()

-- | Synonym for asynchronous 'Channel's which receive signals - the unit
-- value ().
type Signal = SignalChannel A

-- | Synonym for synchronous 'Channel's which receive signals -the unit
-- value ()- and reply with some 'r'.
type SyncSignal r = SignalChannel (S r)

