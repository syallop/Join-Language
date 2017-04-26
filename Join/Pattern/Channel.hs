{-# LANGUAGE
    ConstraintKinds
  , DataKinds
  , FlexibleInstances
  , FunctionalDependencies
  , MultiParamTypeClasses
  , TypeFamilies
  , UndecidableInstances
  #-}
module Join.Pattern.Channel where

import Join.Pattern.Rep

import Join.Channel
import Join.Message

import Data.Typeable

-- | The simplest pattern is a 'Channel s a' type.
-- This declares a pattern that matches all messages sent on the 'Channel'.
--
-- There is one exception, which is when the message type of the channel is '()'.
-- Because the unit type '()' only has one value (also named '()') explicitly passing the
-- value to the trigger is unnecessary.
-- The corresponding trigger type therefore does not accept a value.
type ChannelPattern s a = Channel s a

instance (MessageType a
         ,Typeable s
         ,p~DecideChannelShouldPass a
         ,ShouldPassValue p) => ToPattern (Channel s a) s a p
  where toPattern c = Pattern c MatchAll (shouldPassValue (undefined :: p))

instance (MessageType a
         ,Typeable s
         ,p~DecideChannelShouldPass a
         ,ShouldPassValue p) => ToPatterns (Channel s a) '[Pattern s a p]
  where toPatterns c = OnePattern $ Pattern c MatchAll (shouldPassValue (undefined :: p))

-- | Decide whether a channel's message type should
-- be passed.
-- () = Keep
-- a  = Pass
type family DecideChannelShouldPass a
  where DecideChannelShouldPass () = Keep
        DecideChannelShouldPass a  = Pass

type MessagePassed a = Pass~DecideChannelShouldPass a
type MessageKept   a = Keep~DecideChannelShouldPass a


-- Declare that Channels pass their messages according to the
-- 'DecideChannelShouldPass' type function.
instance (pass~DecideChannelShouldPass msg
         ,ShouldPassValue pass
         )
      => PassesMatchingMessages (Channel sync msg) pass where
  getPassesMatchingMessages _ = shouldPassValue (undefined :: pass)

