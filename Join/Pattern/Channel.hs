{-# LANGUAGE ConstraintKinds
            ,DataKinds
            ,FlexibleInstances
            ,FunctionalDependencies
            ,MultiParamTypeClasses
            ,TypeFamilies
            ,UndecidableInstances
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
         ,DecideChannelShouldPass a p
         ,ShouldPassValue p) => Pattern (Channel s a) s a p
  where toPatternRep c = Pattern c MatchAll (shouldPassValue (undefined :: p))

instance (MessageType a
         ,Typeable s
         ,DecideChannelShouldPass a p
         ,ShouldPassValue p) => Patterns (Channel s a) '[PatternRep s a p]
  where toPatternsRep c = OnePattern $ Pattern c MatchAll (shouldPassValue (undefined :: p))

class ShouldPassValue p
  where shouldPassValue :: p -> ShouldPass p
instance ShouldPassValue Keep
  where shouldPassValue _ = DontPass
instance ShouldPassValue Pass
  where shouldPassValue _ = DoPass

class (p~DecideChannelShouldPassF a) => DecideChannelShouldPass a p | a -> p
instance (p~DecideChannelShouldPassF a) => DecideChannelShouldPass a p

type family DecideChannelShouldPassF a
  where DecideChannelShouldPassF () = Keep
        DecideChannelShouldPassF a  = Pass

type MessagePassed a = Pass~DecideChannelShouldPassF a
type MessageKept   a = Keep~DecideChannelShouldPassF a

