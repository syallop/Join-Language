{-# LANGUAGE DataKinds
            ,FlexibleInstances
            ,GADTs
            ,MultiParamTypeClasses
  #-}
module Join.Types.Pattern.Pred
  ( ChannelPred(..)
  , (&~)
  ) where

import Join.Types.Pattern.Rep

import Join.Types.Channel
import Join.Types.Message

-- | Pattern type of matching messages sent on a 'Channel' ONLY when they satisfy
-- some predicate.
--
-- Declared infix via '&~'.
--
-- E.G. @ intChan&~(<10) @
-- Then a trigger is typed: @ trigger :: Int -> return @
-- and may only fire when the sent message is less than 10.
data ChannelPred s a = MessageType a => ChannelPred (Channel s a) (a -> Bool)

-- | Infix 'ChannelPred'
(&~) :: MessageType a => Channel s a -> (a -> Bool) -> ChannelPred s a
infixr 8 &~
(&~) = ChannelPred

instance Pattern (ChannelPred s a) s a Pass
  where toPatternRep (ChannelPred c p) = Pattern c (MatchWhen p) DoPass

instance Patterns (ChannelPred s a) '[PatternRep s a Pass]
  where toPatternsRep (ChannelPred c p) = OnePattern $ Pattern c (MatchWhen p) DoPass

