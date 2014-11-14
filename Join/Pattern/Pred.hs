{-# LANGUAGE DataKinds
            ,FlexibleInstances
            ,GADTs
            ,MultiParamTypeClasses
  #-}
module Join.Pattern.Pred
  ( ChannelPred(..)
  , (&~)
  ) where

import Join.Pattern.Rep

import Join.Channel
import Join.Message

import Data.Typeable

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

instance Typeable s => ToPattern (ChannelPred s a) s a Pass
  where toPattern (ChannelPred c p) = Pattern c (MatchWhen p) DoPass

instance Typeable s => ToPatterns (ChannelPred s a) '[Pattern s a Pass]
  where toPatterns (ChannelPred c p) = OnePattern $ Pattern c (MatchWhen p) DoPass

