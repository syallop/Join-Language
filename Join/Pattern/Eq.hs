{-# LANGUAGE DataKinds
            ,FlexibleInstances
            ,GADTs
            ,MultiParamTypeClasses
  #-}
module Join.Pattern.Eq
  ( ChannelEq(..)
  , (&=)
  ) where

import Join.Pattern.Rep

import Join.Channel
import Join.Message

import Data.Typeable

-- | Pattern type of matching messages sent on a 'Channel' ONLY when they
-- are equal ('==') to some given value.
--
-- Declared infix via '&='.
--
-- ChannelEq patterns do NOT pass matching values into corresponding triggers.
-- This is because when matching for message equality, by definition we know what
-- the message value is -It's whatever was equality matched upon- and so there's no
-- need to pass it.
--
-- E.G. If: @ boolChan&=False @
-- Then: @ trigger :: return @
-- NOT: @ trigger :: Bool -> return @
data ChannelEq s a = (Eq a,MessageType a) => ChannelEq (Channel s a) a

-- | Infix 'ChannelEq'.
(&=) :: (Eq a,MessageType a) => Channel s a -> a -> ChannelEq s a
infixr 8 &=
(&=) = ChannelEq

instance Show (ChannelEq s a)
  where show (ChannelEq c a) = show c ++ "&=" ++ show (encodeMessage a)

instance Typeable s => ToPattern (ChannelEq s a) s a Keep
  where toPattern (ChannelEq c a) = Pattern c (MatchWhen (== a)) DontPass

instance Typeable s => ToPatterns (ChannelEq s a) '[Pattern s a Keep]
  where toPatterns (ChannelEq c a) = OnePattern $ Pattern c (MatchWhen (== a)) DontPass

