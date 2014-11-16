module Join.Pattern.Eq
  ( (&=)
  ) where

import Join.Pattern.Rep

import Join.Channel
import Join.Message

import Data.Typeable

-- | Pattern type of matching messages sent on a 'Channel' ONLY when they
-- are equal ('==') to some given value.
--
-- ChannelEq patterns do NOT pass matching values into corresponding triggers.
-- This is because when matching for message equality, by definition we know what
-- the message value is -It's whatever was equality matched upon- and so there's no
-- need to pass it.
--
-- E.G. If: @ boolChan&=False @
-- Then: @ trigger :: return @
-- NOT: @ trigger :: Bool -> return @
(&=) :: (Eq a,MessageType a,Typeable s) => Channel s a -> a -> Pattern s a Keep
infixr 8 &=
c &= v = Pattern c (MatchWhen (== v)) DontPass

