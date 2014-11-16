module Join.Pattern.Pred
  ( (&~)
  ) where

import Join.Pattern.Rep

import Join.Channel
import Join.Message

import Data.Typeable

-- | Pattern type of matching messages sent on a 'Channel' ONLY when they satisfy
-- some predicate.
--
-- E.G. @ intChan&~(<10) @
-- Then a trigger is typed: @ trigger :: Int -> return @
-- and may only fire when the sent message is less than 10.
(&~) :: (MessageType a,Typeable s) => Channel s a -> (a -> Bool) -> Pattern s a Pass
infixr 8 &~
c &~ pred = Pattern c (MatchWhen pred) DoPass

