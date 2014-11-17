{-|
Module      : Join.Pattern
Copyright   : (c) Samuel A. Yallop, 2014
Maintainer  : syallop@gmail.com
Stability   : experimental

This module exports a syntax for writing Join Patterns/ Definitions
as encoded by 'Join.Pattern.Rep'.
-}
{-# LANGUAGE DataKinds
            ,TypeFamilies
            ,TypeOperators
  #-}
module Join.Pattern
  ( (&=)
  , (&~)
  , (&)
  , (|>)
  , (|$)
  , module C
  ) where

import Join.Apply
import Join.Channel
import Join.Message
import Join.Pattern.Channel as C
import Join.Pattern.Rep

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

-- | Pattern type of matching messages sent on a 'Channel' ONLY when they satisfy
-- some predicate.
--
-- E.G. @ intChan&~(<10) @
-- Then a trigger is typed: @ trigger :: Int -> return @
-- and may only fire when the sent message is less than 10.
(&~) :: (MessageType a,Typeable s) => Channel s a -> (a -> Bool) -> Pattern s a Pass
infixr 8 &~
c &~ pred = Pattern c (MatchWhen pred) DoPass

-- | Pattern type of matching a conjunction of patterns.
--
-- Declared infix via '&'.
--
-- Composition declares a pattern that matches only when both
-- component patterns match.
--
-- Corresponding trigger types are composed with '->'.
--
-- E.G. If we have the following pattern types, which determine triggers:
--
-- - @ intChan @ => @ trigger :: Int -> return @
--
-- - @ charChan @ => @ trigger :: Char -> return @
--
-- - @ boolEq @ => @ trigger :: return @
--
-- Then:
--
-- - @ intChan & charChan @ => @ trigger :: Int -> Char -> return @
--
-- - @ intChan & boolEq @ => @ trigger :: Int -> return @
--
-- - @ intChan & boolEq & charChan @ => @ trigger :: Int -> Char -> return @
(&) :: (ToPattern t s m p,ToPatterns t' ts)
    => t -> t' -> Patterns ((Pattern s m p) ': ts)
infixr 7 &
p & ps = AndPattern (toPattern p) (toPatterns ps)

-- | Build a definition infix from a patterns type and an associated trigger function.
(|>) :: (ToPatterns pat ts,tr~TriggerType ts r,Apply tr r)
     => pat -> tr -> Definition ts tr r
infixr 6 |>
ps |> tr = Definition (toPatterns ps) (Trigger tr)

-- | Build definitions infix by prepending a single definition type to a definitions type.
(|$) :: (ToDefinition t ts tr r,ToDefinitions t' tss r)
     => t
     -> t'
     -> Definitions ((Definition ts tr r) ': tss) r
infixr 5 |$
d |$ ds = AndDefinition (toDefinition d) (toDefinitions ds)

