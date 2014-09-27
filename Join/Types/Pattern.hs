{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Join.Types.Pattern
Copyright   : (c) Samuel A. Yallop, 2014
Maintainer  : syallop@gmail.com
Stability   : experimental

This module encodes a representation of Join-Definitions.
-}
module Join.Types.Pattern
    (
    -- * Overview of usage.
    -- | The primary export of the module is the 'Pattern' constraint which
    -- is used to map valid pattern types to type(s) a corresponding
    -- trigger function may have.
    --
    -- I.E.
    --
    -- @ Pattern pat return trigger @
    --
    -- Declares:
    --
    -- - 'pat' as a valid pattern.
    --
    -- - 'trigger' as the type of a function which consumes matches on the
    -- pattern, producing a value typed 'return'.
    --
    -- ** Building patterns
    -- *** Matching a Channel
    -- | The simplest pattern is a single 'Channel s a' type.
    -- This declares a pattern that matches all messages sent on the Channel.
    --
    -- E.G.
    --
    -- @ intChan  :: Channel s Int  ==> trigger :: Int  -> return @
    -- @ charChan :: Channel s Char ==> trigger :: Char -> return @
    --
    -- In general, instantiating 'pat' with a 'Channel s' of type 'a'
    -- determines a trigger function of type 'a -> return', return being
    -- some given return type.
    -- I.E. @ pat ~ Channel s a ==> trigger ~ (a -> return) @
    --
    -- There is one exception, which is when the message type of the
    -- channel is a~'()'. This is because the unit type '()' only has one
    -- value (also named ()) making passing the value unnecessary. The
    -- corresponding trigger type therefore does not pass a value.
    --
    -- I.E. @ pat ~ Channel s () @ ==> @ trigger ~ return @
    -- NOT  @                            trigger ~ (() -> return) @
    --
    -- *** Matching equality on a channel
    -- | For convenience, another type of pattern provided is the equality
    -- pattern, declared infix via '&='. This declares a pattern that
    -- matches messages sent on the channel ONLY when they are equal to
    -- some given value.
    --
    -- I.E. @ boolChan &= False  ==> trigger ~ return @
    --
    -- Note, like the special case of the simple 'Channel' pattern where the
    -- carried message type is '()', a corresponding trigger is NOT passed
    -- a value.
    -- This is because when matching for message equality, by definition
    -- we know what the message value is -It's whatever was
    -- equality matched upon- and so there is no need to pass it.
    --
    -- *** Matching multiple patterns
    -- | Multiple Patterns can be composed with an '&' pattern, declared
    -- infix. An '&' composition declares a pattern that matches only when
    -- both component patterns match.
    --
    -- I.E. '&' is loosely declared like:
    -- '&' = (Pattern l, Pattern r) => l -> r -> l :&: r
    --
    -- E.G.
    --
    -- @ intChan & charChan                 ==> trigger ~ Int -> Char -> return @
    -- @ intChan&=1 & charChan              ==> trigger ~ Char -> return @
    -- @ intChan & charChan&='a' & boolChan ==> trigger ~ Int -> Bool -> return @
    --
    -- Effectively the deduced trigger type of a composition 'c0 & c1 & .. & cn'
    -- is equal to a function ('->') consuming each message type 0..n,
    -- dropping messages typed '()' and messages matched by '&=' patterns.
    --
    --
    -- ** Using patterns
    -- | 'Pattern pat return trigger' requires an 'Apply return trigger'
    -- constraint. This means 'apply' can be used to attempt to apply
    -- a list of 'ByteString' arguments' to a 'pat's 'trigger' functon.
    --
    -- The 'rawPattern' method, available on all instances of 'Pattern'
    -- defines how the runtime system should correctly pass messages into
    -- trigger functions.
    --
    -- By following the semantics of the produced 'PatternDescription'
    -- value, a system can store trigger functions and execute them using
    -- 'apply' without error.
    --
    -- The semantics of the 'PatternDescription' value is given in the
    -- 'RawPattern' documentation and is only relevant to those writing
    -- interpreters.
    -- 
    -- * Details
      Pattern
    , rawPattern, PatternDescription, MatchType(..)
    , (&)
    , (&=)

    , MessageDropped
    , MessagePassed

    , Application(..)
    , apply
    , unsafeApply
    ) where

import Join.Types.Apply
import Join.Types.Channel

import Data.ByteString (ByteString)
import Data.Serialize (Serialize,encode)

{- New pattern data types -}

-- | Pattern type of matching 'Channel' messages when equal to some value.
data ChannelEq a = forall s. Serialize a => ChannelEq (Channel s a) a

-- | Infix 'ChannelEq'.
--
-- Right associative and with greater precedence than '&' means:
--
-- @ c1 & (c2&=1) & c3 @
--
-- Is equivalent to:
--
-- @ c1 & c2&=1 & c3 @
(&=) :: Serialize a => Channel s a -> a -> ChannelEq a
infixr 8 &=
(&=) = ChannelEq

instance Show (ChannelEq a) where show (ChannelEq c a) = show c ++ "&=" ++ show (encode a)


-- | Pattern type of matching on a conjunction of patterns.
data patL :&: patR where
    And :: (RawPattern patL, RawPattern patR)
        => patL -> patR -> patL :&: patR

-- | Prefix type synonym of ':&:'.
type And patL patR = patL :&: patR

-- | Infix 'And'
--
-- Right associative, means:
--
-- @ c1 & (c2 & c3) @
--
-- Is equivalent to:
--
-- @ c1 @ c2 @ c3 @
(&) :: (RawPattern patL, RawPattern patR)
    => patL -> patR -> patL :&: patR
infixr 7 &
(&) = And

instance Show (patL :&: patR) where show (And p q) = show p ++ " & " ++ show q



{- Definition of type passing -}

-- | Via DataKinds, the Kind ('Passes a') of Types used to denote the shape
-- of a patterns corresponding trigger function.
data Passes a
    = PassNothing -- ^ Nothing is passed.
    | Pass a      -- ^ 'a' should be passed.

-- | Closed type function deciding whether a message type 'a' is should be passed 
-- All but () is.
type family MessagePasses (a :: *) :: Passes * where
    MessagePasses () = PassNothing
    MessagePasses a  = Pass a

-- | Closed type function combining 'Passes' kinded types.
-- Effectively combines all 'Pass a' 'Pass b' with arrows to 'Pass (a->b)',
-- where 'PassNothing' acts emptily.
type family ComposePasses (p :: Passes *) (p' :: Passes *) :: Passes * where
    ComposePasses PassNothing p'          = p'
    ComposePasses p           PassNothing = p
    ComposePasses (Pass p)    (Pass p')   = Pass (p -> p')
-- | Infix 'ComposePasses'
type p :->: p' = ComposePasses p p'

-- | Class of 'Passes' kinded types 'pass' which, when given a specific
-- 'return' type determine a 'trigger' type.

-- | Class constraint. Given a 'Passes *' shape 'pass', determine a
-- corresponding trigger type which terminates in a 'return' type.
type family ToTrigger (pass :: Passes *) r :: * where
    ToTrigger PassNothing r = r
    ToTrigger (Pass a)    r = (a -> r)



{- Definition of runtime semantics -}

-- | How a message should be matched on a channel.
data MatchType
    = MatchAny              -- ^ Match any message
    | MatchEqual ByteString -- ^ Match only equal messages
    | MatchSignal           -- ^ Match signals - ()

-- | A 'PatternDescription' describes the runtime semantics of a pattern.
--
-- A 'PatternDescription'=[(ChanId,MatchType)] states to match when
-- a message is waiting on each listed channel, as identified by the
-- 'ChanId'. Each 'Channel' must in turn be matched according to it's
-- 'MatchType'.
--
-- 'MatchType':
--
-- - MatchAny              => No restriction. Any available message
--                            counts as a match.
--
-- - MatchEqual ByteString => Only messages equal to the string should
--                            match.
--
-- - MatchSignal           => Special case of MatchAny when the underlying
--                            message-type has the shape of the unit type ().
type PatternDescription = [(ChanId,MatchType)]

-- | Class of pattern types 'pat' which can produce a description of their
-- runtime semantics by calling 'rawPattern' on a 'pat'.
--
-- All 'RawPattern' instances have a 'Show' instance.
-- See 'PatternDescription' documentation for a defintion of the expected
-- runtime pattern matching semantics.
class Show pat
   => RawPattern pat where
    rawPattern :: pat -> PatternDescription
instance RawPattern (Channel s ())  where rawPattern c               = [(getId c, MatchSignal)]
instance RawPattern (Channel s a)   where rawPattern c               = [(getId c, MatchAny)]
instance RawPattern (ChannelEq a)   where rawPattern (ChannelEq c a) = [(getId c, MatchEqual $ encode a)]
instance RawPattern (patL :&: patR) where rawPattern (And p q)       = rawPattern p ++ rawPattern q

-- | Type function given a pattern type 'pat', determines a 'Passes *' type
-- which describes the types of value expected to be passed to
-- a corresponding trigger function
-- (if anything is to be passed at all).
type family PatternPasses pat :: Passes * where
   PatternPasses (Channel s a)   = MessagePasses a
   PatternPasses (ChannelEq a)   = PassNothing
   PatternPasses (patL :&: patR) = (PatternPasses patL) :->: (PatternPasses patR)
-- | Type function. Given a pattern type 'pat', and a return type 'r',
-- determine the type of a corresponding trigger function
-- (Via 'PatternPasses').
type PatternTrigger pat r = ToTrigger (PatternPasses pat) r



-- | Class of pattern types 'pat' which, when given a specific 'return'
-- type determine a 'trigger' type.
class (RawPattern pat
      ,Apply trigger return
      )
    => Pattern pat return trigger | pat return -> trigger
instance (trigger~PatternTrigger pat return
         ,RawPattern pat
         ,Apply trigger return
         )
      => Pattern pat return trigger


-- | Constraint. 'a' requires that when used as a message type
-- (I.E. 'Chan a') the message value is passed into a corresponding
-- trigger function.
--
-- Useful for constraining an overly polymorphic join definition from all
-- 'a', to 'a' except ().
--
-- E.G. Given the following code, if 'a' is left unconstrained, and
-- completely polymorphic to the call site, it cannot be compiled. Thi
-- do chan :: Chan a <- newChannel
--    chan |> \v -> ...
--
-- 'a' is left completly unconstrained and is polymorphic to the call site.
-- It can't therefore be compiled as when 'a'~() no messages are passed.
-- This may be fixed by constraining 'a' by 'MessagePassed a'.
type MessagePassed a = MessagePasses a ~ Pass a

-- | Counterpart to constraint 'MessagePassed'. Most likely of little use
-- as currently 'MessageDropped a' seems to imply a~() which can probably
-- be otherwise deduced by it's usage.
type MessageDropped a = MessagePasses a ~ PassNothing

