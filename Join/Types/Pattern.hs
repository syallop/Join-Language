{-# LANGUAGE ConstraintKinds
            ,DataKinds
            ,ExistentialQuantification
            ,FlexibleInstances
            ,FunctionalDependencies
            ,GADTs
            ,IncoherentInstances
            ,MultiParamTypeClasses
            ,TypeFamilies
            ,TypeOperators
            ,UndecidableInstances
 #-}

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
    -- | The primary export of the module is the 'JoinDefinition'
    -- constraint which is used to declare valid JoinDefinitions where each
    -- contained clause of pattern types is paired with an appropriately
    -- typed trigger function.
    --
    -- I.E.
    --
    -- @ JoinDefinition jdef return @
    --
    -- Declares:
    --
    -- - 'jdef' as the type of a valid join definition
    --
    -- - 'return' as the terminating return type of all contained trigger
    --   functions.
    --
    -- ** Single Clauses
    -- | A single join definition clause is defined by the infix operator
    -- '|>':
    --
    -- I.E.:
    --
    -- @ pattern |> trigger @
    --
    -- Declares:
    --
    -- - 'pattern' to be a valid pattern value.
    --
    -- - 'trigger' to be a trigger function, correctly typed to accept
    --    messages from the matching 'pattern'.
    --
    -- *** Matching a Channel
    -- | The simplest pattern is a single 'Channel s a' type.
    -- This declares a pattern that matches all messages sent on the
    -- Channel.
    --
    -- E.G.
    --
    -- @ (intChan  :: Channel s Int)  |> (trigger :: Int  -> return) @
    -- @ (charChan :: Channel s Char) |> (trigger :: Char -> return) @
    --
    -- In general a 'Channel s a' will determine a corresponding trigger
    -- typed 'a -> return'
    --
    -- There is one exception, which is when the message type of the
    -- channel is '()'. Because the unit type '()' only has one value (also
    -- named '()') explicitly passing the value to the trigger is
    -- unnecessary. The corresponding trigger type therefore does not
    -- accept a value.
    --
    -- I.E. @ (signalChannel :: Channel s ()) |> (trigger :: return)
    -- NOT  @ (signalChannel :: Channel s ()) |> (trigger :: () -> return)
    --
    -- *** Matching equality on a channel
    -- | For convenience, another type of pattern provided is the equality
    -- pattern, declared infix via '&='. This declares a pattern that
    -- matches messages sent on the channel ONLY when they are equal to
    -- some given value.
    --
    -- Note that like the special case of the 'Channel s ()' pattern,
    -- a corresponding trigger is NOT passed a value.
    -- This is because when matching for message equality, by definition we
    -- know what the message value is -It's whatever was equality matched
    -- upon- and so there's no need to pass it.
    --
    -- I.E. @ boolChan&=False |> (trigger :: return) @
    -- NOT  @ boolChan&=False |> (trigger :: Bool -> return) @
    --
    --
    -- *** Matching multiple patterns in a clause
    -- | Multiple patterns can be composed with an '&' pattern, declared
    -- infix. An '&' composition declares a pattern that matches only when
    -- both component patterns match.
    --
    -- E.G.
    --
    -- @ intChan & charChan |> (trigger :: Int -> Char -> return) @
    -- @ intChan&=1 & charChan |> (trigger :: Char -> return) @
    -- @ intChan & charChan&='a' & boolChan |> (trigger :: Int -> Bool -> return) @
    --
    -- Effectively the deduced trigger type of a composition 'c0 & c1 & .. & cn'
    -- is equal to a function ('->') consuming each message type 0..n,
    -- dropping messages typed '()' and messages matched by '&=' patterns.
    --
    -- ** Multiple clauses
    -- | Many clauses with overlapping patterns can be defined
    -- disjunctively using the infix operator '|$'.
    --
    -- E.G.
    -- @   (intChan & charChan          |> (trigger1 :: Int -> Char -> return))
    --  |$ (intChan&=1                  |> (trigger2 :: return))
    --  |$ (charChan&=1 & charChan&='a' |> (trigger3 :: return))
    -- @
    -- 
    --
    -- ** Interpreting Join Definition
    -- | The 'describe' method, available on all instances of
    -- 'JoinDefinition' defines how the runtime system should correctly
    -- pass messages to trigger functions.
    --
    -- Each clause requires an 'Apply' constraint, meaning 'apply' can be
    -- used to attempt to apply a list of 'ByteString' arguments to
    -- a patterns trigger function.
    --
    -- By following the semantics of the 'PatternDescription' values produced by
    -- 'describe', an interpreter can store trigger functions and execute them
    -- using 'apply' without error.
    --
    -- The semantics of the 'PatternDescription' value is given in the
    -- 'RawPattern' documentation and is only relevant to writing
    -- interpreters.
    --
    -- * Details
      JoinDefinition, describe
    , TriggerF(..)
    , (|>)
    , (|$)

    , Pattern
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
import Join.Types.Message

{- New pattern data types -}

-- | Pattern type of matching 'Channel' messages when equal to some value.
data ChannelEq a = forall s. (Eq a,MessageType a) => ChannelEq (Channel s a) a

-- | Infix 'ChannelEq'.
--
-- Right associative and with greater precedence than '&' means:
--
-- @ c1 & (c2&=1) & c3 @
--
-- Is equivalent to:
--
-- @ c1 & c2&=1 & c3 @
(&=) :: (Eq a,MessageType a) => Channel s a -> a -> ChannelEq a
infixr 8 &=
(&=) = ChannelEq

instance Show (ChannelEq a) where show (ChannelEq c a) = show c ++ "&=" ++ show (encodeMessage a)

-- | Pattern type of matching on a conjunction of patterns.
data patL :&: patR where
    And :: (RawPattern patL, RawPattern patR)
        => patL -> patR -> patL :&: patR

-- | Prefix type synonym of ':&:'.
type And patL patR = patL :&: patR
infixr :&:

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



{- Definition of runtime semantics -}

-- | How messages should be matched on a channel.
data MatchType where

  -- ^ Match messages which satisfy a predicate, 'ShouldPass' declaring
  -- whether a matching message is passed into trigger functions or not.
  MatchWhen :: MessageType m => (m -> Bool) -> ShouldPass -> MatchType

  -- ^ Match any message with 'ShouldPass' declaring whether a matching message
  -- is passed into trigger functions or not.
  MatchAll  :: ShouldPass -> MatchType

type ShouldPass = Bool
pass = True
keep = False

-- | A 'PatternDescription' describes the runtime semantics of a pattern.
--
-- A 'PatternDescription'=[(ChanId,MatchType)] states to match when
-- a message is waiting on each listed channel, as identified by the
-- 'ChanId'. Each 'Channel' must in turn be matched according to it's
-- 'MatchType'.
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
instance RawPattern (Channel s ())  where rawPattern c               = [(getId c, MatchAll keep)]
instance RawPattern (Channel s a)   where rawPattern c               = [(getId c, MatchAll pass)]
instance RawPattern (ChannelEq a)   where rawPattern (ChannelEq c a) = [(getId c, MatchWhen (== a) keep)]
instance RawPattern (patL :&: patR) where rawPattern (And p q)       = rawPattern p ++ rawPattern q



{- Definition of type passing -}

-- | Type function. Given a pattern type 'pat', and a return type 'r',
-- determine the type of a corresponding trigger function.
type family PatternTrigger pat r :: * where
    PatternTrigger (Channel s a)   r = ChannelTrigger a (ShouldPassMessage a) r
    PatternTrigger (ChannelEq a)   r = r
    PatternTrigger (patL :&: patR) r = PatternTrigger patL (PatternTrigger patR r)

-- | Type function. Given a 'Channel' message type 'a', and a return type
-- 'r', use a 'shouldPass' flag to determine the corresponding trigger
-- type.
-- True => Message type 'a' is passed => 'a -> r'
-- False => Message type 'a' is NOT passed => 'r'
type family ChannelTrigger a (shouldPass::Bool) r where
    ChannelTrigger a True  r = a -> r
    ChannelTrigger a False r = r

-- | Type function. Decide whether a given message type 'a' should be
-- passed into trigger function's.
type family ShouldPassMessage a :: Bool where
    ShouldPassMessage () = False
    ShouldPassMessage a  = True

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

-- | A trigger function with an 'Apply' instance returning a value typed
-- 'return'
data TriggerF return = forall trigger. Apply trigger return => TriggerF trigger

-- | Class of types which can be used as Join Definitions, with all trigger
-- functions terminating with a 'return' type.
class JoinDefinition jdef return | jdef -> return where
    describe :: jdef -> [(PatternDescription,TriggerF return)]


-- | A single 'PatternClause' is a 'Pattern' type and an associated trigger
-- function.
--
-- E.G. PatternClause (c1 & c2) (\a b -> return)
data PatternClause return where
    PatternClause :: Pattern pat return trigger => pat -> trigger -> PatternClause return

-- | Infix 'PatternClause'
--
-- E.G. c1 & c2 |> (\a b -> return)
(|>) :: Pattern pat return trigger => pat -> trigger -> PatternClause return
infixr 6 |>
(|>) = PatternClause

instance JoinDefinition (PatternClause return) return where
    describe (PatternClause pat trigger) = [(rawPattern pat,TriggerF trigger)]

-- | Disjunctive alternative of 'PatternClause'.
--
-- E.G. OrPatternClause (c1 & c2 |> \a b -> return)
--                      (c2 & c3 |> \b c -> return)
data OrPatternClause return where
    OrPatternClause :: JoinDefinition jdef return
                    => PatternClause return
                    -> jdef
                    -> OrPatternClause return

-- | Infix 'OrPatternClause'
--
-- E.G.   (c1 & c2 |> \a b -> return)
--     |$ (c2 & c3 |> \b c -> return)
infixr 5 |$
(|$) :: JoinDefinition jdef return => PatternClause return -> jdef -> OrPatternClause return
(|$) = OrPatternClause

instance JoinDefinition (OrPatternClause return) return where
    describe (OrPatternClause pc pcs) = describe pc ++ describe pcs


-- | Constraint. 'a' requires that when used as a message type
-- (I.E. 'Chan a') the message value will be passed into a corresponding
-- trigger function.
--
-- Useful for constraining an overly polymorphic join definition from all
-- 'a', to 'a' except ().
--
-- E.G. Given the following code, if 'a' is left unconstrained, and
-- completely polymorphic to the call site, it cannot be compiled.
-- do chan :: Chan a <- newChannel
--    chan |> \v -> ...
--
-- 'a' is left completely unconstrained and is polymorphic to the call site.
-- It can't therefore be compiled as when 'a'~() no messages are passed.
-- This may be fixed by constraining 'a' by 'MessagePassed a'.
type MessagePassed a = True~ShouldPassMessage a

-- | Counterpart to constraint 'MessagePassed'. Most likely of little use
-- as currently 'MessageDropped a' seems to imply a~() which can probably
-- be otherwise deduced by it's usage.
type MessageDropped a = False~ShouldPassMessage a

