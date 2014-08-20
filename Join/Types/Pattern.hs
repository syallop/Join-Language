{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    -- is used to map valid pattern types to the type a corresponding
    -- trigger function should have.
    --
    -- I.E.
    --
    -- @ Pattern p r f @
    --
    -- Declares:
    --
    -- - 'p' as a valid pattern
    --
    -- - 'f' is the type of a function which consumes matches on the
    -- pattern, producing a 'r'.

    -- ** Building patterns
    -- *** Matching a Channel
    -- | The simplest pattern that can be instantiated is a single 'Channel' type.
    -- This declares a pattern that matches all messages sent on the Channel.
    --
    -- E.G.
    --
    -- @ intChan  @ ==> @ f :: Int -> r @
    -- @ charChan @ ==> @ f :: Char -> r @
    --
    -- In general, instantiating 'p' with a Channel of type 'a' produces
    -- a trigger function of type 'a -> r' where r is some given return
    -- value.
    -- I.E. @ p~(Channel s a) @ ==> @ f~(a -> r) @

    -- *** Matching equality on a Channel
    -- | For convenience, another type of pattern is the equality pattern, declared infix via
    -- '&='. This declares a pattern that matches only messages sent on the
    -- Channel which are equal to the given value.
    --
    -- E.G.
    --
    -- @ boolChan &= False @ ==> @ f :: r @
    --
    -- In general, instantiating 'p' with a Channel-Equality of type 'a',
    -- produces a function of type 'r'.
    -- NOTE: The trigger function does not take an 'a'. This is because the
    -- value is fixed by the equality.
    -- I.E. @ p~(ChannelEq (Channel s a) a) @ ==> @ f :: r @

    -- *** Matching multiple patterns
    -- | Multiple Patterns can be conjoined with an \'and\' pattern,
    -- declared infix via '&'. This declares a pattern that matches only
    -- when both individual patterns match.
    --
    -- E.G.
    --
    -- @ intChan & charChan    @ ==> @ f :: Int -> Char -> r @
    -- @ intChan&=1 & charChan @ ==> @ f :: Char -> r @
    -- @ intChan & charChan & boolChan&=True @ ==> @ f :: Int -> Char -> r @
    --
    -- In general, instantiating 'p' with a conjunction of patterns 'p' and
    -- 'q' produces a function typed to accept 'p' followed by 'q'.
    -- NOTE: Argument types are composed left-to-right, and the type of
    -- equality-matches do not appear in the trigger.

    -- ** Using patterns
    -- | The 'Pattern p r f' constraint implies a
    -- @ Apply r f @ relationship.
    -- This means 'apply' can be used to attempt to apply a list of
    -- 'ByteString' arguments to a patterns trigger function.
    --
    -- Knowledge of how to correctly apply arguments to a pattern trigger
    -- can be attained by the 'rawPattern' function.
    --
    -- By following the semantics of the produced description value, a system can
    -- store trigger functions and execute them using 'apply' without error.
    --
    -- The semantics of the description value is given in the
    -- 'RawPattern' documentation.

    -- * Details
      Pattern
    , rawPattern
    , (&)
    , (&=)

    , Application(..)
    , apply
    , unsafeApply
    ) where

import Join.Types.Apply
import Join.Types.Channel

import Data.ByteString (ByteString)
import Data.Serialize (Serialize,encode)

{- Classes -}

-- | Class of types with an associated raw pattern declaring what is
-- required for a match.
--
-- A raw pattern '[(Int,Maybe ByteString)]' matches when there is a message
-- on each channel identified by the Int, which must match the 'ByteString'
-- if given.
class RawPattern p where rawPattern :: p -> [(Int,Maybe ByteString)]

type Pattern p r f = (Pattern' p r f, Apply f r)

-- | Class of types 'p' which may be used as a pattern.
--
-- 'f'' gives the type a corresponding trigger function must have to
-- consume the pattern and terminate with 'f'.
class (RawPattern p, Show p) =>
      Pattern' p f f' | p f -> f' where

{- Data structures -}

-- | Pattern type of matching 'Channel' messages when equal to some
-- value.
data ChannelEq a = forall s. Serialize a => ChannelEq (Channel s a) a

-- | Infix 'ChannelEq'.
--
-- Right associative and with greater precedence than '&' means:
--
-- @ c1 & (c2&=1) & c3 @
--
-- Can be re-written as:
--
-- @ c1 & c2&=1 & c3 @
(&=) :: Serialize a => Channel s a -> a -> ChannelEq a
infixr 8 &=
(&=) = ChannelEq

-- | Pattern type of matching on a conjunction of two 'Patterns'.
data And f f' where And :: (Pattern' q f f', Pattern' p f' f'') => p -> q -> And f f''

-- | Infix 'And'
--
-- Right associative, means:
--
-- @ c1 & (c2 & c3) @
--
-- Can be re-written as:
--
-- @ c1 & c2 & c3 @
--
-- As all 'SubPattern's are also valid 'Pattern's, the operator may be
-- thought of as a way to combine 'SubPattern's.
(&) :: (Pattern' q f f', Pattern' p f' f'') => p -> q -> And f f''
infixr 7 &
(&) = And

{- Instances -}

instance RawPattern (Channel s a) where rawPattern c               = [(getId c, Nothing)]
instance RawPattern (ChannelEq a) where rawPattern (ChannelEq c a) = [(getId c, Just $ encode a)]
instance RawPattern (And p q)     where rawPattern (And p q)       = rawPattern p ++ rawPattern q

instance Show (ChannelEq a) where show (ChannelEq c a) = show c ++ "&=" ++ show (encode a)
instance Show (And p q) where show (And p q) = show p ++ " & " ++ show q

instance Combine a f f' => Pattern' (Channel s a) f  f' where
instance                   Pattern' (ChannelEq a) f  f  where
instance f0~f1          => Pattern' (And f0 f')   f1 f' where

-- Class of types 'a' which can be combined with a type 'f', resulting in
-- a conjunctive trigger type 'f\'''.
--
-- The below machinery is needed to implement the relation:
-- Combine () f ~> f
-- Combine a  f ~> (a -> f)
class Combine a f f' where
instance (DecideKeep a shouldKeep, Combine' a shouldKeep f f')
      => Combine a f f' where

-- Class of types 'a', when given a flag indicating whether it should be
-- kept, can be combined with a type 'f', resulting in a conjunctive
-- trigger type 'f\''.
class Combine' a shouldKeep f f' | a shouldKeep f -> f' where
instance Combine' () Drop f f where
instance Serialize a
      => Combine' a Keep f (a -> f) where

data Drop -- Type is dropped in composition.
data Keep -- Type is kept in composition.

-- Class of type's 'a' which we can decide whether to keep when combining
-- pattern trigger function types.
class DecideKeep a shouldKeep | a -> shouldKeep where
instance shouldKeep~Keep => DecideKeep a shouldKeep where
instance DecideKeep () Drop where

