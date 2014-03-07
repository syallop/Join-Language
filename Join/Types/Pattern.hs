{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      : Join.Types.Pattern
Copyright   : (c) Samuel A. Yallop, 2014
Maintainer  : syallop@gmail.com
Stability   : experimental

This module encodes a representation of the pattern component of a Join-Definition.
-}
module Join.Types.Pattern
    (
    -- * Defining patterns
    -- | The primary export of the module is 'Pattern' which is a closed
    -- typeclass which pairs pattern types to the type a corresponding
    -- consuming trigger should have.
    --
    -- I.E.
    --
    -- @ Pattern p f @
    --
    -- Declares:
    --
    -- - 'p' is a valid pattern.
    --
    -- - 'f' is the type of a function which consumes matches on the
    --   pattern.
    --
    -- Valid patterns are defined as either:
    --
    -- - A single 'Channel' => Match all messages on Channel.
    --
    -- - A single 'Channel' and a value => Match only messages sent on
    -- Channel which are equal to the value.
    --
    -- - A conjunction of the previous two forms.
    --
    -- Operators '&' and '&=' are provided for building 'Patterns'. E.G.
    -- Given:
    --
    -- @cc :: Channel A Char
    --
    --  ci :: Channel S Int
    --
    --  cb :: Channel s Bool
    -- @
    --
    -- Some valid 'Pattern's are:
    --
    -- @cc
    --
    -- cc & ci
    --
    -- cc & ci&=1
    --
    -- cc&='a' & cb & ci&=1
    -- @
    --
    -- And the corresponding trigger type would be determined as:
    --
    -- @:: Char -> r
    --
    --  :: Char -> Int -> r
    --
    --  :: Char -> Int -> r
    --
    --  :: Char -> Bool -> Int -> r
    -- @
      (&)
    , (&=)
    , Pattern(..)
    , SubPattern(..)

    -- * Applying patterns
    -- | The secondary export of the module is the 'Apply' typeclass, which
    -- provides a type-safe method of applying a function to a list of
    -- ByteString encoded arguments, to result in an arbitrary type.
    --
    -- The intended use of this typeclass is in combination with 'Pattern'.
    -- E.G. given:
    --
    -- @ (Apply f r, Pattern p f) => (p,f) @
    --
    -- Says:
    --
    -- - 'p' is a valid pattern.
    --
    -- - 'f' is a function which accepts each message matched by the
    --   pattern and terminates with some type 'r'.
    --
    -- An interpreter can then 'encode' the messages matched by 'pattern
    -- p', collect them in a list and call them on 'apply f'. This
    -- operation should always result in a 'Result r' type. For convenience
    -- 'unsafeApply' is provided for similar situations when success can be
    -- guaranteed/ failure cannot be recovered.
    , Application(..)
    , Apply(..)
    , unsafeApply
    ) where

import Join.Types.Channel

import Data.ByteString
import Data.Serialize

-- | Require that messages sent on a Channel must match a specific value to
-- trigger a match.
data ChannelEq a = forall s. Serialize a => ChannelEq (Channel s a) a
instance Show (ChannelEq a) where show (ChannelEq c a) = show c ++ "&=" ++ show (encode a)

-- | Class of types 'p' which may be used as a pattern in
-- a Join-Definition.
--
-- The type variable 'f' describes the type a corresponding trigger
-- function must have in order to consume a match and terminate with
-- a result type.
--
class Show p => Pattern p f | p -> f where
    pattern :: p -> [(Int,Maybe ByteString)]

-- | Class of types 'p' which may be used as a smaller component in
-- a Join-Definition in order to build a larger 'Pattern'.
--
-- The type variable 't' gives the type a corresponding trigger function
-- must take in order to properly consume a subpattern match
class Show p => SubPattern p t | p -> t where
    subpattern :: p -> (Int,Maybe ByteString)

-- | Type of conjunctive patterns. Conjoins a SubPattern to a Pattern
-- specifying both must match in order for the whole pattern to match.
data And f where And :: (SubPattern p t, Pattern p' f) => p -> p' -> And (t -> f)
instance Show (And p) where show (And p ps) = show p ++ "&" ++ show ps

-- Only Channel's and ChannelEq's are valid subpatterns.
instance SubPattern (Channel s a) a where subpattern c = (getId c, Nothing)
instance SubPattern (ChannelEq a) a where subpattern (ChannelEq c a) = (getId c, Just $ encode a)

-- Channel's and ChannelEq's are also valid patterns in themselves.
instance Pattern (Channel s a) (a -> r) where pattern c = [subpattern c]
instance Pattern (ChannelEq a) (a -> r) where pattern c = [subpattern c]

-- And conjunctions are valid Patterns.
instance Pattern (And f) f where pattern (And p ps) = subpattern p : pattern ps

-- | Infix combine a smaller SubPattern with a Pattern.
--
-- Right associative, meaning:
--
-- @ c1 & (c2 & c3) @
--
-- May be written as:
--
-- @ c1 & c2 & c3 @
--
-- As all subpatterns are also valid patterns, the operator may be thought
-- of as a way to combine subpatterns.
--
-- Because all subpatterns are also valid patterns, the operator may be
-- thought of as a method for conjoining subpatterns.
(&) :: (SubPattern p t, Pattern p' t') => p -> p' -> And (t -> t')
infixr 7 &
p & ps = And p ps

-- | Infix define a ChannelEq match.
--
-- Right associative and with a greater precedence than '&'.
-- This means:
--
-- @ c1 & (c2&=1) @
--
-- Can be written as:
--
-- @ c1 & c2&=1 @
(&=) :: Serialize a => Channel s a -> a -> ChannelEq a
infixr 8 &=
c &= v = ChannelEq c v

-- | Encapsulates the result of some type of function application that may
-- either succeed or fail in a limited number of ways.
data Application r
    = Result r -- ^ Successful result value.
    | Excess   -- ^ Too many arguments.
    | Shortage -- ^ Too few arguments.
    | Mistyped -- ^ Mistyped argument.

instance Show (Application r) where
    show (Result _) = "Successful application."
    show Excess = "Too many arguments."
    show Shortage = "Too few arguments."
    show Mistyped = "Mistyped arguments."

-- | Class of types 'f' which may be applied to a sequence of ByteString
-- parameters, resulting in a value of type 'r'.
--
-- 'apply' may be used in interpreters to run Join-Definition trigger
-- function on messages that have been deemed to match the corresponding
-- pattern.
class Apply f r where
    apply :: f -> [ByteString] -> Application r

instance Apply r r where
    apply r [] = Result r
    apply _ _  = Excess

instance (Serialize a, Apply b r) => Apply (a -> b) r where
    apply f (m:ms) = case decode m of
        Left  _ -> Mistyped
        Right v -> apply (f v) ms
    apply _ [] = Shortage

-- | Unsafe version of 'Apply's 'apply'.
--
-- Only guaranteed to be safe when:
--
-- - The number of list items is exactly equal to the number of arguments
-- expected by 'f'.
--
-- - Each argument Serializes to the corresponding expected type.
unsafeApply :: Apply f r => f -> [ByteString] -> r
unsafeApply f ms = case apply f ms of
    Result r -> r
    failure  -> error $ show failure

