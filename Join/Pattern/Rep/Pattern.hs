{-# LANGUAGE DataKinds
            ,ExistentialQuantification
            ,FlexibleInstances
            ,FunctionalDependencies
            ,GADTs
            ,KindSignatures
            ,MultiParamTypeClasses
            ,PolyKinds
            ,RankNTypes
            ,ScopedTypeVariables
            ,TypeFamilies
            ,TypeOperators
            ,UndecidableInstances
            ,IncoherentInstances
  #-}
{-|
Module      : Join.Pattern.Rep.Pattern
Copyright   : (c) Samuel A. Yallop, 2014
Maintainer  : syallop@gmail.com
Stability   : experimental

This module gives a strongly typed representation of join patterns as concrete ADT's.

The 'root' types exported are 'Pattern' (holding the data of a single pattern)
and 'Patterns' (holding data of one or many patterns).
-}
module Join.Pattern.Rep.Pattern
  (-- * Types used within pattern(s)
   MsgPred
  ,ShouldPass(..)
  ,Pass
  ,Keep
  ,Match(..)

  -- * Pattern(s)
  ,Pattern(..)
  ,Patterns(..)

  -- * Pattern Syntax extensibility
  ,ToPattern(toPattern)
  ,ToPatterns(toPatterns)

  ,foldPatterns
  ) where

import Join.Apply
import Join.Channel
import Join.Message

import Data.Typeable

-- | Represent a predicate which may be applied to messages within a
-- pattern.
type MsgPred m = m -> Bool

-- | Declare whether some matching message should be passed into a corresponding
-- trigger function or not.
--
-- Two type constructors 'DoPass' and 'DontPass' are indexed by the types 'Pass' and 'Keep'
-- respectively, I.E.:
--
-- value{DoPass}   <=> type{Pass}
-- value{DontPass} <=> type{Keep}
data ShouldPass p where

  -- ^ Matching message should be passed.
  -- value DoPass <=> type Pass
  DoPass   :: ShouldPass Pass

  -- ^ Matching message should NOT be passed.
  -- value DontPass <=> type Keep
  DontPass :: ShouldPass Keep

data Pass -- ^ Denote a message should be passed, at the type level.
data Keep -- ^ Denote a message should NOT be passed, at the type level.

-- | Represent an optional predicate which might be applied to messages within a pattern.
--
-- Theoretically, the no-predicate case could be simulated by a (const True) predicate
-- ,practically having a special case allows significant runtime speedups.
data Match m where

  -- ^ Match only when a predicate is satisfied.
  MatchWhen :: MessageType m => MsgPred m -> Match m

  -- ^ Match all messages.
  MatchAll  :: Match m

-- | Represent a single item of a pattern.
data Pattern s m p where
  Pattern :: (MessageType m,Typeable s)
          => Channel (s :: Synchronicity *) m -- ^ Channel matched upon
          -> Match m                          -- ^ Type of matching to perform
          -> ShouldPass p                     -- ^ Whether a successful match should be passed
          -> Pattern s m p

-- | Represent one and many pattern's.
--
-- Type variables:
--
-- - 's' : Synchronicity type of focus
--
-- - 'm' : Message type of focus
--
-- - 'p' : shouldpass type (Pass/Keep) of focus.
--
-- - 'ts' : Accumulates a type-list of 'Pattern s m p's of each composed 'Pattern'.
--
-- The \'focus\' refers to either:
--
-- - The \'head\' 'Pattern' of a composite 'AndPattern'.
--
-- - The 'Pattern' of a single 'OnePattern'.
--
-- Note: There is purposefully no notion of an 'empty pattern'
-- so a 'Patterns' contains 1..n but never 0 'Pattern's.
data Patterns (ts :: [*]) where

  -- ^ A single 'Pattern'
  OnePattern :: Pattern s m p
             -> Patterns '[Pattern s m p]

  -- ^ A composite pattern where all contained 'Pattern's
  -- must match for the whole to be considered matched.
  AndPattern :: Pattern s m p
             -> Patterns ts
             -> Patterns ((Pattern s m p) ': ts)

-- | Class of types which can be converted to a single pattern.
class ToPattern t s m p | t -> s m p
  where toPattern :: t -> Pattern s m p

-- 'Pattern's are trivialy themselves.
instance ToPattern (Pattern s m p) s m p
  where toPattern t = t

-- | Class of types which can be converted to one or many patterns.
class ToPatterns t ts | t -> ts
  where toPatterns :: t -> Patterns ts

foldPatterns :: (forall s m p. Pattern s m p -> acc -> acc)
             -> acc
             -> Patterns ts
             -> acc
foldPatterns f acc prs = case prs of
  OnePattern pr -> f pr acc
  AndPattern pr prs -> foldPatterns f (f pr acc) prs

