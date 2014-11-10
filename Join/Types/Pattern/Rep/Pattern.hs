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
Module      : Join.Types.Pattern.Rep.Pattern
Copyright   : (c) Samuel A. Yallop, 2014
Maintainer  : syallop@gmail.com
Stability   : experimental

This module gives a strongly typed representation of join patterns as concrete ADT's.

The 'root' types exported are 'PatternRep' (holding the data of a single pattern)
and 'PatternsRep' (holding data of one or many patterns).
-}
module Join.Types.Pattern.Rep.Pattern
  (-- * Types used within pattern(s)
   MsgPredRep
  ,ShouldPass(..)
  ,Pass
  ,Keep
  ,MatchRep(..)

  -- * Pattern(s)
  ,PatternRep(..)
  ,PatternsRep(..)

  -- * Pattern Syntax extensibility
  ,Pattern(..)
  ,Patterns(..)

  ,foldPatternsRep
  ) where

import Join.Types.Apply
import Join.Types.Channel
import Join.Types.Message

import Data.Typeable

-- | Represent a predicate which may be applied to messages within a
-- pattern.
type MsgPredRep m = m -> Bool

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
data MatchRep m where

  -- ^ Match only when a predicate is satisfied.
  MatchWhen :: MessageType m => MsgPredRep m -> MatchRep m

  -- ^ Match all messages.
  MatchAll  :: MatchRep m

-- | Represent a single item of a pattern.
data PatternRep s m p where
  Pattern :: (MessageType m,Typeable s)
          => Channel (s :: Synchronicity *) m -- ^ Channel matched upon
          -> MatchRep m                       -- ^ Type of matching to perform
          -> ShouldPass p                     -- ^ Whether a successful match should be passed
          -> PatternRep s m p

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
-- - 'ts' : Accumulates a type-list of 'PatternRep s m p's of each composed 'PatternRep'.
--
-- The \'focus\' refers to either:
--
-- - The \'head\' 'PatternRep' of a composite 'AndPattern'.
--
-- - The 'PatternRep' of a single 'OnePattern'.
--
-- Note: There is purposefully no notion of an 'empty pattern'
-- so a 'PatternsRep' contains 1..n but never 0 'PatternRep's.
data PatternsRep (ts :: [*]) where

  -- ^ A single 'PatternRep'
  OnePattern :: PatternRep s m p
             -> PatternsRep '[PatternRep s m p]

  -- ^ A composite pattern where all contained 'PatternRep's
  -- must match for the whole to be considered matched.
  AndPattern :: PatternRep s m p
             -> PatternsRep ts
             -> PatternsRep ((PatternRep s m p) ': ts)

-- | Class of types which can be converted to a single pattern.
class Pattern t s m p | t -> s m p
  where toPatternRep :: t -> PatternRep s m p

-- 'PatternRep's are trivialy themselves.
instance Pattern (PatternRep s m p) s m p
  where toPatternRep t = t

-- | Class of types which can be converted to one or many patterns.
class Patterns t ts | t -> ts
  where toPatternsRep :: t -> PatternsRep ts

foldPatternsRep :: (forall s m p. PatternRep s m p -> acc -> acc)
                -> acc
                -> PatternsRep ts
                -> acc
foldPatternsRep f acc prs = case prs of
  OnePattern pr -> f pr acc
  AndPattern pr prs -> foldPatternsRep f (f pr acc) prs

