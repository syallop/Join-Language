{-# LANGUAGE DataKinds
            ,ExistentialQuantification
            ,FlexibleInstances
            ,FunctionalDependencies
            ,GADTs
            ,KindSignatures
            ,MultiParamTypeClasses
            ,PolyKinds
            ,TypeFamilies
            ,TypeOperators
            ,UndecidableInstances
            ,IncoherentInstances
  #-}
{-|
Module      : Join.Types.Pattern.Rep.Definition
Copyright   : (c) Samuel A. Yallop, 2014
Maintainer  : syallop@gmail.com
Stability   : experimental

This module gives a strongly typed representation of join definitions as concrete ADT's.

The 'root' types exported are 'DefinitionRep' (holding the data of a single definition)
and 'DefinitionsRep' (holding data of one or many definitions).
-}
module Join.Types.Pattern.Rep.Definition
  (-- * Types used in definition(s)
   Trigger(..)
  ,HasTriggerType(..)
  ,TriggerType

   -- * Definition(s)
  ,DefinitionRep(..)
  ,DefinitionsRep(..)

  -- * Definition Syntax extensibility
  ,Definition(..)
  ,Definitions(..)
  ) where

import Join.Types.Apply
import Join.Types.Pattern.Rep.Pattern

-- | A 'Trigger f r' is a function 'f' with an 'Apply f r' constraint.
data Trigger f r = Apply f r => Trigger f
instance Show (Trigger f r) where show _ = "TRIGGER"

-- | Represent a single definition item.
data DefinitionRep ts tr r where
  Definition :: (HasTriggerType ts tr r,Apply tr r)
             => PatternsRep s m p ts -- ^ Pattern(s) to match upon.
             -> Trigger tr r         -- ^ A corresponding trigger function to fire upon match.
             -> DefinitionRep ts tr r

-- | The list of 'PatternRep' type 'ts', corresponds to the trigger type 'tr' (terminating in 'r')
class (tr~TriggerType ts r) => HasTriggerType ts tr r | ts r -> tr
instance (tr~TriggerType ts r) => HasTriggerType ts tr r

-- | Compute the trigger type corresponding to the given list of 'PatternRep' types (terminating in 'r').
type family TriggerType ts r
      where TriggerType '[PatternRep s m Pass]        r = m -> r
            TriggerType '[PatternRep s m Keep]        r = r
            TriggerType ((PatternRep s m Pass) ': ts) r = m -> TriggerType ts r
            TriggerType ((PatternRep s m Keep) ': ts) r = TriggerType ts r

-- | Represent one and many definition's.
--
-- Type variables:
--
-- - 'r'   : The terminating return type of all contained triggers.
--
-- - 'tss' : Accumulates a type-list of 'DefinitionRep ts tr r' from each contained 'DefinitionRep':
--   where:
--
--   - 'ts' is the type-list collected by the 'PatternsRep'
--
--   - 'tr' is the type of the matching trigger function
--
--   - 'r' is the type of the matching trigger functions terminating type.
--
-- Note: There is purposefully no notion of an 'empty definition'
-- so a 'DefinitionsRep' contains 1..n but never 0 'DefinitionRep's
data DefinitionsRep tss r where

  -- ^ A single 'DefinitionRep'
  OneDefinition :: DefinitionRep ts tr r
                -> DefinitionsRep '[DefinitionRep ts tr r] r

  -- ^ A composite definition where all contained 'DefinitionRep's
  -- may have overlapping channels/ patterns and must be treated as such.
  AndDefinition :: DefinitionRep ts tr r
                -> DefinitionsRep tss r
                -> DefinitionsRep ((DefinitionRep ts tr r) ': tss) r

-- | Class of types which can be converted to a single definition.
class Definition t ts tr r | t -> ts tr r
  where toDefinitionRep :: t -> DefinitionRep ts tr r

-- | Class of types which can be converted to one or many definitions.
class Definitions t tss r | t -> tss r
  where toDefinitionsRep :: t -> DefinitionsRep tss r

