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
Module      : Join.Pattern.Rep.Definition
Copyright   : (c) Samuel A. Yallop, 2014
Maintainer  : syallop@gmail.com
Stability   : experimental

This module gives a strongly typed representation of join definitions as concrete ADT's.

The 'root' types exported are 'Definition' (holding the data of a single definition)
and 'Definitions' (holding data of one or many definitions).
-}
module Join.Pattern.Rep.Definition
  (-- * Types used in definition(s)
   Trigger(..)
  ,HasTriggerType(..)
  ,TriggerType

   -- * Definition(s)
  ,Definition(..)
  ,Definitions(..)

  -- * Definition Syntax extensibility
  ,ToDefinition(toDefinition)
  ,ToDefinitions(toDefinitions)

  ,foldDefinitions

  ,uniqueIds

  ,appendDefinitions
  ) where

import Join.Apply
import Join.Channel
import Join.Pattern.Rep.List
import Join.Pattern.Rep.Pattern

import qualified Data.Set as Set

-- | A 'Trigger f r' is a function 'f' with an 'Apply f r' constraint.
data Trigger f r = Apply f r => Trigger f
instance Show (Trigger f r) where show _ = "TRIGGER"

-- | Represent a single definition item.
data Definition ts tr r where
  Definition :: (HasTriggerType ts tr r,Apply tr r)
             => Patterns ts       -- ^ Pattern(s) to match upon.
             -> Trigger tr r         -- ^ A corresponding trigger function to fire upon match.
             -> Definition ts tr r

-- | The list of 'Pattern' type 'ts', corresponds to the trigger type 'tr' (terminating in 'r')
class (tr~TriggerType ts r) => HasTriggerType ts tr r | ts r -> tr
instance (tr~TriggerType ts r) => HasTriggerType ts tr r

-- | Compute the trigger type corresponding to the given list of 'Pattern' types (terminating in 'r').
type family TriggerType ts r
      where TriggerType '[Pattern s m Pass]        r = m -> r
            TriggerType '[Pattern s m Keep]        r = r
            TriggerType ((Pattern s m Pass) ': ts) r = m -> TriggerType ts r
            TriggerType ((Pattern s m Keep) ': ts) r = TriggerType ts r

-- | Represent one and many definition's.
--
-- Type variables:
--
-- - 'r'   : The terminating return type of all contained triggers.
--
-- - 'tss' : Accumulates a type-list of 'Definition ts tr r' from each contained 'Definition':
--   where:
--
--   - 'ts' is the type-list collected by the 'Patterns'
--
--   - 'tr' is the type of the matching trigger function
--
--   - 'r' is the type of the matching trigger functions terminating type.
--
-- Note: There is purposefully no notion of an 'empty definition'
-- so a 'Definitions' contains 1..n but never 0 'Definition's
data Definitions tss r where

  -- ^ A single 'Definition'
  OneDefinition :: Definition ts tr r
                -> Definitions '[Definition ts tr r] r

  -- ^ A composite definition where all contained 'Definition's
  -- may have overlapping channels/ patterns and must be treated as such.
  AndDefinition :: Definition ts tr r
                -> Definitions tss r
                -> Definitions ((Definition ts tr r) ': tss) r

-- | Class of types which can be converted to a single definition.
class ToDefinition t ts tr r | t -> ts tr r
  where toDefinition :: t -> Definition ts tr r

-- Definition is itself
instance ToDefinition (Definition ts tr r) ts tr r
  where toDefinition = id

-- | Class of types which can be converted to one or many definitions.
class ToDefinitions t tss r | t -> tss r
  where toDefinitions :: t -> Definitions tss r

-- Definitions is itself.
instance ToDefinitions (Definitions tss r) tss r
  where toDefinitions = id

-- Definition is One Definitions.
instance ToDefinitions (Definition ts tr r) '[Definition ts tr r] r
  where toDefinitions dr = OneDefinition dr

foldDefinitions :: (forall ts tr r. Definition ts tr r -> acc -> acc)
                -> acc
                -> Definitions tss r
                -> acc
foldDefinitions f acc (OneDefinition dr)     = f dr acc
foldDefinitions f acc (AndDefinition dr drs) = foldDefinitions f (f dr acc) drs

appendDefinitions :: Definitions tss r -> Definitions tss' r -> Definitions (tss :++ tss') r
appendDefinitions (OneDefinition dr)     drs' = AndDefinition dr drs'
appendDefinitions (AndDefinition dr drs) drs' = AndDefinition dr (appendDefinitions drs drs')

uniqueIds :: Definitions tss r -> Set.Set ChanId
uniqueIds dr = foldDefinitions uniqueIds' Set.empty dr
  where
    uniqueIds' :: Definition ts tr r -> Set.Set ChanId -> Set.Set ChanId
    uniqueIds' (Definition pr _) acc = foldPatterns uniqueIds'' acc pr

    uniqueIds'' :: Pattern s m p -> Set.Set ChanId -> Set.Set ChanId
    uniqueIds'' (Pattern c _ _) acc = Set.insert (getId c) acc

