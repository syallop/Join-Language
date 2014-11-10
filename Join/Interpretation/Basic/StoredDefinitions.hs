{-# LANGUAGE DataKinds
            ,ExistentialQuantification
            ,FlexibleContexts
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
Module      : Join.Interpretation.Basic.StoredDefinitions
Copyright   : (c) Samuel A. Yallop, 2014
Maintainer  : syallop@gmail.com
Stability   : experimental

This module is an analogue of the structures exported by Join.Types.Pattern.Rep
modified to hold slightly different information for the purpose of efficient runtime execution.

The types and functions are named by the structure they adapt, prefixed by 'Stored'.

Note: Due to the similar structure and near code duplication, it is possible that
Join.Types.Pattern.Rep.* will later be abstracted to cover this module.
-}

module Join.Interpretation.Basic.StoredDefinitions
  (StoredPatternRep(..)
  ,StoredPatternsRep(..)
  ,StoredDefinitionRep(..)
  ,StoredDefinitionsRep(..)

  ,foldStoredPatterns
  ,foldStoredDefinitionsRep

  ,storeDefinitionsWith
  ,mapStoredDefinitions
  ) where

import Data.Typeable

import Join
import Join.Types.Apply
import Join.Types.Channel
import Join.Types.Message
import Join.Types.Pattern.Rep

import Join.Interpretation.Basic.MessageBox

-- | Stored analogy to 'PatternRep' with 'MatchRep' replaced
-- by a possible 'BoxIx' referencing the subbox where messages which
-- would match a 'MatchWhen pred' predicate are kept.
data StoredPatternRep s m p where
  StoredPattern :: (MessageType m,Typeable s)
                => Channel (s :: Synchronicity *) m
                -> Maybe BoxIx
                -> ShouldPass p
                -> StoredPatternRep s m p

-- | Stored analogy to 'PatternsRep'.
data StoredPatternsRep (ts :: [*]) where
  OneStoredPattern :: StoredPatternRep s m p
                   -> StoredPatternsRep '[PatternRep s m p]

  AndStoredPattern :: StoredPatternRep s m p
                   -> StoredPatternsRep ts
                   -> StoredPatternsRep ((PatternRep s m p) ': ts)

-- | Stored analogy to 'DefinitionRep', with differences:
-- - StoredPatternsRep instead of PatternRep
-- - Concrete return type Inert.
-- - additional 'refine' type allowing definitions to be tagged with a value.
data StoredDefinitionRep ts tr refine where
  StoredDefinition :: (HasTriggerType ts tr Inert,Apply tr Inert)
                   => StoredPatternsRep ts
                   -> Trigger tr Inert
                   -> refine
                   -> StoredDefinitionRep ts tr refine

-- | Stored analogy to 'DefinitionsRep'.
data StoredDefinitionsRep tss refine where
  OneStoredDefinition :: StoredDefinitionRep ts tr refine
                      -> StoredDefinitionsRep '[DefinitionRep ts tr Inert] refine

  AndStoredDefinition :: StoredDefinitionRep ts tr refine
                      -> StoredDefinitionsRep tss refine
                      -> StoredDefinitionsRep ((DefinitionRep ts tr Inert) ': tss) refine

-- | Fold a function over the StoredPatternRep contained within a StoredPatternsRep.
foldStoredPatterns :: (forall m s p. (MessageType m,Typeable s) => Channel (s::Synchronicity *) m -> Maybe BoxIx -> ShouldPass p -> acc -> acc)
                   -> acc
                   -> StoredPatternsRep ts
                   -> acc
foldStoredPatterns f acc (OneStoredPattern (StoredPattern c mb sp)) = f c mb sp acc
foldStoredPatterns f acc (AndStoredPattern (StoredPattern c mb sp) sps) = foldStoredPatterns f (f c mb sp acc) sps

-- | Fold a function over the StoredDefinitionRep contained within a StoredDefinitionsRep.
foldStoredDefinitionsRep :: (forall ts tr. StoredDefinitionRep ts tr refine -> acc -> acc)
                         -> acc
                         -> StoredDefinitionsRep tss refine
                         -> acc
foldStoredDefinitionsRep f acc (OneStoredDefinition sdr)      = f sdr acc
foldStoredDefinitionsRep f acc (AndStoredDefinition sdr sdrs) = foldStoredDefinitionsRep f (f sdr acc) sdrs




-- | Store all contained PatternRep by using a function to map 'MatchWhen pred's and an accumulator
-- to a BoxIx and an updated accumulator.
storeDefinitionsWith :: (forall s m. (MessageType m,Typeable s) => Channel (s :: Synchronicity *) m -> MatchRep m -> acc -> (acc,Maybe BoxIx))
                     -> acc
                     -> DefinitionsRep tss Inert
                     -> (acc,StoredDefinitionsRep tss ())
storeDefinitionsWith f acc (OneDefinition dr) =
  let (acc',dr') = storeDefinitionWith f acc dr
     in (acc',OneStoredDefinition dr')
storeDefinitionsWith f acc (AndDefinition dr drs) =
  let (acc',dr') = storeDefinitionWith f acc dr
      (acc'',drs') = storeDefinitionsWith f acc' drs
     in (acc'',AndStoredDefinition dr' drs')

storeDefinitionWith :: (forall s m. (MessageType m,Typeable s) => Channel (s :: Synchronicity *) m -> MatchRep m -> acc -> (acc,Maybe BoxIx))
                    -> acc
                    -> DefinitionRep ts tr Inert
                    -> (acc,StoredDefinitionRep ts tr ())
storeDefinitionWith f acc (Definition pr tr) = let (acc',pr') = storePatternsWith f acc pr
                                                  in (acc',StoredDefinition pr' tr ())
storePatternsWith :: (forall s m. (MessageType m,Typeable s) => Channel (s :: Synchronicity *) m -> MatchRep m -> acc -> (acc,Maybe BoxIx))
                  -> acc
                  -> PatternsRep ts
                  -> (acc,StoredPatternsRep ts)
storePatternsWith f acc (OnePattern spr) =
  let (acc',spr') = storePatternWith f acc spr
     in (acc',OneStoredPattern spr')
storePatternsWith f acc (AndPattern spr spsr) =
  let (acc',spr') = storePatternWith f acc spr
      (acc'',spsr') = storePatternsWith f acc' spsr
     in (acc'',AndStoredPattern spr' spsr')

storePatternWith :: ((MessageType m,Typeable s) => Channel (s :: Synchronicity *) m -> MatchRep m -> acc -> (acc,Maybe BoxIx))
                 -> acc
                 -> PatternRep s m p
                 -> (acc,StoredPatternRep s m p)
storePatternWith f acc (Pattern c mr sp) = let (acc',mBoxIx) = f c mr acc
                                              in (acc',StoredPattern c mBoxIx sp)


-- | Transform the contained 'refine' value of a StoredDefinitionRep by some function
-- which has access to the StoredDefinition's StoredPatterns to decide this new refine' value.
mapStoredDefinitions :: (forall ts. StoredPatternsRep ts -> refine')
                     -> StoredDefinitionsRep tss ()
                     -> StoredDefinitionsRep tss refine'
mapStoredDefinitions f (OneStoredDefinition sdr) = OneStoredDefinition (mapStoredDefinition f sdr)
mapStoredDefinitions f (AndStoredDefinition sdr sdsr) = AndStoredDefinition (mapStoredDefinition f sdr)
                                                                                 (mapStoredDefinitions f sdsr)

mapStoredDefinition :: (StoredPatternsRep ts -> refine')
                    -> StoredDefinitionRep ts tr ()
                    -> StoredDefinitionRep ts tr refine'
mapStoredDefinition f (StoredDefinition sp tr ()) = let r = f sp in StoredDefinition sp tr r

