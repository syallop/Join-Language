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
Join.Pattern.Rep.* will later be abstracted to cover this module.
-}

module Join.Interpretation.Basic.StoredDefinitions
  (StoredPattern(..)
  ,StoredPatterns(..)
  ,StoredDefinition(..)
  ,StoredDefinitions(..)

  ,foldStoredPatterns
  ,foldStoredDefinitions

  ,storeDefinitionsWith
  ,mapStoredDefinitions
  ) where

import Data.Typeable

import Join
import Join.Pattern.Rep

import Join.Interpretation.Basic.MessageBox

-- | Stored analogy to 'Pattern' with 'Match' replaced
-- by a possible 'BoxIx' referencing the subbox where messages which
-- would match a 'MatchWhen pred' predicate are kept.
data StoredPattern s m p where
  StoredPattern :: (MessageType m,Typeable s)
                => Channel (s :: Synchronicity *) m
                -> Maybe BoxIx
                -> ShouldPass p
                -> StoredPattern s m p

-- | Stored analogy to 'Patterns'.
data StoredPatterns (ts :: [*]) where
  OneStoredPattern :: StoredPattern s m p
                   -> StoredPatterns '[Pattern s m p]

  AndStoredPattern :: StoredPattern s m p
                   -> StoredPatterns ts
                   -> StoredPatterns ((Pattern s m p) ': ts)

-- | Stored analogy to 'Definition', with differences:
-- - StoredPatterns instead of Pattern
-- - Concrete return type Inert.
-- - additional 'refine' type allowing definitions to be tagged with a value.
data StoredDefinition ts tr refine where
  StoredDefinition :: (tr~TriggerType ts Inert,Apply tr Inert)
                   => StoredPatterns ts
                   -> Trigger tr Inert
                   -> refine
                   -> StoredDefinition ts tr refine

-- | Stored analogy to 'Definitions'.
data StoredDefinitions tss refine where
  OneStoredDefinition :: StoredDefinition ts tr refine
                      -> StoredDefinitions '[Definition ts tr Inert] refine

  AndStoredDefinition :: StoredDefinition ts tr refine
                      -> StoredDefinitions tss refine
                      -> StoredDefinitions ((Definition ts tr Inert) ': tss) refine

-- | Fold a function over the StoredPattern contained within a StoredPatterns.
foldStoredPatterns :: (forall m s p. (MessageType m,Typeable s) => Channel (s::Synchronicity *) m -> Maybe BoxIx -> ShouldPass p -> acc -> acc)
                   -> acc
                   -> StoredPatterns ts
                   -> acc
foldStoredPatterns f acc (OneStoredPattern (StoredPattern c mb sp)) = f c mb sp acc
foldStoredPatterns f acc (AndStoredPattern (StoredPattern c mb sp) sps) = foldStoredPatterns f (f c mb sp acc) sps

-- | Fold a function over the StoredDefinition contained within a StoredDefinitions.
foldStoredDefinitions :: (forall ts tr. StoredDefinition ts tr refine -> acc -> acc)
                         -> acc
                         -> StoredDefinitions tss refine
                         -> acc
foldStoredDefinitions f acc (OneStoredDefinition sdr)      = f sdr acc
foldStoredDefinitions f acc (AndStoredDefinition sdr sdrs) = foldStoredDefinitions f (f sdr acc) sdrs




-- | Store all contained Pattern by using a function to map 'MatchWhen pred's and an accumulator
-- to a BoxIx and an updated accumulator.
storeDefinitionsWith :: (forall s m. (MessageType m,Typeable s) => Channel (s :: Synchronicity *) m -> Match m -> acc -> (acc,Maybe BoxIx))
                     -> acc
                     -> Definitions tss Inert
                     -> (acc,StoredDefinitions tss ())
storeDefinitionsWith f acc (OneDefinition dr) =
  let (acc',dr') = storeDefinitionWith f acc dr
     in (acc',OneStoredDefinition dr')
storeDefinitionsWith f acc (AndDefinition dr drs) =
  let (acc',dr') = storeDefinitionWith f acc dr
      (acc'',drs') = storeDefinitionsWith f acc' drs
     in (acc'',AndStoredDefinition dr' drs')

storeDefinitionWith :: (forall s m. (MessageType m,Typeable s) => Channel (s :: Synchronicity *) m -> Match m -> acc -> (acc,Maybe BoxIx))
                    -> acc
                    -> Definition ts tr Inert
                    -> (acc,StoredDefinition ts tr ())
storeDefinitionWith f acc (Definition pr tr) = let (acc',pr') = storePatternsWith f acc pr
                                                  in (acc',StoredDefinition pr' tr ())
storePatternsWith :: (forall s m. (MessageType m,Typeable s) => Channel (s :: Synchronicity *) m -> Match m -> acc -> (acc,Maybe BoxIx))
                  -> acc
                  -> Patterns ts
                  -> (acc,StoredPatterns ts)
storePatternsWith f acc (OnePattern spr) =
  let (acc',spr') = storePatternWith f acc spr
     in (acc',OneStoredPattern spr')
storePatternsWith f acc (AndPattern spr spsr) =
  let (acc',spr') = storePatternWith f acc spr
      (acc'',spsr') = storePatternsWith f acc' spsr
     in (acc'',AndStoredPattern spr' spsr')

storePatternWith :: ((MessageType m,Typeable s) => Channel (s :: Synchronicity *) m -> Match m -> acc -> (acc,Maybe BoxIx))
                 -> acc
                 -> Pattern s m p
                 -> (acc,StoredPattern s m p)
storePatternWith f acc (Pattern c mr sp) = let (acc',mBoxIx) = f c mr acc
                                              in (acc',StoredPattern c mBoxIx sp)


-- | Transform the contained 'refine' value of a StoredDefinition by some function
-- which has access to the StoredDefinition's StoredPatterns to decide this new refine' value.
mapStoredDefinitions :: (forall ts. StoredPatterns ts -> refine')
                     -> StoredDefinitions tss ()
                     -> StoredDefinitions tss refine'
mapStoredDefinitions f (OneStoredDefinition sdr) = OneStoredDefinition (mapStoredDefinition f sdr)
mapStoredDefinitions f (AndStoredDefinition sdr sdsr) = AndStoredDefinition (mapStoredDefinition f sdr)
                                                                                 (mapStoredDefinitions f sdsr)

mapStoredDefinition :: (StoredPatterns ts -> refine')
                    -> StoredDefinition ts tr ()
                    -> StoredDefinition ts tr refine'
mapStoredDefinition f (StoredDefinition sp tr ()) = let r = f sp in StoredDefinition sp tr r

