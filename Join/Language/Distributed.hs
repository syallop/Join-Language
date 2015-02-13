{-# LANGUAGE
    ConstraintKinds
  , DataKinds
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , KindSignatures
  , MultiParamTypeClasses
  , RankNTypes
  , TemplateHaskell
  , TypeOperators
  , TypeSynonymInstances
  #-}
{-|
Module      : Join.Language.Distributed
Copyright   : (c) Samuel A. Yallop, 2015
Maintainer  : syallop@gmail.com
Stability   : experimental

This module exports a small DSL for distributing Join Channels
, which may be composed with other "DSL-Compose" DSL's, in particular
. with "Join.Language"s core Join DSL.

Provided are instructions for looking up and registering Channels against identifying String Names.
-}
module Join.Language.Distributed
  ( Dist
  , DistInst(..)

  , lookupChannel
  , registerChannel
  ) where

import Join.Channel

import DSL.Program

type Name = String

-- | Type of Join instructions used in the distribution of
-- 'Channel's.
data DistInst (a :: *) where

  -- Lookup a distributed 'Channel' with the given name (and type).
  LookupChannel
    :: MessageType a
    => Name
    -> DistInst (Maybe (Channel A a))

  -- Register a distributed 'Channel' with the given name (and type).
  RegisterChannel
    :: MessageType a
    => Name
    -> Channel A a
    -> DistInst Bool

-- | A Monadic type encapsulating a sequence of 'DistInst's.
type Dist a = Program DistInst a

type UsingDist a = ProgramUsing DistInst a

-- | Enter a single 'LookupChannel' instruction into a compatible Program.
--
-- Lookup a distributed 'Channel' with the given name (and type).
lookupChannel :: MessageType a => Name -> UsingDist (Maybe (Channel A a))
lookupChannel n = inject $ LookupChannel n

-- | Enter a single 'registerChannel' instruction into a compatible Program.
--
-- Register a distributed 'Channel' with the given name (and type).
registerChannel :: MessageType a => Name -> Channel A a -> UsingDist Bool
registerChannel n c = inject $ RegisterChannel n c

