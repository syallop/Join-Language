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
  ( DistProgram
  , DistProgramIn
  , DistInst(..)

  , lookupChannel
  , registerChannel
  ) where

import Join.Channel

import DSL.Instruction
import DSL.Program

type Name = String

-- | Type of Join instructions used in the distribution of
-- 'Channel's.
data DistInst (p :: * -> *) (a :: *) where

  -- Lookup a distributed 'Channel' with the given name (and type).
  LookupChannel
    :: MessageType a
    => Name
    -> DistInst p (Maybe (Channel A a))

  -- Register a distributed 'Channel' with the given name (and type).
  RegisterChannel
    :: MessageType a
    => Name
    -> Channel A a
    -> DistInst p Bool

-- | 'DistProgram' is a Monadic type that can be thought of as representing a sequence of 'DistInst'
-- instructions only.
type DistProgram a = Program DistInst a

-- | 'DistProgramIn' is a Monadic type that can be thought of as representing a sequence of "DSL-Compose"
-- compatible instructions, one of which must be the distributed instructions 'DistInst'.
type DistProgramIn i a = (DistInst :<- i) => Program i a

-- | Enter a single 'LookupChannel' instruction into a compatible Program.
--
-- Lookup a distributed 'Channel' with the given name (and type).
lookupChannel :: MessageType a => Name -> DistProgramIn i (Maybe (Channel A a))
lookupChannel n = inject $ LookupChannel n

-- | Enter a single 'registerChannel' instruction into a compatible Program.
--
-- Register a distributed 'Channel' with the given name (and type).
registerChannel :: MessageType a => Name -> Channel A a -> DistProgramIn i Bool
registerChannel n c = inject $ RegisterChannel n c

