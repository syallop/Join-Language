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
  , DistInst(..)

  , lookupChannel
  , registerChannel

  , ProgramUsingDist
  , ProgramUsingDistIn
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

-- | 'ProgramUsingDist' is a Monadic type that can be thought of as representing a sequence of "DSL-Compose"
-- compatible instructions, one of which is the 'DistInst' instruction.
type ProgramUsingDist a = ProgramUsing DistInst a

-- | 'ProgramUsingDistIn' is a Monadic type that can be thought of as representing a sequence of "DSL-Compose"
-- compatible instructions, one of which is the 'CoreInst' instruction, where the type variable
-- 'is' gives the overall composed instruction type.
type ProgramUsingDistIn is a = (DistInst :<- is) => Program is a

-- | Enter a single 'LookupChannel' instruction into a compatible Program.
--
-- Lookup a distributed 'Channel' with the given name (and type).
lookupChannel :: MessageType a => Name -> ProgramUsingDist (Maybe (Channel A a))
lookupChannel n = inject $ LookupChannel n

-- | Enter a single 'registerChannel' instruction into a compatible Program.
--
-- Register a distributed 'Channel' with the given name (and type).
registerChannel :: MessageType a => Name -> Channel A a -> ProgramUsingDist Bool
registerChannel n c = inject $ RegisterChannel n c

