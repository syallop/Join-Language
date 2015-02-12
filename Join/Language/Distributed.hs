{-# LANGUAGE ConstraintKinds
            ,DataKinds
            ,FlexibleContexts
            ,FlexibleInstances
            ,GADTs
            ,KindSignatures
            ,MultiParamTypeClasses
            ,RankNTypes
            ,TemplateHaskell
            ,TypeOperators
            ,TypeSynonymInstances
 #-}

module Join.Language.Distributed
  ( Dist
  , DistInst(..)

  , lookupChannel
  , registerChannel
  ) where

import Join.Channel

import DSL.Program

type Name = String

data DistInst (a :: *) where

  LookupChannel
      :: MessageType a
      => Name
      -> DistInst (Maybe (Channel A a))

  RegisterChannel
      :: MessageType a
      => Name
      -> Channel A a
      -> DistInst Bool

type Dist a = Program DistInst a

type UsingDist a = ProgramUsing DistInst a

lookupChannel :: MessageType a => Name -> UsingDist (Maybe (Channel A a))
lookupChannel n = inject $ LookupChannel n

registerChannel :: MessageType a => Name -> Channel A a -> UsingDist Bool
registerChannel n c = inject $ RegisterChannel n c

