{-# LANGUAGE
    GADTs
  , TemplateHaskell
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
  #-}
{-|
Module      : Join.Pattern.Builder
Copyright   : (c) Samuel A. Yallop, 2014
Maintainer  : syallop@gmail.com
Stability   : experimental

Build larger Definitions from smaller abstracted components
in a stongly typed manner.

-}
module Join.Pattern.Builder
  (type Repeat
  ,buildWith
  ,build

  ,module Data.NonZero.Natural
  ,module Data.NonZero.Vector
  ) where

import Prelude hiding (head,tail,zip,append,snoc,replicate)

import Data.NonZero.Vector hiding ((++))
import Data.NonZero.Natural
import Join.Pattern.Rep

-- | Map the elements of a Vector to a single concatenated Definitions's.
buildWith :: (a -> Definitions tss r) -> Vector n a -> Definitions (n:*tss) r
buildWith f v = build $ mapVector f v

-- | Concatenate a Vector of Definitions.
build :: Vector n (Definitions tss r) -> Definitions (n:*tss) r
build (Only a) = a
build (a :| vs) = appendDefinitions a (build vs)

-- | A list type 'l', appended to itself 'n' times.
type family Repeat n l
  where Repeat One     l = l
        Repeat (Suc n) l = l :++ (Repeat n l)
type n :* l = Repeat n l

