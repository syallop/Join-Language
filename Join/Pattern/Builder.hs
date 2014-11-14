{-# LANGUAGE GADTs
            ,TemplateHaskell
            ,TypeFamilies
            ,TypeOperators
            ,UndecidableInstances
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

  ,module Join.Pattern.Builder.Natural
  ,module Join.Pattern.Builder.Vector
  ) where

import Prelude hiding (head,tail,zip,append,snoc,replicate)

import Join.Pattern.Builder.Natural
import Join.Pattern.Builder.Vector
import Join.Pattern.Rep
import Join.Pattern.Rep

-- | Map the elements of a Vector to a single concatenated Definitions's.
buildWith :: (a -> Definitions tss r) -> Vector n a -> Definitions (n:*tss) r
buildWith f v = build $ mapVector f v

-- | Concatenate a Vector of Definitions.
build :: Vector n (Definitions tss r) -> Definitions (n:*tss) r
build (VOne a) = a
build (VAnd a vs) = appendDefinitions a (build vs)

-- | A list type 'l', appended to itself 'n' times.
type family Repeat n l
  where Repeat One     l = l
        Repeat (Suc n) l = l :++ (Repeat n l)
type (:*) = Repeat

