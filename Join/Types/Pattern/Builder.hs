{-# LANGUAGE GADTs
            ,TemplateHaskell
            ,TypeFamilies
            ,TypeOperators
            ,UndecidableInstances
  #-}
{-|
Module      : Join.Types.Pattern.Builder
Copyright   : (c) Samuel A. Yallop, 2014
Maintainer  : syallop@gmail.com
Stability   : experimental

Build larger Definitions from smaller abstracted components
in a stongly typed manner.

-}
module Join.Types.Pattern.Builder
  (type Repeat
  ,buildWith
  ,build

  ,module Join.Types.Pattern.Builder.Natural
  ,module Join.Types.Pattern.Builder.Vector
  ) where

import Prelude hiding (head,tail,zip,append,snoc,replicate)

import Join.Types.Pattern.Builder.Natural
import Join.Types.Pattern.Builder.Vector
import Join.Types.Pattern.Rep
import Join.Types.Pattern.Rep

-- | Map the elements of a Vector to a single concatenated DefinitionsRep's.
buildWith :: (a -> DefinitionsRep tss r) -> Vector n a -> DefinitionsRep (n:*tss) r
buildWith f v = build $ mapVector f v

-- | Concatenate a Vector of DefinitionsRep.
build :: Vector n (DefinitionsRep tss r) -> DefinitionsRep (n:*tss) r
build (VOne a) = a
build (VAnd a vs) = appendDefinitionsRep a (build vs)

-- | A list type 'l', appended to itself 'n' times.
type family Repeat n l
  where Repeat One     l = l
        Repeat (Suc n) l = l :++ (Repeat n l)
type (:*) = Repeat

