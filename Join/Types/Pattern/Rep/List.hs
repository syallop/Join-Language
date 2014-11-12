{-# LANGUAGE DataKinds
            ,TypeFamilies
            ,TypeOperators
  #-}
{-|
Module      : Join.Types.Pattern.Rep.List
Copyright   : (c) Samuel A. Yallop, 2014
Maintainer  : syallop@gmail.com
Stability   : experimental

Export type functions used under Pattern.
-}

module Join.Types.Pattern.Rep.List
  (Append
  ,(:++)
  ) where

type family Append tss tss' where
  Append '[]      l' = l'
  Append (e ': l) l' = e ': Append l l'
infixr 5 :++
type (:++) = Append

