{-# LANGUAGE
    GADTs
  , TemplateHaskell
  #-}
{-|
Module      : Join.Pattern.Builder.Natural
Copyright   : (c) Samuel A. Yallop, 2014
Maintainer  : syallop@gmail.com
Stability   : experimental

Represent non-zero natural numbers whose term corresponds exactly
to their type.
-}
module Join.Pattern.Builder.Natural
  (Nat
  ,One
  ,Suc
  ,Plus(),(:+:)

  ,Natural(One,Suc)
  ,toNatural
  ) where

import Join.Pattern.Builder.Nat

import Language.Haskell.TH

-- | Type of natural numbers excluding zero.
-- Each term uniquely determines (and is determined by)
-- a Nat type.
-- I.E.
--
-- @
-- One :: Natural One
--
-- Suc One :: Natural (Suc One)
--
-- Suc (Suc One) :: Natural (Suc (Suc One))
-- @
data Natural n where
  One :: Natural One
  Suc :: Natural n -> Natural (Suc n)

-- | Use TemplateHaskell to conveniently write 'Natural's.
-- I.E.
--
-- @ $(toNatural 3) == Suc (Suc One) :: Natural (Suc (Suc One)) @
toNatural :: Integer -> Q Exp
toNatural 1 = [e| One |]
toNatural n = [e| Suc $(toNatural (n-1)) |]

