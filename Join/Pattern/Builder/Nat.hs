{-# LANGUAGE DataKinds
           , ExplicitNamespaces
           , KindSignatures
           , TypeFamilies
           , TypeOperators
  #-}
{-|
Module      : Join.Pattern.Builder.Nat
Copyright   : (c) Samuel A. Yallop, 2014
Maintainer  : syallop@gmail.com
Stability   : experimental

Represent non-zero natural numbers at the type level.

Module is separate from Builder.Natural so as not to define uneeded
One and Suc terms which are otherwise unnecessarily created to get the
corresponding types from DataKinds.
This allows Builder.Natural to make use of the term names itself.

Ideally we want to say something like:
data kind Nat
  = One
  | Suc Nat
-}
module Join.Pattern.Builder.Nat
  (Nat -- Type/Kind
  ,One -- Type :: Nat
  ,Suc -- Type :: Nat
  ,Plus,(:+:)
  ) where

-- | 'Nat' is used as the *kind* of natural numbers excluding a zero.
-- I.E.
--
-- @
--   One
--
--   Suc One
--
--   Suc Suc One
-- @
--
-- are all types of kind 'Nat'.
data Nat
  = One
  | Suc Nat

-- | @ One :: Nat @
type One = 'One

-- | @ Suc :: Nat -> Nat @
type Suc = 'Suc

-- | Addition of 'Nat's.
type family Plus (n :: Nat) (m :: Nat) :: Nat where
  Plus One     m = (Suc m)
  Plus (Suc n) m = Suc    (Plus n m)
type (:+:) = Plus

