{-# LANGUAGE DataKinds
            ,FlexibleInstances
            ,GADTs
            ,KindSignatures
            ,MultiParamTypeClasses
            ,TypeOperators
  #-}
module Join.Pattern.Patterns
  (And(..)
  ,(&)
  ) where

import Join.Pattern.Rep

-- | Pattern type of matching a conjunction of patterns.
--
-- Declared infix via '&'.
--
-- Composition declares a pattern that matches only when both
-- component patterns match.
--
-- Corresponding trigger types are composed with '->'.
--
-- E.G. If we have the following pattern types, which determine triggers:
--
-- - @ intChan @ => @ trigger :: Int -> return @
--
-- - @ charChan @ => @ trigger :: Char -> return @
--
-- - @ boolEq @ => @ trigger :: return @
--
-- Then:
--
-- - @ intChan & charChan @ => @ trigger :: Int -> Char -> return @
--
-- - @ intChan & boolEq @ => @ trigger :: Int -> return @
--
-- - @ intChan & boolEq & charChan @ => @ trigger :: Int -> Char -> return @
data And (ts :: [*]) where
  And :: (ToPattern t s m p,ToPatterns t' ts)
      => t
      -> t'
      -> And ((Pattern s m p) ': ts)

-- | Infix 'And'.
(&) :: (ToPattern t s m p,ToPatterns t' ts)
      => t
      -> t'
      -> And ((Pattern s m p) ': ts)
infixr 7 &
(&) = And

instance ToPatterns (And ts) ts
  where toPatterns (And p p') = AndPattern (toPattern p) (toPatterns p')

