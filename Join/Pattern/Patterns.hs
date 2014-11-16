{-# LANGUAGE DataKinds
            ,TypeOperators
  #-}
module Join.Pattern.Patterns
  ((&)
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
(&) :: (ToPattern t s m p,ToPatterns t' ts)
    => t -> t' -> Patterns ((Pattern s m p) ': ts)
infixr 7 &
p & ps = AndPattern (toPattern p) (toPatterns ps)

