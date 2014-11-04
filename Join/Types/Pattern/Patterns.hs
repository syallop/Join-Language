{-# LANGUAGE DataKinds
            ,FlexibleInstances
            ,GADTs
            ,KindSignatures
            ,MultiParamTypeClasses
            ,TypeOperators
  #-}
module Join.Types.Pattern.Patterns
  (And(..)
  ,(&)
  ) where

import Join.Types.Pattern.Rep

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
data And s m p (ts :: [*]) where
  And :: (Pattern t s m p,Patterns t' s' m' p' ts)
      => t
      -> t'
      -> And s m p ((PatternRep s m p) ': ts)

-- | Infix 'And'.
(&) :: (Pattern t s m p,Patterns t' s' m' p' ts)
      => t
      -> t'
      -> And s m p ((PatternRep s m p) ': ts)
infixr 7 &
(&) = And

instance Patterns (And s m p ts) s m p ts
  where toPatternsRep (And p p') = AndPattern (toPatternRep p) (toPatternsRep p')

