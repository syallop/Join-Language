{-# LANGUAGE DataKinds
            ,GADTs
            ,FlexibleInstances
            ,TypeOperators
            ,MultiParamTypeClasses
  #-}
module Join.Pattern.Clause
  (DefinitionClause(..)
  ,(|>)
  ) where

import Join.Apply
import Join.Pattern.Rep

-- | A DefinitionClause is a pattern type and an associated trigger function.
--
-- Declared infix via '|>'.
data DefinitionClause ts tr r where
  DefinitionClause :: (ToPatterns pat ts,HasTriggerType ts tr r,Apply tr r)
                   => pat
                   -> tr
                   -> DefinitionClause ts tr r

-- | Infix 'DefinitionClause'.
(|>) :: (ToPatterns pat ts,HasTriggerType ts tr r,Apply tr r)
     => pat
     -> tr
     -> DefinitionClause ts tr r
infixr 6 |>
(|>) = DefinitionClause

instance ToDefinition (DefinitionClause ts tr r) ts tr r
  where toDefinition (DefinitionClause pat tr) = Definition (toPatterns pat) (Trigger tr)

instance ToDefinitions (DefinitionClause ts tr r) '[Definition ts tr r] r
  where toDefinitions (DefinitionClause pat tr) = OneDefinition (Definition (toPatterns pat) (Trigger tr))

