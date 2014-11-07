{-# LANGUAGE DataKinds
            ,GADTs
            ,FlexibleInstances
            ,TypeOperators
            ,MultiParamTypeClasses
  #-}
module Join.Types.Pattern.Clause
  (DefinitionClause(..)
  ,(|>)
  ) where

import Join.Types.Apply
import Join.Types.Pattern.Rep

-- | A DefinitionClause is a pattern type and an associated trigger function.
--
-- Declared infix via '|>'.
data DefinitionClause ts tr r where
  DefinitionClause :: (Patterns pat ts,HasTriggerType ts tr r,Apply tr r)
                => pat
                -> tr
                -> DefinitionClause ts tr r

-- | Infix 'DefinitionClause'.
(|>) :: (Patterns pat ts,HasTriggerType ts tr r,Apply tr r)
                => pat
                -> tr
                -> DefinitionClause ts tr r
infixr 6 |>
(|>) = DefinitionClause

instance Definition (DefinitionClause ts tr r) ts tr r
  where toDefinitionRep (DefinitionClause pat tr) = Definition (toPatternsRep pat) (Trigger tr)

instance Definitions (DefinitionClause ts tr r) '[DefinitionRep ts tr r] r
  where toDefinitionsRep (DefinitionClause pat tr) = OneDefinition (Definition (toPatternsRep pat) (Trigger tr))

