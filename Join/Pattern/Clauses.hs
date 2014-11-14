{-# LANGUAGE DataKinds
            ,FlexibleInstances
            ,GADTs
            ,MultiParamTypeClasses
            ,TypeOperators
  #-}
module Join.Pattern.Clauses where

import Join.Pattern.Rep.Definition

-- | Many related definitions.
--
-- Declared infix via '|$'.
data DefinitionClauses tss r where
  DefinitionClauses :: (ToDefinition t ts tr r,ToDefinitions t' tss r)
                 => t
                 -> t'
                 -> DefinitionClauses ((Definition ts tr r) ': tss) r

-- | Infix 'DefinitionClauses'.
(|$) :: (ToDefinition t ts tr r,ToDefinitions t' tss r)
     => t
     -> t'
     -> DefinitionClauses ((Definition ts tr r) ': tss) r
infixr 5 |$
(|$) = DefinitionClauses

instance ToDefinitions (DefinitionClauses tss r) tss r
  where toDefinitions (DefinitionClauses t t') = AndDefinition (toDefinition t) (toDefinitions t')

