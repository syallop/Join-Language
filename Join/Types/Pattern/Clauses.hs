{-# LANGUAGE DataKinds
            ,FlexibleInstances
            ,GADTs
            ,MultiParamTypeClasses
            ,TypeOperators
  #-}
module Join.Types.Pattern.Clauses where

import Join.Types.Pattern.Rep.Definition

-- | Many related definitions.
--
-- Declared infix via '|$'.
data DefinitionClauses tss r where
  DefinitionClauses :: (Definition t ts tr r,Definitions t' tss r)
                 => t
                 -> t'
                 -> DefinitionClauses ((DefinitionRep ts tr r) ': tss) r

-- | Infix 'DefinitionClauses'.
(|$) :: (Definition t ts tr r,Definitions t' tss r)
     => t
     -> t'
     -> DefinitionClauses ((DefinitionRep ts tr r) ': tss) r
infixr 5 |$
(|$) = DefinitionClauses

instance Definitions (DefinitionClauses tss r) tss r
  where toDefinitionsRep (DefinitionClauses t t') = AndDefinition (toDefinitionRep t) (toDefinitionsRep t')

