{-# LANGUAGE DataKinds,TypeOperators
  #-}
module Join.Pattern.Clauses
  ((|$)
  ) where

import Join.Pattern.Rep.Definition

-- | Build definitions infix by prepending a single definition type to a definitions type.
(|$) :: (ToDefinition t ts tr r,ToDefinitions t' tss r)
     => t
     -> t'
     -> Definitions ((Definition ts tr r) ': tss) r
infixr 5 |$
d |$ ds = AndDefinition (toDefinition d) (toDefinitions ds)

