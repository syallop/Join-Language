module Join.Pattern.Clause
  ((|>)
  ) where

import Join.Apply
import Join.Pattern.Rep

-- | Build a definition infix from a patterns type and an associated trigger function.
(|>) :: (ToPatterns pat ts,HasTriggerType ts tr r,Apply tr r)
     => pat -> tr -> Definition ts tr r
infixr 6 |>
ps |> tr = Definition (toPatterns ps) (Trigger tr)

