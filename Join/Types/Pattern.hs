{-|
Module      : Join.Types.Pattern
Copyright   : (c) Samuel A. Yallop, 2014
Maintainer  : syallop@gmail.com
Stability   : experimental

This module exports a syntax for building Join Patterns/ Definitions
as encoded by 'Join.Types.Pattern.Rep'.
-}

module Join.Types.Pattern
  (
   module P
  ) where

import Join.Types.Pattern.Channel  as P
import Join.Types.Pattern.Clause   as P
import Join.Types.Pattern.Clauses  as P
import Join.Types.Pattern.Eq       as P
import Join.Types.Pattern.Patterns as P
import Join.Types.Pattern.Pred     as P

