{-|
Module      : Join.Pattern
Copyright   : (c) Samuel A. Yallop, 2014
Maintainer  : syallop@gmail.com
Stability   : experimental

This module exports a syntax for writing Join Patterns/ Definitions
as encoded by 'Join.Pattern.Rep'.
-}

module Join.Pattern
  (
   module P
  ) where

import Join.Pattern.Channel  as P
import Join.Pattern.Clause   as P
import Join.Pattern.Clauses  as P
import Join.Pattern.Eq       as P
import Join.Pattern.Patterns as P
import Join.Pattern.Pred     as P

