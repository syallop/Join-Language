{-|
Module      : Join.Types.SyncVal
Copyright   : (c) Samuel A. Yallop, 2014
Maintainer  : syallop@gmail.com
Stability   : experimental

This module defines a 'SyncVal' type, which can be used to encapsulate a value which may not
have been computed yet.

The type may be used to wrap the return value of synchronous calls. The advantage of doing so would be that
other actions could be performed after the synchronous call and before the point where the value is
required. This could eliminate unnecessary waiting.
-}
module Join.Types.SyncVal
    ( SyncVal()
    , new
    , read
    , write
    ) where

import Prelude hiding (read)

import Control.Applicative ((<$>))
import qualified Data.IVar.Simple as I

-- | Represents a Synchronous value which may or may not exist yet.
newtype SyncVal a = SyncVal (I.IVar a)

-- | Create a new empty SyncVal.
new :: IO (SyncVal a)
new = SyncVal <$> I.new

-- | Synchronously wait for a value on a SyncVal.
read :: SyncVal a -> a
read (SyncVal i) = I.read i

-- | Write a value to a SyncVal.
-- Throws Data.IVar.Simple.BlockedIndefinitelyOnIVar exception if a value
-- has already been written.
write :: SyncVal a -> a -> IO ()
write (SyncVal i) = I.write i

