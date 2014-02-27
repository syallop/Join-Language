module Join.Language.Types.SyncVal
    ( SyncVal()
    , new
    , wait
    , write
    ) where

import Control.Applicative ((<$>))
import qualified Data.IVar.Simple as I

-- | Represents a Synchronous value which may or may not exist yet.
newtype SyncVal a = SyncVal (I.IVar a)

-- | Create a new empty SyncVal.
new :: IO (SyncVal a)
new = SyncVal <$> I.new

-- | Synchronously wait for a value on a SyncVal.
wait :: SyncVal a -> a
wait (SyncVal i) = I.read i

-- | Write a value to a SyncVal.
-- Throws Data.IVar.Simple.BlockedIndefinitelyOnIVar exception if a value
-- has already been written.
write :: SyncVal a -> a -> IO ()
write (SyncVal i) = I.write i

