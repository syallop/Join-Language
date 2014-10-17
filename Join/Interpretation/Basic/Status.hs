{-|
Module     : Join.Interpreter.Simple.Status
Copyright  : (c) Samuel A. Yallop, 2014
Maintainer : syallop@gmail.com
Stability  : experimental

This module provides a fixed-length bitstring types which may be used to track the status of a system.
-}
module Join.Interpretation.Basic.Status
    (
     -- * Title
     -- | A Status is a fixed length, mutable bitstring of 1's and 0's.
     -- with operations for querying and setting individual bits.
      Status()
    , mkStatus
    , index
    , set
    , unset
    , getStatusIndexes

    , setMany

    -- | A StatusPattern is a fixed length, immutable bitstring of 1's and 0's
    -- describing a pattern upon which Status's may be matched.
    --
    -- A Status matches a StatusPattern when each corresponding bit
    -- matches where a pattern bit of:
    -- - 0 matches 0 or 1
    -- - 1 matches only 1
    , StatusPattern()
    , mkStatusPattern
    , match
    , getPatternIndexes

    , showStatus
    , showStatusPattern
    ) where

import Prelude hiding (replicate,zipWith,and)

import Data.Maybe               (mapMaybe)
import Data.Vector         as V (Vector,replicate,unsafeIndex,modify,(//),zipWith,and,toList,indexed,foldl)
import Data.Vector.Mutable      (write)

newtype Status = Status (Vector Bool) deriving Show

-- | Format a human-readable representation of a Status.
showStatus :: Status -> String
showStatus (Status v) = V.foldl (\str b -> str ++ if b then "1" else "0") "" v

-- | Create a new Status with 'i' elements.
mkStatus :: Int -> Status
mkStatus i = Status $ replicate i False

-- | Index the bool at 'i'.
index :: Status -> Int -> Bool
index (Status v) = unsafeIndex v

-- | Set an index to True.
set :: Status -> Int -> Status
set = setTo True

setMany :: Status -> [Int] -> Status
setMany (Status v) ixs = Status $ v // [(ix,True) | ix <- ixs]

-- | Set an index to False.
unset :: Status -> Int -> Status
unset = setTo False

-- | Set to a value a status index.
setTo :: Bool -> Status -> Int -> Status
setTo b (Status v) i = Status $ modify (\v' -> write v' i b) v

-- | Get a list of set indexes in a status.
--
-- E.G. 
-- prop> getSetIndexes <FFTFT> == [2,4]
getStatusIndexes :: Status -> [Int]
getStatusIndexes (Status v) = getIndexes v


newtype StatusPattern = StatusPattern (Vector Bool) deriving Show

-- | Format a human-readable representation of a StatusPattern.
showStatusPattern :: StatusPattern -> String
showStatusPattern (StatusPattern v) = V.foldl (\str b -> str ++ if b then "1" else "0") "" v

-- | Create a new StatusPattern of length 'i' where 'ixs' denotes a list of
-- indexes set to 1.
mkStatusPattern :: Int -> [Int] -> StatusPattern
mkStatusPattern i ixs = StatusPattern $ replicate i False // [(ix,True) | ix <- ixs]

-- | A Status matches a StatusPattern only when each corresponding bit
-- matches. A pattern bit of:
-- - 0 matches 0 or 1
-- - 1 matches only 1
match :: Status -> StatusPattern -> Bool
match (Status i) (StatusPattern p) = and $ zipWith imp p i
  where
    imp True False = False
    imp _    _     = True

getPatternIndexes :: StatusPattern -> [Int]
getPatternIndexes (StatusPattern v) = getIndexes v

getIndexes :: Vector Bool -> [Int]
getIndexes v = mapMaybe (\(ix,b) -> if b then Just ix else Nothing) (toList . indexed $ v)

