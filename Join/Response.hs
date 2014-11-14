{-|
Module      : Join.Response
Copyright   : (c) Samuel A. Yallop, 2014
Maintainer  : syallop@gmail.com
Stability   : experimental

This module defines a 'Response' type, which can be used to encapsulate a value which may not
have been computed yet.

The type may be used to wrap the return value of synchronous calls. The advantage of doing so would be that
other actions could be performed after the synchronous call and before the point where the value is
required. This could eliminate unnecessary waiting.
-}
module Join.Response
    ( Response()
    , emptyResponse
    , readResponse
    , writeResponse
    ) where

import Prelude hiding (read)

import Control.Applicative ((<$>))
import Control.Concurrent.MVar

import System.IO.Unsafe

-- |
newtype Response a = Response (MVar a)

-- | Initialise a new empty 'Response'.
emptyResponse :: IO (Response a)
emptyResponse = Response <$> newEmptyMVar

-- | Block, reading a reponse.
{-# NOINLINE readResponse #-}
readResponse :: Response a -> a
readResponse (Response ma) = unsafePerformIO $ takeMVar ma

-- | Write a value to a 'Response'.
-- Throw an exception if the 'Response' has already been written to.
-- - This is invalid usage.
writeResponse :: Response a -> a -> IO ()
writeResponse (Response ma) value = do
  success <- tryPutMVar ma value
  if success
    then return ()
    else error "Response already written to."

