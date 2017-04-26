{-# LANGUAGE DataKinds
           , FlexibleContexts
           , RankNTypes
           , ScopedTypeVariables
  #-}
{-|
Module      : Join.Data.JVar
Copyright   : (c) Samuel A. Yallop, 2017
Maintainer  : syallop@gmail.com
Stability   : experimental

A 'JVar' mimics the 'Control.Concurrent.MVar' API and provides an abstraction for putting
, taking and waiting on concurrent variables.
-}
module Join.Data.JVar
  ( JVar ()
  , newEmptyJVar
  , newJVar

  , takeJVar
  , putJVar
  , readJVar
  , swapJVar
  )
  where

import Data.Maybe (isJust,isNothing)

import Join
import Join.Pattern.Pass

-- | A 'JVar' is a synchronising variable used for communication between
-- concurrent Join 'Process's and implemented with Join definitions.
data JVar a = JVar
  {_JVarState :: Channel A      (Maybe a)
  ,_JVarTake  :: Channel (S a)  ()
  ,_JVarRead  :: Channel (S a)  ()
  ,_JVarSwap  :: Channel (S a)  a
  ,_JVarPut   :: Channel (S ()) a
  }

-- | Create a JVar which is initially holds no value.
newEmptyJVar :: forall a . MessageType a => Process (JVar a)
newEmptyJVar = do
  -- Declare the channels we'll use
  state <- newChannel :: Process (Channel A (Maybe a))
  take  <- newChannel :: Process (Channel (S a) ())    -- Request for take
  read  <- newChannel :: Process (Channel (S a) ())    -- Request for read
  swap  <- newChannel :: Process (Channel (S a) a)     -- Request for swap
  put   <- newChannel :: Process (Channel (S ()) a)    -- Request to put a value

     -- Extract and reply the state to the take request.
  def $ (state&~isJust    & take        |> \(Just st)      -> reply take  st  `with` send state Nothing)

     -- Extract and reply the state to the read request AND place the state back.
     |$ (state&~isJust    & read        |> \(Just st)      -> reply read  st  `with` send state (Just st))

     -- Extract and reply the old state AND place a new state.
     -- NOTE: The 'Passing' adaptor is required as otherwise we can't tell if
     -- 'a' will be instantiated with a message type that will be passed or not.
     |$ (state&~isJust    & (Pass swap) |> \(Just st0) st1 -> reply swap st0  `with` send state (Just st1))

     -- Put a new state if the old one is empty.
     |$ (state&~isNothing & (Pass put)  |> \Nothing    st  -> acknowledge put `with` send state (Just st))

  -- Initially empty
  send state Nothing

  return $ JVar state take read swap put

-- | Create a 'JVar' which contains the supplied value.
newJVar :: MessageType a => a -> Process (JVar a)
newJVar st = do
  jvar <- newEmptyJVar
  putJVar jvar st
  return jvar


-- | Return the contents of a 'JVar'. If the 'JVar' is currently empty,
-- 'takeJVar' will wait until it is full. After a 'takeJVar', the 'JVar' is left
-- empty.
takeJVar :: MessageType a => JVar a -> Process a
takeJVar jVar = syncSignal' (_JVarTake jVar)

-- | Put a value into an 'JVar'. If the 'JVar' is currently full, 'putJVar' will
-- wait until it becomes empty.
putJVar :: MessageType a => JVar a -> a -> Process ()
putJVar jVar = sync' (_JVarPut jVar)

-- | Read the contents of an 'JVar'. If the 'JVar' is currently empty,
-- 'readJVar' will wait until its full.
readJVar :: MessageType a => JVar a -> Process a
readJVar jVar = syncSignal' (_JVarRead jVar)

-- | Take a value from an 'JVar', put a new value into the 'JVar' and return
-- the value taken.
swapJVar :: MessageType a => JVar a -> a -> Process a
swapJVar jVar = sync' (_JVarSwap jVar)

