{-# LANGUAGE FlexibleContexts
            ,FlexibleInstances
            ,GADTs
            ,MultiParamTypeClasses
            ,RankNTypes
 #-}
{-|
Module      : Join.Interpreter.Basic
Copyright   : (C) Samuel A. Yallop, 2014
Maintainer  : syallop@gmail.com
Stability   : experimental

This module exports:

- A simple interface to make writing Language interpreters more
convenient.

- The full interface used to define the Language.
-}
module Join.Interpreter.Interface
  (-- * Simple Interface
   -- | This interface is provided as a convenience for writing interpreters
   -- to the Language.
   --
   -- An interpreter can be written to this interface by declaring
   -- an 'Interpreter' instance and providing functions for each
   -- 'Instruction'.
   --
   -- This instance will take the form:
   --
   -- @
   -- instance Interpreter INTERPRETERT IO where
   --   getInterpreterFs INTERPRETER = InterpreterFs
   --     {_iReturn     = \a -> return a
   --     ,_iDef        = \definitions -> ...
   --     ,_iNewChannel = ...
   --     ...
   --     ,iWith        = \p q -> ...
   --     }
   -- @
   --
   -- A run function would then take the form:
   --
   -- @
   --   initialInterpreter :: IO INTERPRETERT
   --   initialInterpreter = ...
   --
   --   run :: Process a -> IO a
   --   run p = do
   --     ...
   --     interpreter <- initialInterpreter
   --     ...
   --     a <- interpretWith interpreter p
   --     ...
   --     return a
   -- @
   InterpreterFs(..)
  ,Interpreter(getInterpreterFs)
  ,interpretWith

  -- * Full Interface
  -- | The 'Process' type is implemented as an operational monad
  -- on the 'Instruction' type with 'IO' at the base.
  --
  -- More flexible interpreters can be written using this interface.
  --
  -- For general usage examples see <http://hackage.haskell.org/package/operational>
  --
  -- A specific example is this modules internal 'interpretWith\'' function.
  ,Process
  ,Instruction(..)
  ) where

import Join.Language
import Join.Pattern
import Join.Pattern.Rep
import Join.Channel
import Join.Response

import Control.Monad.IO.Class

import qualified DSL.Program             as DSL
import qualified DSL.Program.Interpreter as DSL

-- | Record of interpreter functions, one for each 'Instruction'
-- , in some context 'm'.
data InterpreterFs m = InterpreterFs
  {_iReturn     :: forall a. a -> m a
  ,_iDef        :: ToDefinitions d tss Inert => d -> m ()
  ,_iNewChannel :: InferChannel s a => m (Channel s a)
  ,_iSend       :: MessageType a => Chan a -> a -> m ()
  ,_iSpawn      :: Process () -> m ()
  ,_iSync       :: (MessageType a,MessageType r) => SyncChan a r -> a -> m (Response r)
  ,_iReply      :: MessageType r => SyncChan a r -> r -> m ()
  ,_iWith       :: Process () -> Process () -> m ()
  }

-- | Class of types a 'InterpreterFs' can be built from.
class MonadIO m => Interpreter i m where
  getInterpreterFs :: i -> InterpreterFs m

-- trivial instance
instance MonadIO m => Interpreter (InterpreterFs m) m where
  getInterpreterFs = id

-- | With an 'Interpreter' type 'i', interpret a 'Process'
interpretWith :: Interpreter i m => i -> Process a -> m a
{-interpretWith int = interpretWith' (getInterpreterFs int)-}
interpretWith int = DSL.interpretProgramUsing (interpretUsingInterpreter $ getInterpreterFs int)

-- With an 'InterpreterFs', interpret a 'Process'.
{-interpretWith' :: MonadIO m => InterpreterFs m -> Process a -> m a-}
{-interpretWith' intFs (Program proc) = case proc of-}

interpretUsingInterpreter :: MonadIO m => InterpreterFs m -> DSL.Interpreter Instruction m
interpretUsingInterpreter intFs = DSL.Interpreter $ \inst -> case inst of

    Def definitions
      -> _iDef intFs definitions

    NewChannel
      -> _iNewChannel intFs

    Send c m
      -> _iSend intFs c m

    Spawn p
      -> _iSpawn intFs p

    Sync sc sm
      -> _iSync intFs sc sm

    Reply sc rm
      -> _iReply intFs sc rm

    With p q
      -> _iWith intFs p q

    IOAction io
      -> liftIO io

