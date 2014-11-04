{-# LANGUAGE ConstraintKinds
            ,DataKinds
            ,FlexibleContexts
            ,FlexibleInstances
            ,GADTs
            ,MultiParamTypeClasses
            ,RankNTypes
            ,TypeOperators
            ,TypeSynonymInstances
 #-}


{-# OPTIONS_HADDOCK prune #-}
{-|
Module      : Join.Language
Copyright   : (c) Samuel A. Yallop, 2014
Maintainer  : syallop@gmail.com
Stability   : experimental

This module describes an encoding of the Join-Calculus.

It defines methods for writing Join-Calculus programs which build a type
'Process' which is then open to interpretation.

Exported functions may be used to build programs of type 'Process' which
may be then inspected by an interpreter to compute the
effect of running the described program.
-}
module Join.Language
    (
    -- * User API
    -- ** Join Process's
    -- | Processes are the units of computation.
    --
    -- A Process is an independent execution of a sequence of Join
    -- 'Instruction's and IO actions and executes in isolation of all
    -- other running Processes. Communication between Processes is achieved
    -- by message passing over 'Channel's.
    --
    -- The 'Process' type is the core user-level type of this library and
    -- is the type that user programs are written in. 'Process' is
    -- a monadic type and so supports do notation in which it is
    -- recommended that programs are written.
    --
    -- Each 'Instruction' has a corresponding function which enters it into
    -- a 'Process' context. These are the atomic functions in which Join
    -- programs are built.
    --
    -- Monadically sequencing together processes to build larger
    -- computations says that each subprocess finishes execution before the next is interpreted.
    -- This is not always desired. Two primitive functions for controlling
    -- execution time are noted below:
    --
    -- - 'spawn' is provided to asynchronously run a Process, without
    --  waiting for a result.
    --
    -- - 'with' is provided to specify that two processes must be executed
    --   at the same time.
    --
    -- - 'withAll' specifies a list of processes to be executed at the
    --    same time.
    --
    -- For example programs, see "Join.Language.Examples"
      Process
    , spawn
    , with
    , withAll

    -- ** Channels and messages
    -- | Channels are the communication medium of the Join Calculus.
    -- The core calculus defines Channels as being asynchronously
    -- unidirectional and parameterised over a type of values that they
    -- carry.
    --
    -- In a 'Process' first a 'Channel' is created by a call to
    -- 'newChannel' as in:
    --
    -- @ c <- newChannel @
    --
    -- The type of message the Channel carries can usually be inferred from
    -- its usage, but must otherwise be annotated E.G.:
    --
    -- @c <- newChannel :: Process (Channel A Int)@
    --
    -- It may have been noticed that the 'Channel' type specifies a type
    -- parameter 'A'. This is because the Language has opted to define
    -- Channels as being of two varieties. The traditional asynchronous
    -- variety as defined by the Join calculus and an additional
    -- synchronous variety. The type parameter is either 'A' or 'S', denoting
    -- Asynchronous or Synchronous respectively.
    --
    -- Aynchronous Channel over messages of type t:
    --
    -- @ :: Channel A t @
    --
    -- Synchronous Channel over messages of type t, returning message of
    -- type r:
    -- 
    -- @ :: Channel (S r) t @
    --
    -- After a Channel has been defined, it may be sent messages is
    -- a number of distinct ways:
    --
    -- - 'send' is used to send a value on an asynchronous Channel, returning
    -- immediately with no return value.
    --
    -- - 'signal' is a convenience for 'send c ()' "signaling" the unit
    --   value to a 'Signal' channel ('Chan A ()').
    --
    -- - 'sync' is used to send a value to a synchronous Channel, returning
    -- immediately with a 'Response'. A reference to a reply value which can
    -- be 'wait'ed upon when the value is required.
    --
    -- - 'sync\'' is a variant of 'sync' which immediately blocks on a reply
    --   value.
    --
    -- - 'reply' is used to send a message in reply to a synchronous Channel.
    --
    -- - 'syncSignal' is a convenience for 'sync s ()' "signaling" the unit
    --   value to a 'SyncSignal' channel ('SyncChan () r').
    --
    -- - 'syncSignal\'' is a variant of 'syncSignal' which immediately
    --   blocks on a reply value.
    --
    -- - 'acknowledge' is a convenience for 'reply s ()' "acknowledging"
    --   a message sent on a synchronous channel by replying with the unit
    --   value.
    --
    --   Each of these functions also provide an 'all'-suffixed variant
    --   which runs the corresponding action on a list of arguments, in
    --   parallel via 'with' when possible.
    --
    -- / It is noted that the addition of synchronous /
    -- / Channels does not add to the Join-Calculus by virtue of the fact /
    -- / that they could otherwise be implemented by /
    -- / a continuation-passing-style on the primitive asynchronous /
    -- / Channels./.
    , newChannel , newChannels
    , send       , sendAll
    , signal     , signalAll
    , sync       , syncAll
    , wait       , waitAll
    , sync'      , syncAll'
    , syncSignal , syncSignalAll
    , syncSignal', syncSignalAll'
    , reply      , replyAll
    , acknowledge, acknowledgeAll

    -- ** Join definitions
    -- | Join definitions are the key construct provided by the Join-calculus
    -- and allow a declarative style of defining reactions to messages sent
    -- to channels.
    --
    -- On the left-hand-side (LHS) of a Join definition is a 'Pattern' to match
    -- upon. The pattern is either:
    --
    -- - A single Channel => Match all messages sent to the channel
    --
    -- - A single Channel &= value => Match messages sent on the Channel
    --   which are equal to the value.
    --
    -- - A conjunction of the previous two forms.
    --
    -- The operators '&' and '&=' may be used to build 'Patterns'.
    --
    -- E.G. Given:
    --
    -- @cc :: Channel A Char
    --
    --  ci :: Channel (S Integer) Int
    -- @
    --
    --      Some valid patterns are:
    -- 
    -- @cc
    --
    -- ci
    --
    -- cc & ci
    --
    -- cc & ci&=1
    -- @
    --
    -- On the right-hand-side of the Join definition is a trigger function, typed to accept
    -- each message defined on the LHS in order and result in a function in
    -- 'Process'.
    --
    -- The operator '|>' may be used to build 'Def' patterns in infix
    -- style.
    --
    -- E.G. Given the previous example patterns, valid definitions are:
    --
    -- @cc         |> (\char     -> undefined)
    --
    --  ci         |> (\int      -> undefined)
    --
    --  cc & ci    |> (\char int -> undefined)
    --
    --  cc & ci&=1 |> (\char int -> undefined)
    -- @
    --
    -- The semantics of a Join 'Def' are that when the LHS
    -- 'Pattern' matches, the corresponding messages are passed to the RHS
    -- trigger function which is executed asynchronously in the background.
    , def
    , (|>)
    , (&)
    , (&=)

    -- ** Convenience functions
    -- | 'Process' helper functions.
    , Inert
    , inert

    -- * Implementer API
    -- | Below is the base instruction type, along with typeclasses and
    -- functions which should only be required directly in the
    -- implementation of interpreters.
    , Instruction(..)
    , Definitions
    , Apply
    , apply
    ) where

import Prelude hiding (read)

import Join.Types
import Join.Types.Pattern.Rep

import Control.Monad.Operational (ProgramT,singleton)
import Control.Monad             (replicateM)
import Control.Monad.IO.Class    (liftIO)
import Data.Monoid

-- | Type of atomic Join instructions.
--
-- This is the underlying type of the 'Process' Monad which is the users
-- interface to writing Join programs.
--
-- For writing Join programs, see the corresponding 'Process' functions:
-- I.E. For 'Def' instruction, see 'def' function. Etc.
--
-- For writing interpreters of Join programs, more comprehensive documentation may be
-- found in the source (because haddock cannot currently document GADTs).
data Instruction a where

    -- Join definition.
    Def :: Definitions t tss Inert
        => t
        -> Instruction ()

    -- Request a new typed Channel.
    NewChannel :: (InferSync s, MessageType a) -- Synchronicity can be inferred, 'a' is a 'MessageType'.
               => Instruction (Channel s a)    -- Infer the required type of a new synchronous/ asynchronous Channel.

    -- Sends a value on a Channel.
    Send       :: MessageType a                -- Message type can be serialized.
               => Chan a                       -- Target Asynchronous Channel.
               -> a                            -- Value sent
               -> Instruction ()

    -- Asynchronously spawn a Process.
    Spawn      :: Process ()                  -- Process to spawn.
               -> Instruction ()

    -- Send a value on a Synchronous Channel and wait for a result.
    Sync       :: (MessageType a, MessageType r) -- Message type can be serialized.
               => SyncChan a r                   -- Channel sent and waited upon.
               -> a                              -- Value sent.
               -> Instruction (Response r)      -- Reply channel.

    -- Send a reply value on a Synchronous Channel.
    Reply      :: MessageType r                  -- Message type can be serialized.
               => SyncChan a r                 -- A Synchronous Channel to reply to.
               -> r                            -- Value to reply with.
               -> Instruction ()

    -- Concurrently execute two Process's.
    With       :: Process ()                  -- First process.
               -> Process ()                  -- Second process.
               -> Instruction ()


-- | Process is a Monadic type that can be thought of as representing a
-- sequence of Join 'Instructions'.
type Process a = ProgramT Instruction IO a

-- | Enter a single 'Def' Instruction into Process.
--
-- Declares that when a 'Pattern' p is matched, a trigger function t is to be called, passed the matching messages.
--
-- E.G. Increment:
--
-- @ def ci (\i -> reply ci (i+1)) @
--
-- Says that when ci (which may be inferred to have type :: Channel S Int)
-- receives a message, it is passed to the RHS function which increments it
-- and passes it back.
def :: Definitions t tss Inert => t -> Process ()
def p = singleton $ Def p

-- | Enter a single 'NewChannel' Instruction into Process.
--
-- Request a new typed Channel be created. Whether the
-- Channel is synchronous or asynchronous is determined by the calling
-- context.
newChannel :: (InferSync s,MessageType a) => Process (Channel s a)
newChannel = singleton NewChannel

-- | Request a given number of new typed Channels be created.
-- All Channels will have the same message type and synchronicity type.
-- Whether the Channels are synchronous or asynchronous is determined by
-- the calling context.
newChannels :: (InferSync s,MessageType a) => Int -> Process [Channel s a]
newChannels i = replicateM i newChannel

-- | Enter a single 'Send' Instruction into Process.
--
-- On a (regular) asynchronous 'Channel', send a message.
send :: MessageType a => Chan a -> a -> Process ()
send c a = singleton $ Send c a

-- | Simultaneously send messages to (regular) asynchronous 'Channel's.
sendAll :: MessageType a => [(Chan a,a)] -> Process ()
sendAll = withAll . map (uncurry send)

-- | Send an asynchronous signal.
signal :: Signal -> Process ()
signal c = send c ()

-- | Simultaneously send asynchronous signals.
signalAll :: [Signal] -> Process ()
signalAll = withAll . map signal

-- | Enter a single 'Spawn' Instruction into Process.
--
-- Asynchronously spawn a 'Process' () computation in the
-- background.
spawn :: Process () -> Process ()
spawn p = singleton $ Spawn p

-- | Enter a single 'Sync' Instruction into Process.

-- Send a message to a synchronous 'Channel', returning
-- a 'Response' - a handle to the reply message which may be 'wait'ed upon
-- when needed.
sync :: (MessageType a,MessageType r) => SyncChan a r -> a -> Process (Response r)
sync s a = singleton $ Sync s a

-- | Send messages to synchronous 'Channel's, returning a list
-- of 'Response's - handles to the reply messages which may be 'wait'ed upon
-- when needed.
syncAll :: (MessageType a,MessageType r) => [(SyncChan a r,a)] -> Process [Response r]
syncAll = mapM (uncurry sync)

-- | In a Process, block on a 'Response'.
wait :: Response r -> Process r
wait sv = return $! readResponse sv

-- | Block on many 'Response's.
waitAll :: [Response r] -> Process [r]
waitAll = mapM wait

-- | Send a message to a synchronous 'Channel', blocking on a reply value.
sync' :: (MessageType a,MessageType r) => SyncChan a r -> a -> Process r
sync' s a = sync s a >>= wait

-- | Send messages to synchronous 'Channel's, blocking on
-- the reply values.
syncAll' :: (MessageType a,MessageType r) => [(SyncChan a r,a)] -> Process [r]
syncAll' = mapM (uncurry sync')

-- | Send a synchronous signal, returning a 'Response' - a handle to the
-- reply message which may be 'wait'ed upon when needed.
syncSignal :: MessageType r => SyncSignal r -> Process (Response r)
syncSignal s = sync s ()

-- | Send synchronous signals returning a list of 'Response's - handles
-- to the reply messages which may be 'wait'ed upon when needed.
syncSignalAll :: MessageType r => [SyncSignal r] -> Process [Response r]
syncSignalAll = mapM syncSignal

-- | Send a synchronous signal, blocking on a reply value.
syncSignal' :: MessageType r => SyncSignal r -> Process r
syncSignal' s = syncSignal s >>= wait

-- | Send synchronous signals. blocking on the reply values.
syncSignalAll' :: MessageType r => [SyncSignal r] -> Process [r]
syncSignalAll' = mapM syncSignal'

-- | Enter a single 'Reply' Instruction into Process.
--
-- On a synchronous 'Channel', respond with a message to the
-- sender.
reply :: MessageType r => SyncChan a r -> r -> Process ()
reply s a = singleton $ Reply s a

-- | Simultaneously, respond with messages to synchronous 'Channels.
replyAll :: MessageType r => [(SyncChan a r,r)] -> Process ()
replyAll = withAll . map (uncurry reply)

-- | Reply with a synchronous acknowledgment.
acknowledge :: SyncChan a () -> Process ()
acknowledge s = reply s ()

-- | Simultaneously reply with synchronous acknowledgements.
acknowledgeAll :: [SyncChan a ()] -> Process ()
acknowledgeAll = withAll . map acknowledge

-- | Enter a single 'With' Instruction into Process.
--
-- Concurrently run two 'Process' () computations.
with :: Process () -> Process () -> Process ()
with p q = singleton $ With p q

instance Monoid Inert where
    mempty  = inert
    mappend = with

-- | Compose a list of 'Inert' 'Process's to be ran concurrently.
withAll :: [Inert] -> Inert
withAll = mconcat

-- | Type synonym for a Process which terminates without value.
type Inert = Process ()

-- | Synonym for:
--
-- @ return () :: Process () @
--
-- May be used to indicate the end of a process which returns no useful
-- value.
inert :: Inert
inert = return ()

