{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_HADDOCK prune #-}
{-|
Module      : Join.Language
Copyright   : (c) Samuel A. Yallop, 2014
Maintainer  : syallop@gmail.com
Stability   : experimental

This module describes an encoding of the Join-Calculus.

It defines methods for writing Join-Calculus programs which build a type
'ProcessM' which is then open to interpretation.

Exported functions may be used to build programs of type 'ProcessM' which
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
    -- The 'ProcessM' type is the core user-level type of this library and
    -- is the type that user programs are written in. 'ProcessM' is
    -- a monadic type and so supports do notation in which it is
    -- recommended that programs are written.
    --
    -- Each 'Instruction' has a corresponding function which enters it into
    -- a 'ProcessM' context. These are the atomic functions in which Join
    -- programs are built.
    --
    -- Monadically sequencing together processes to build larger
    -- computations says that each subprocess finishes execution before the next is interpreted.
    -- This is not always desired. Two primitive functions for controlling
    -- execution time are noted below:
    --
    -- - 'spawn' is provided to asynchronously run a ProcessM, without
    --  waiting for a result.
    --
    -- - 'with' is provided to specify that two processes must be executed
    --   at the same time.
    --
    -- For example programs, see "Join.Language.Examples"
      ProcessM
    , spawn
    , with

    -- ** Channels and messages
    -- | Channels are the communication medium of the Join Calculus.
    -- The core calculus defines Channels as being asynchronously
    -- unidirectional and parameterised over a type of values that they
    -- carry.
    --
    -- In a 'ProcessM' first a 'Channel' is created by a call to
    -- 'newChannel' as in:
    --
    -- @ c <- newChannel @
    --
    -- The type of message the Channel carries can usually be inferred from
    -- its usage, but must otherwise be annotated E.G.:
    --
    -- @c <- newChannel :: ProcessM (Channel A Int)@
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
    -- - 'send' is used to value on an asynchronous Channel, returning
    -- immediately with no return value.
    --
    -- - 'sync' is used to send a value to a synchronous Channel, returning
    -- immediately with a 'SyncVal'. A reference to a reply value which can
    -- be 'wait'ed upon when the value is required.
    --
    -- - 'reply' is used to send a message in reply to a synchronous Channel.
    --
    -- / It is noted that the addition of synchronous /
    -- / Channels does not add to the Join-Calculus by virtue of the fact /
    -- / that they could otherwise be implemented by /
    -- / a continuation-passing-style on the primitive asynchronous /
    -- / Channels./.
    , newChannel
    , send
    , sync
    , reply

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
    -- 'ProcessM'.
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
    -- | 'ProcessM' helper functions.
    , Inert
    , inert
    , onReply

    -- * Implementer API
    -- | Below is the base instruction type, along with typeclasses and
    -- functions which should only be required directly in the
    -- implementation of interpreters.
    , Instruction(..)
    , Pattern(..)
    , SubPattern(..)
    , Apply
    , apply
    ) where

import Join.Types

import Control.Monad.Operational (ProgramT,singleton)

import Data.Serialize

-- | Type of atomic Join instructions.
--
-- This is the underlying type of the 'ProcessM' Monad which is the users
-- interface to writing Join programs.
--
-- For writing Join programs, see the corresponding 'ProcessM' functions:
-- I.E. For 'Def' instruction, see 'def' function. Etc.
--
-- For writing interpreters of Join programs, more comprehensive documentation may be
-- found in the source (because haddock cannot currently document GADTs).
data Instruction a where

    -- Join definition.
    Def        :: (Apply t Inert, Pattern p t) -- Trigger can be applied,pattern is associated with trigger type.
               => p                            -- Pattern matched on.
               -> t                            -- Trigger function called on match.
               -> Instruction ()

    -- Request a new typed Channel.
    NewChannel :: (InferSync s, Serialize a)   -- Synchronicity can be inferred, message type can be serialized.
               => Instruction (Channel s a)    -- Infer the required type of a new synchronous/ asynchronous Channel.

    -- Sends a value on a Channel.
    Send       :: Serialize a                  -- Message type can be serialized.
               => Channel A a                  -- Target Asynchronous Channel.
               -> a                            -- Value sent
               -> Instruction ()

    -- Asynchronously spawn a Process.
    Spawn      :: ProcessM ()                  -- Process to spawn.
               -> Instruction ()

    -- Send a value on a Synchronous Channel and wait for a result.
    Sync       :: (Serialize a, Serialize r)   -- Message type can be serialized.
               => Channel (S r) a              -- Channel sent and waited upon.
               -> a                            -- Value sent.
               -> Instruction (SyncVal r)      -- SyncVal encapsulated reply value.

    -- Send a reply value on a Synchronous Channel.
    Reply      :: Serialize r                  -- Message type can be serialized.
               => Channel (S r) a              -- A Synchronous Channel to reply to.
               -> r                            -- Value to reply with.
               -> Instruction ()

    -- Concurrently execute two Process's.
    With       :: ProcessM ()                  -- First process.
               -> ProcessM ()                  -- Second process.
               -> Instruction ()


-- | Infix, enter a Def instruction into ProcessM.
--
-- I.E. allows:
--
-- @ def ci (\i -> reply ci (i+1)) @
--
-- To be written:
--
-- @ ci |> (\i -> reply ci (i+1)) @
(|>) :: (Apply t Inert, Pattern p t) => p -> t -> ProcessM ()
infixr 7 |>
p |> t = def p t



-- | ProcessM is a Monadic type that can be thought of as representing a
-- sequence of Join 'Instructions'.
type ProcessM a = ProgramT Instruction IO a

-- | Enter a single 'Def' Instruction into ProcessM.
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
def :: (Apply t Inert, Pattern p t) => p -> t -> ProcessM ()
def c p = singleton $ Def c p

-- | Enter a single 'NewChannel' Instruction into ProcessM.
--
-- Request a new typed Channel be created. Whether the
-- Channel is synchronous or asynchronous is determined by the calling
-- context.
newChannel :: forall s a. (InferSync s,Serialize a) => ProcessM (Channel s a)
newChannel = singleton NewChannel

-- | Enter a single 'Send' Instruction into ProcessM.
--
-- On a (regular) asynchronous 'Channel', send a message.
send :: forall a. Serialize a => Channel A a -> a -> ProcessM ()
send c a = singleton $ Send c a

-- | Enter a single 'Spawn' Instruction into ProcessM.
--
-- Asynchronously spawn a 'ProcessM' () computation in the
-- background.
spawn :: ProcessM () -> ProcessM ()
spawn p = singleton $ Spawn p

-- | Enter a single 'Sync' Instruction into ProcessM.

-- Send a message to a synchronous 'Channel', returning
-- a 'SyncVal' - a handle to the reply message which may be 'wait'ed upon
-- when needed.
sync :: (Serialize a,Serialize r) => Channel (S r) a -> a -> ProcessM (SyncVal r)
sync s a = singleton $ Sync s a

-- | Enter a single 'Reply' Instruction into ProcessM.
--
-- On a synchronous 'Channel', respond with a message to the
-- sender.
reply :: Serialize r => Channel (S r) a -> r -> ProcessM ()
reply s a = singleton $ Reply s a

-- | Enter a single 'With' Instruction into ProcessM.
--
-- Concurrently run two 'ProcessM' () computations.
with :: ProcessM () -> ProcessM () -> ProcessM ()
with p q = singleton $ With p q

-- | Helper for continuation style programming with Channels.
-- E.G., given:
-- 
-- @
-- do s <- newChannel
--    s |> (\x -> do liftIO $ print x
--                  reply s (x+1))
--    onReply s (liftIO . print)
-- @
-- 
--
-- sending an Int value on s will print it as well as it's successor.
onReply :: Serialize a => Channel A a -> (a -> ProcessM ()) -> ProcessM ()
onReply = def

-- | Type synonym for a ProcessM which terminates without value.
type Inert = ProcessM ()

-- | Synonym for:
--
-- @ return () :: ProcessM () @
--
-- May be used to indicate the end of a process which returns no useful
-- value.
inert :: Inert
inert = return ()

