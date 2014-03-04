{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

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
    -- Synchronous Channel over messages of type t:
    -- 
    -- @ :: Channel S t @
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

    -- ** Join patterns
    -- | Join patterns are the key construct provided by the Join-calculus
    -- and allow a declarative style of defining reactions to messages sent
    -- to channels.
    --
    -- On the left-hand-side (LHS) of a Join pattern is a 'Pattern' to match
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
    --  ci :: Channel S Int
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
    -- On the right-hand-side of the Join pattern is a trigger function, typed to accept
    -- each message defined on the LHS in order and result in a function in
    -- 'ProcessM'.
    --
    -- The operator '|-' may be used to build 'Def' patterns in infix
    -- style.
    --
    -- E.G. Given the previous example patterns, valid definitions are:
    --
    -- @cc         |- (\char     -> undefined)
    --
    --  ci         |- (\int      -> undefined)
    --
    --  cc & ci    |- (\char int -> undefined)
    --
    --  cc & ci&=1 |- (\char int -> undefined)
    -- @
    --
    -- The semantics of a Join 'Def' pattern are that when the LHS
    -- 'Pattern' matches, the corresponding messages are passed to the RHS
    -- trigger function which is executed asynchronously in the background.
    , def
    , (|-)
    , (&)
    , (&=)

    -- ** Convenience functions
    -- | Composite helper functions in 'ProcessM'.
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

import Join.Language.Types

import Control.Monad.Operational (ProgramT,singleton)

import Data.ByteString (ByteString)
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
    Def        :: (Apply t, Pattern p t)    -- Trigger can be applied,pattern is associated with trigger type.
               => p                         -- Pattern matched on.
               -> t                         -- Trigger function called on match.
               -> Instruction ()

    -- Request a new typed Channel.
    NewChannel :: (InferSync s, Serialize a) -- Synchronicity can be inferred, message type can be serialized.
               => Instruction (Channel s a)  -- Infer the required type of a new synchronous/ asynchronous Channel.

    -- Sends a value on a Channel.
    Send       :: Serialize a               -- Message type can be serialized.
               => Channel A a               -- Target Asynchronous Channel.
               -> a                         -- Value sent
               -> Instruction ()

    -- Asynchronously spawn a Process.
    Spawn      :: ProcessM ()               -- Process to spawn.
               -> Instruction ()

    -- Send a value on a Synchronous Channel and wait for a result.
    Sync       :: Serialize a               -- Message type can be serialized.
               => Channel S a               -- Channel sent and waited upon.
               -> a                         -- Value sent.
               -> Instruction (SyncVal a)   -- SyncVal encapsulated reply value.

    -- Send a reply value on a Synchronous channel.
    Reply      :: Serialize a               -- Message type can be serialized.
               => Channel S a               -- A Synchronous Channel to reply to.
               -> a                         -- Value to reply with.
               -> Instruction ()

    -- Concurrently execute two Process's.
    With       :: ProcessM ()               -- First process.
               -> ProcessM ()               -- Second process.
               -> Instruction ()

-- | Class of types which can be applied to a sequence of ByteString
-- parameters.
-- 'apply' is partial.
-- 
-- Only guaranteed to be safe when:
--
-- - The number of list items is exactly equal to the number of arguments
--   expected by f.
--
-- - Each argument to f is serializable.
--
-- - Each item is a serialized encoding of the corresponding expected type.
--
-- May be 'safely' used in interpreters to run the Def trigger function on
-- messages that match the corresponding pattern.
-- I.E. 'Pattern p t' says that when a sequence of messages match the
-- pattern p, then the a function of type t may be applied to them in
-- a 'safe' manner.
class Apply f where apply :: f -> [ByteString] -> ProcessM ()
instance Apply (ProcessM ()) where
    apply p [] = p
    apply _ _  = error "Remaining arguments"
instance (Serialize a, Apply r) => Apply (a -> r) where
    apply p (m:ms) = case decode m of
        Left _  -> error "Mistyped argument"
        Right v -> apply (p v) ms
    apply _ [] = error "Too few arguments"



-- | Require that messages sent on a Channel must match a specific value to
-- trigger a match.
data ChannelEq a = forall s. Serialize a => ChannelEq (Channel s a) a
instance Show (ChannelEq a) where show (ChannelEq c a) = show c ++ "&=" ++ show (encode a)

-- | Class of types 'p' which may appear as a pattern in a 'def', where 't'
-- gives the type the corresponding trigger function must have in order to
-- properly consume a pattern match and produce a 'ProcessM ()' result.
class Show p => Pattern p t | p -> t where
    rawPattern :: p -> [(Int,Maybe ByteString)]

-- | Class of types 'p' which may appear as a subpattern, as a smaller
-- component within a pattern in a 'def', where 't' gives the type
-- a corresponding trigger function must take in order to properly consume
-- a subpattern match.
class Show p => SubPattern p t | p -> t where
    rawSubPattern :: p -> (Int,Maybe ByteString)

-- | Type of conjunctive patterns. Conjoins a SubPattern to a Pattern
-- specifying both must match in order for the whole pattern to match.
data And p where And :: (SubPattern t p, Pattern t' p') => t -> t' -> And (p -> p')
instance Show (And p) where show (And p ps) = show p ++ "&" ++ show ps

instance Pattern (And p) p where rawPattern (And p ps) = rawSubPattern p : rawPattern ps

-- Channels and ChannelEq's may be used as part of a subpattern or as
-- a pattern in themselves.
instance SubPattern (Channel s a) a                  where rawSubPattern c               = (getId c, Nothing)
instance SubPattern (ChannelEq a) a                  where rawSubPattern (ChannelEq c a) = (getId c, Just $ encode a)
instance Pattern    (Channel s a) (a -> ProcessM ()) where rawPattern    c               = [rawSubPattern c]
instance Pattern    (ChannelEq a) (a -> ProcessM ()) where rawPattern    c               = [rawSubPattern c]

-- | Infix combine a smaller SubPattern with a Pattern.
--
-- Right associative, meaning:
--
-- @ c1 & (c2 & c3) @
--
-- May be written as:
--
-- @ c1 & c2 & c3 @
--
-- As all subpatterns are also valid patterns, the operator may be thought
-- of as a way to combine subpatterns.
--
-- Because all subpatterns are also valid patterns, the operator may be
-- thought of as a method for conjoining subpatterns.
(&) :: (SubPattern p t, Pattern p' t') => p -> p' -> And (t -> t')
infixr 7 &
p & ps = And p ps

-- | Infix define a ChannelEq match.
--
-- Right associative and with a greater precedence than '&'.
-- This means:
--
-- @ c1 & (c2&=1) @
--
-- Can be written as:
--
-- @ c1 & c2&=1 @
(&=) :: Serialize a => Channel s a -> a -> ChannelEq a
infixr 8 &=
c &= v = ChannelEq c v

-- | Infix, enter a Def instruction into ProcessM.
--
-- I.E. allows:
--
-- @ def ci (\i -> reply ci (i+1)) @
--
-- To be written:
--
-- @ ci |- (\i -> reply ci (i+1)) @
(|-) :: (Apply t, Pattern p t) => p -> t -> ProcessM ()
infixr 7 |-
p |- t = def p t



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
def :: (Apply t, Pattern p t) => p -> t -> ProcessM ()
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
sync :: Serialize a => Channel S a -> a -> ProcessM (SyncVal a)
sync s a = singleton $ Sync s a

-- | Enter a single 'Reply' Instruction into ProcessM.
--
-- On a synchronous 'Channel', respond with a message to the
-- sender.
reply :: Serialize a => Channel S a -> a -> ProcessM ()
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
--    s |- (\x -> do liftIO $ print x
--                  reply s (x+1))
--    onReply s (liftIO . print)
-- @
-- 
--
-- sending an Int value on s will print it as well as it's successor.
onReply :: Serialize a => Channel A a -> (a -> ProcessM ()) -> ProcessM ()
onReply = def

-- | Synonym for:
--
-- @ return () :: ProcessM () @
--
-- May be used to indicate the end of a process which returns no useful
-- value.
inert :: ProcessM ()
inert = return ()

