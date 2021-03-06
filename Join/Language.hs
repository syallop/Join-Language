{-# LANGUAGE
    ConstraintKinds
  , DataKinds
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , KindSignatures
  , MultiParamTypeClasses
  , RankNTypes
  , TemplateHaskell
  , TypeOperators
  , TypeSynonymInstances
  #-}

{-# OPTIONS_HADDOCK prune #-}
{-|
Module      : Join.Language
Copyright   : (c) Samuel A. Yallop, 2014
Maintainer  : syallop@gmail.com
Stability   : experimental

This module encodes the core instructions of the Join-Calculus as a
"DSL-Compose" DSL.

It defines methods for writing Join-Calculus programs which may then be interpreted
by a compatible "DSL-Compose" interpreter.

Exported functions may be used to build Join-Calculus programs which may then
be inspected by a compatible "DSL-Compose" interpreter to compute the effect of execution.
-}
module Join.Language
    (
    -- * User API
    -- ** Join Process's
    -- | Processes are the units of computation.
    --
    -- A join program is a concurrent execution of a sequence of 'CoreInst' instructions
    -- (which are the core instructions only, excluding extra functionality).
    -- Communication between programs is achieved by message passing over 'Channel's.
    --
    -- 'Process' and 'ProcessIn' are the main user-level types of this module and are used to
    -- build join programs.
    -- 'Process' is a subset of 'ProcessIn' which only permits core instructions to be used.
    -- 'ProcessIn' exposes a type variable describing other "DSL-Compose" compatible instruction
    -- types which it may be interleaved with. In particular, this might be used with the
    -- "Join.Language.Distributed" instructions to allow channels to be shared across running instances.
    -- To ignore this feature or just to restrict programs to CoreInst instructions only:
    -- E.G. given:
    --
    -- @ let generalProcess :: ProcessIn i a @
    --
    -- then:
    --
    -- @ let simpleProcess = generalProcess : Process a @
    --
    -- Each instruction has a corresponding function which enters it into
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
    , ProcessIn
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
    , send       , sendAll        , sendN
    , signal     , signalAll      , signalN
    , sync       , syncAll        , syncN
    , wait       , waitAll
    , sync'      , syncAll'       , syncN'
    , syncSignal , syncSignalAll  , syncSignalN
    , syncSignal', syncSignalAll' , syncSignalN'
    , reply      , replyAll
    , acknowledge, acknowledgeAll

    , ioAction

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
    -- @
    --   cc :: Channel A Char
    --
    --   ci :: Channel (S Integer) Int
    -- @
    --
    --      Some valid patterns are:
    --
    -- @
    --   cc
    --
    --   ci
    --
    --   cc & ci
    --
    --   cc & ci&=1
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
    -- @
    --   cc         |> (\char     -> undefined)
    --
    --   ci         |> (\int      -> undefined)
    --
    --   cc & ci    |> (\char int -> undefined)
    --
    --   cc & ci&=1 |> (\char int -> undefined)
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
    , inert

    -- * Implementer API
    -- | Below is the base instruction type, along with typeclasses and
    -- functions which should only be required directly in the
    -- implementation of interpreters.
    , CoreInst(..)
    , Definitions
    , Apply
    , apply
    ) where

import Prelude hiding (read)

import Join.Apply
import Join.Channel
import Join.Message
import Join.Pattern
import Join.Pattern.Rep
import Join.Response

import Control.Monad             (replicateM)
import Data.Monoid

import DSL.Instruction
import DSL.Program

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
data CoreInst (p :: * -> *) (a :: *) where

    -- Join definition.
    Def
      :: ToDefinitions d tss (p ())
      => d
      -> CoreInst p ()

    -- Request a new typed Channel.
    NewChannel
      :: InferChannel s a              -- Synchronicity can be inferred, 'a' is a 'MessageType'.
      => CoreInst p (Channel s a)      -- Infer the required type of a new synchronous/ asynchronous Channel.

    -- Sends a value on a Channel.
    Send
      :: MessageType a
      => Chan a                        -- Target Asynchronous Channel.
      -> a                             -- Value sent
      -> CoreInst p ()

    -- Asynchronously spawn a Process.
    Spawn
      :: p ()                          -- Process to spawn.
      -> CoreInst p ()

    -- Send a value on a Synchronous Channel and wait for a result.
    Sync
      :: (MessageType a,MessageType r)
      => SyncChan a r                  -- Channel sent and waited upon.
      -> a                             -- Value sent.
      -> CoreInst p (Response r)       -- Reply channel.

    -- Send a reply value on a Synchronous Channel.
    Reply
      :: MessageType r
      => SyncChan a r                  -- A Synchronous Channel to reply to.
      -> r                             -- Value to reply with.
      -> CoreInst p ()

    -- Concurrently execute two Process's.
    With
      :: p ()                          -- First process.
      -> p ()                          -- Second process.
      -> CoreInst p ()

    -- Embed an IO action to be executed synchronously.
    IOAction
      :: IO a                          -- Embedded IO action.
      -> CoreInst p a

-- | 'Process' is a Monadic type that can be thought of as representing a sequence of core join
-- instructions only. This is in comparison to the more general 'ProcessIn' type which allows composition
-- with other "DSL-Compose" compatible instruction types.
type Process a = Program CoreInst a

-- | 'ProcessIn' is a Monadic type that can be thought of as representing a sequence of "DSL-Compose"
-- compatible instructions, one of which must be the core join instructions 'CoreInst'.
-- This is in comparison to the less general 'Process' type which only allows core instructions to be used.
type ProcessIn i a = (CoreInst :<- i) => Program i a

-- | Synonym for:
--
-- @ return () :: Process () @
--
-- May be used to indicate the end of a process which returns no useful
-- value.
inert :: ProcessIn i ()
inert = return ()

-- | Enter a single 'Def' instruction into a compatible Program.
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
def :: ToDefinitions d tss (Program i ()) => d -> ProcessIn i ()
def p = inject $ Def p

-- | Enter a single 'NewChannel' instruction into a compatible Program.
--
-- Request a new typed Channel be created. Whether the
-- Channel is synchronous or asynchronous is determined by the calling
-- context.
newChannel :: InferChannel s a => ProcessIn i (Channel s a)
newChannel = inject NewChannel

-- | Request a given number of new typed Channels be created.
-- All Channels will have the same message type and synchronicity type.
-- Whether the Channels are synchronous or asynchronous is determined by
-- the calling context.
newChannels :: InferChannel s a => Int -> ProcessIn i [Channel s a]
newChannels i = replicateM i newChannel

-- | Enter a single 'Send' instruction into a compatible Program.
--
-- On a (regular) asynchronous 'Channel', send a message.
send :: MessageType a => Chan a -> a -> ProcessIn i ()
send c a = inject $ Send c a

-- | Simultaneously send messages to (regular) asynchronous 'Channel's.
sendAll :: MessageType a => [(Chan a,a)] -> ProcessIn i ()
sendAll = withAll . map (uncurry send)

-- | Send a number of identical messages to a Channel.
sendN :: MessageType a => Int -> a -> Chan a -> ProcessIn i ()
sendN i msg chan = sendAll $ replicate i (chan,msg)

-- | Send an asynchronous signal.
signal :: Signal -> ProcessIn i ()
signal c = send c ()

-- | Simultaneously send asynchronous signals.
signalAll :: [Signal] -> ProcessIn i ()
signalAll = withAll . map signal

-- | Send a number of signals to the same 'Signal'
signalN :: Int -> Signal -> ProcessIn i ()
signalN i s = signalAll $ replicate i s

-- | Enter a single 'Spawn' instruction into a compatible Program.
--
-- Asynchronously spawn a 'Process' () computation in the
-- background.
spawn :: Program i () -> ProcessIn i ()
spawn p = inject $ Spawn p

-- | Enter a single 'Sync' instruction into a compatible Program.

-- Send a message to a synchronous 'Channel', returning
-- a 'Response' - a handle to the reply message which may be 'wait'ed upon
-- when needed.
sync :: (MessageType a,MessageType r) => SyncChan a r -> a -> ProcessIn i (Response r)
sync s a = inject $ Sync s a

-- | Send messages to synchronous 'Channel's, returning a list
-- of 'Response's - handles to the reply messages which may be 'wait'ed upon
-- when needed.
syncAll :: (MessageType a,MessageType r) => [(SyncChan a r,a)] -> ProcessIn i [Response r]
syncAll = mapM (uncurry sync)

-- | Send a number of synchronous messages to the same Channel.
syncN :: (MessageType a,MessageType r) => Int -> SyncChan a r -> a -> ProcessIn i [Response r]
syncN i s a = syncAll $ replicate i (s,a)

-- | In a Process, block on a 'Response'.
wait :: Response r -> ProcessIn i r
wait sv = return $! readResponse sv

-- | Block on many 'Response's.
waitAll :: [Response r] -> ProcessIn i [r]
waitAll = mapM wait

-- | Send a message to a synchronous 'Channel', blocking on a reply value.
sync' :: (MessageType a,MessageType r) => SyncChan a r -> a -> ProcessIn i r
sync' s a = sync s a >>= wait

-- | Send messages to synchronous 'Channel's, blocking on
-- the reply values.
syncAll' :: (MessageType a,MessageType r) => [(SyncChan a r,a)] -> ProcessIn i [r]
syncAll' = mapM (uncurry sync')

-- | Send a number of synchronous messages to a 'Channel' blocking on all reply values.
syncN' :: (MessageType a,MessageType r) => Int -> SyncChan a r -> a -> ProcessIn i [r]
syncN' i s a = syncAll' $ replicate i (s,a)

-- | Send a synchronous signal, returning a 'Response' - a handle to the
-- reply message which may be 'wait'ed upon when needed.
syncSignal :: MessageType r => SyncSignal r -> ProcessIn i (Response r)
syncSignal s = sync s ()

-- | Send synchronous signals returning a list of 'Response's - handles
-- to the reply messages which may be 'wait'ed upon when needed.
syncSignalAll :: MessageType r => [SyncSignal r] -> ProcessIn i [Response r]
syncSignalAll = mapM syncSignal

syncSignalN :: MessageType r => Int -> SyncSignal r -> ProcessIn i [Response r]
syncSignalN i s = syncSignalAll $ replicate i s

-- | Send a synchronous signal, blocking on a reply value.
syncSignal' :: MessageType r => SyncSignal r -> ProcessIn i r
syncSignal' s = syncSignal s >>= wait

-- | Send synchronous signals. blocking on the reply values.
syncSignalAll' :: MessageType r => [SyncSignal r] -> ProcessIn i [r]
syncSignalAll' = mapM syncSignal'

syncSignalN' :: MessageType r => Int -> SyncSignal r -> ProcessIn i [r]
syncSignalN' i s = syncSignalAll' $ replicate i s

-- | Enter a single 'Reply' instruction into a compatible Program.
--
-- On a synchronous 'Channel', respond with a message to the
-- sender.
reply :: MessageType r => SyncChan a r -> r -> ProcessIn i ()
reply s a = inject $ Reply s a

-- | Simultaneously, respond with messages to synchronous 'Channels.
replyAll :: MessageType r => [(SyncChan a r,r)] -> ProcessIn i ()
replyAll = withAll . map (uncurry reply)

-- | Reply with a synchronous acknowledgment.
acknowledge :: SyncChan a () -> ProcessIn i ()
acknowledge s = reply s ()

-- | Simultaneously reply with synchronous acknowledgements.
acknowledgeAll :: [SyncChan a ()] -> ProcessIn i ()
acknowledgeAll = withAll . map acknowledge

-- | Enter a single 'With' instruction into a compatible Program.
--
-- Concurrently run two 'Process' () computations.
with :: Program i () -> Program i () -> ProcessIn i ()
with p q = inject $ With p q

-- | Enter a single 'IOAction' instruction into a compatible Program.
--
-- Embed an IO action to be executed synchronously.
ioAction :: IO a -> ProcessIn i a
ioAction io = inject $ IOAction io

instance (CoreInst :<- i) => Semigroup (Program i ()) where
  (<>) = with

instance (CoreInst :<- i) => Monoid (Program i ()) where
    mempty  = inert

-- | Compose a list of 'Inert' 'Process's to be ran concurrently.
withAll :: [Program i ()] -> ProcessIn i ()
withAll = mconcat

