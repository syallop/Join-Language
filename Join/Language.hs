{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Join.Language
    ( Instruction(..)
    , ProcessM
    , def
    , inert
    , newChannel
    , send
    , spawn
    , sync
    , reply
    , with

    , onReply

    , Pattern()
    , SubPattern()
    , rawSubPattern
    , rawPattern

    , (&)
    , (&=)
    , (|-)

    , Apply
    , apply
    ) where

import Join.Language.Types

import Control.Monad.Operational (ProgramT,singleton)

import Data.ByteString (ByteString)
import Data.Serialize

-- Single primitive instructions in a Join process.
data Instruction a where

    -- | Join definition.
    Def        :: (Apply t, Pattern p t)    -- Trigger can be applied,pattern is associated with trigger type.
               => p                         -- Pattern matched on.
               -> t                         -- Trigger function called on match.
               -> Instruction ()

    -- | Request a new typed Channel.
    NewChannel :: (InferSync s, Serialize a) -- Synchronicity can be inferred, message type can be serialized.
               => Instruction (Channel s a)  -- Infer the required type of a new synchronous/ asynchronous Channel.

    -- | Sends a value on a Channel.
    Send       :: Serialize a               -- Message type can be serialized.
               => Channel A a               -- Target Asynchronous Channel.
               -> a                         -- Value sent
               -> Instruction ()

    -- | Asynchronously spawn a Process.
    Spawn      :: ProcessM ()               -- Process to spawn.
               -> Instruction ()

    -- | Send a value on a Synchronous Channel and wait for a result.
    Sync       :: Serialize a               -- Message type can be serialized.
               => Channel S a               -- Channel sent and waited upon.
               -> a                         -- Value sent.
               -> Instruction (SyncVal a)   -- SyncVal encapsulated reply value.

    -- | Send a reply value on a Synchronous channel.
    Reply      :: Serialize a               -- Message type can be serialized.
               => Channel S a               -- A Synchronous Channel to reply to.
               -> a                         -- Value to reply with.
               -> Instruction ()

    -- | Concurrently execute two Process's.
    With       :: ProcessM ()               -- First process.
               -> ProcessM ()               -- Second process.
               -> Instruction ()

-- | Class of types which can be applied to a sequence of ByteString
-- parameters.
-- 'apply' is partial.
-- 
-- Only guaranteed to be safe when:
-- - The number of list items is exactly equal to the number of arguments
--   expected by f.
-- - Each argument to f is serializable.
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

-- | Class of Pattern types 'p' which uniquely determine a type 't',
-- a corresponding trigger function should have in order to take each value
-- type matched by the pattern and terminate with ProcessM ().
class Show p => Pattern p t | p -> t where
    rawPattern :: p -> [(Int,Maybe ByteString)]

-- | Class of SubPattern types 'p' which uniquely determine the type 't'
-- a corresponding trigger function should take as an arg when the pattern is
-- matched.
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

-- | Infix combine a SubPattern with a Pattern.
(&) :: (SubPattern p t, Pattern p' t') => p -> p' -> And (t -> t')
infixr 7 &
p & ps = And p ps

-- | Infix define a ChannelEq match.
(&=) :: Serialize a => Channel s a -> a -> ChannelEq a
infixr 8 &=
c &= v = ChannelEq c v

-- | Infix, enter a Def instruction into ProcessM.
(|-) :: (Apply t, Pattern p t) => p -> t -> ProcessM ()
infixr 7 |-
p |- t = def p t




-- | A single join Process. Implemented monadically over Instruction with the
-- Operational Monad.
type ProcessM a = ProgramT Instruction IO a

-- | Enter a single Def Instruction into ProcessM.
def :: (Apply t, Pattern p t) => p -> t -> ProcessM ()
def c p = singleton $ Def c p

-- | Enter a single Inert Instruction into ProcessM.
inert :: ProcessM ()
inert = return ()

-- | Enter a single NewChannel Instruction into ProcessM.
newChannel :: forall s a. (InferSync s,Serialize a) => ProcessM (Channel s a)
newChannel = singleton NewChannel

-- | Enter a single Send Instruction into ProcessM.
send :: forall a. Serialize a => Channel A a -> a -> ProcessM ()
send c a = singleton $ Send c a

-- | Enter a single Spawn Instruction into ProcessM.
spawn :: ProcessM () -> ProcessM ()
spawn p = singleton $ Spawn p

-- | Enter a single Sync Instruction into ProcessM.
sync :: Serialize a => Channel S a -> a -> ProcessM (SyncVal a)
sync s a = singleton $ Sync s a

-- | Enter a single Reply Instruction into ProcessM.
reply :: Serialize a => Channel S a -> a -> ProcessM ()
reply s a = singleton $ Reply s a

-- | Enter a single With Instruction into ProcessM.
with :: ProcessM () -> ProcessM () -> ProcessM ()
with p q = singleton $ With p q

-- | Helper for continuation style programming with Channels.
-- E.G., given:
--
-- do s <- newSyncChannel
--    def (on $ All s) (\x -> do liftIO $ print x
--                               reply s (x+1))
--    onReply s (liftIO . print)
--
-- sending an Int value on s will print it as well as it's successor.
onReply :: Serialize a => Channel A a -> (a -> ProcessM ()) -> ProcessM ()
onReply = def

