{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# LANGUAGE DataKinds #-}
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

    , DefPattern(..)
    , ChannelPattern(..)
    , on
    , (&)
    , (&|)

    , Apply
    , apply
    ) where

import Join.Language.Types

import Control.Monad.Operational (ProgramT,singleton)

import Data.ByteString (ByteString)
import Data.Serialize

-- | Single primitive instructions in a Join process.
data Instruction a where

    -- | Join definition.
    Def        :: Apply p
               => DefPattern p              -- ^ Pattern of Channels matched on.
               -> p                         -- ^ Trigger function called on match.
               -> Instruction ()

    -- | Request a new typed Channel.
    NewChannel :: (InferSync s, Serialize a)
               => Instruction (Channel s a) -- Infer the required type of a new synchronous/ asynchronous Channel.

    -- | Sends a value on a Channel.
    Send       :: Serialize a
               => Channel A a               -- ^ Target Asynchronous Channel.
               -> a                         -- ^ Value sent
               -> Instruction ()

    -- | Asynchronously spawn a Process.
    Spawn      :: ProcessM ()               -- ^ Process to spawn.
               -> Instruction ()

    -- | Send a value on a Synchronous Channel and wait for a result.
    Sync       :: Serialize a
               => Channel S a               -- ^ Channel sent and waited upon.
               -> a                         -- ^ Value sent.
               -> Instruction (SyncVal a)   -- ^ SyncVal encapsulated reply value.

    -- | Send a reply value on a Synchronous channel.
    Reply      :: Serialize a
               => Channel S a               -- ^ A Synchronous Channel to reply to.
               -> a                         -- ^ Value to reply with.
               -> Instruction ()

    -- | Concurrently execute two Process's.
    With       :: ProcessM ()               -- ^ First process.
               -> ProcessM ()               -- ^ Second process.
               -> Instruction ()


class Apply f where apply :: f -> [ByteString] -> ProcessM ()
instance Apply (ProcessM ()) where
    apply p [] = p
    apply _ _  = error "Remaining arguments"
instance (Serialize a, Apply r) => Apply (a -> r) where
    apply p (m:ms) = case decode m of
        Left _  -> error "Mistyped argument"
        Right v -> apply (p v) ms
    apply _ [] = error "Too few arguments"


-- | A pattern of one or many patterns on Channels. Type variable denotes
-- the type of a function accepting the type of each conjunctive Channel in
-- order and produces a ProcessM (). This type corresponds to the type of
-- the function in the RHS of a join definition.
--
-- For convenience, a DefPattern may be built using 'on' and infix
-- functions '&' and '&|'.
-- E.G. Matching on (a, b, c, ..., n, m) == (a & b & c & ...n &| m)
data DefPattern p where
    LastPattern :: Serialize a => ChannelPattern a -> DefPattern (a -> ProcessM ())
    AndPattern  :: Serialize a => ChannelPattern a -> DefPattern b -> DefPattern (a -> b)

{-data ChannelPattern c a where-}
    {-All  :: c a -> ChannelPattern c a-}
    {-Only :: Eq a => c a -> a -> ChannelPattern c a-}

data ChannelPattern a where
    All :: Channel t a -> ChannelPattern a
    Only :: Eq a => Channel t a -> a -> ChannelPattern a


instance Show (DefPattern p) where
    show (LastPattern (All c))    = show c
    show (LastPattern (Only c _)) = show c ++ "=value"

    show (AndPattern (All c) d) = show c ++ " & " ++ show d
    show (AndPattern (Only c _) d) = show c ++ "=value" ++ " & " ++ show d

-- | Infix, cons a Channel to a ChannelPattern.
(&) :: Serialize a => ChannelPattern a -> DefPattern p -> DefPattern (a -> p)
p & ps = p `AndPattern` ps
infixr 7 &

-- | Infix, cons a Channel to a final Channel of a ChannelPattern.
(&|) :: (Serialize a, Serialize b) => ChannelPattern a -> ChannelPattern b -> DefPattern (a -> b -> ProcessM ())
c &| d = c & LastPattern d

-- | 'Promote' a single Channel to a ChannelPattern.
on :: Serialize a => ChannelPattern a -> DefPattern (a -> ProcessM ())
on = LastPattern




-- | A single join Process. Implemented monadically over Instruction with the
-- Operational Monad.
type ProcessM a = ProgramT Instruction IO a

-- | Enter a single Def Instruction into ProcessM.
def :: forall p. Apply p => DefPattern p -> p -> ProcessM ()
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
onReply c = def (on $ All c)

