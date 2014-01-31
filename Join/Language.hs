{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Join.Language
    ( Instruction(..)
    , ProcessM
    , def
    , inert
    , newChannel
    , spawn
    , with
    ) where

import Join.Language.Types

import Control.Monad.Operational (Program,singleton)

-- | Single primitive instructions in a join process.
-- Constructors vaguely commented with intended semantics - how the
-- instruction should be interpreted/ whether it should be synchronous,
-- etc.
data Instruction a where

    -- | Join definition.
    Def        :: Channels a              -- ^ Channels matched for.
               -> (a -> ProcessM ())      -- ^ Handler function called.
               -> Instruction ()          -- ^ No result, asynchronous.

    -- | End process.
    Inert      :: Instruction ()          -- ^ No result, end interpretation of ProcessM.

    -- | Request a new typed Channel.
    NewChannel :: Instruction (Channel a) -- ^ Result in typed Channel, synchronous.

    -- | Spawn a value on a Channel.
    Spawn      :: Channel a               -- ^ Channel spawned on.
               -> a                       -- ^ Value spawned.
               -> Instruction ()          -- ^ No result, asynchronous.

    -- | Concurrently execute two Process's.
    With       :: ProcessM ()             -- ^ First process.
               -> ProcessM ()             -- ^ Second process.
               -> Instruction ()          -- ^ No result, asynchronous execution of both Processes.

-- | A single join Process. Implemented monadically over Instruction with the
-- Operational Monad.
type ProcessM a = Program Instruction a

-- | Enter a single Def Instruction into ProcessM.
def :: forall a. Channels a -> (a -> ProcessM ()) -> ProcessM ()
def c p = singleton $ Def c p

-- | Enter a single Inert Instruction into ProcessM.
inert :: ProcessM ()
inert = singleton Inert

-- | Enter a single NewChannel Instruction into ProcessM.
newChannel :: forall a. ProcessM (Channel a)
newChannel = singleton NewChannel

-- | Enter a single Spawn Instruction into ProcessM.
spawn :: forall a. Channel a -> a -> ProcessM ()
spawn c a = singleton $ Spawn c a

-- | Enter a single With Instruction into ProcessM.
with :: ProcessM () -> ProcessM () -> ProcessM ()
with p q = singleton $ With p q
