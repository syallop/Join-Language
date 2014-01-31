{-# LANGUAGE GADTs #-}
module Join.Language where

import Join.Language.Types

import Control.Monad.Operational (Program)

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
