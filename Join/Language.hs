{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Join.Language
    ( Instruction(..)
    , ProcessM
    , def
    , inert
    , newChannel
    , send
    , spawn
    , with

    , newSyncChannel
    , reply

    , Interpretation(..)
    , interpret

    , DefPattern(..)
    , ChannelPattern(..)
    , on
    , (&)
    , (&|)
    ) where

import Join.Language.Types

import Control.Monad.IO.Class
import Control.Monad.Operational (ProgramT,ProgramViewT(..),singleton,viewT)


-- | Single primitive instructions in a join process.
-- Constructors vaguely commented with intended semantics - how the
-- instruction should be interpreted/ whether it should be synchronous,
-- etc.
data Instruction a where

    -- | Join definition.
    Def        :: DefPattern p            -- ^ Channels matched for.
               -> p                       -- ^ Handler function called.
               -> Instruction ()          -- ^ No result, asynchronous.

    -- | End process.
    Inert      :: Instruction ()          -- ^ No result, end interpretation of ProcessM.

    -- | Request a new typed Channel.
    NewChannel :: Instruction (Channel a) -- ^ Result in typed Channel, synchronous.

    -- | Sens a value to a Channel.
    Send       :: ChannelLike c a
               => c a                    -- ^ Target channel.
               -> a                      -- ^ Value sent
               -> Instruction ()         -- ^ No result, asynchronous.

    -- | Asynchronously spawn a Process.
    Spawn :: ProcessM ()                 -- ^ Process to spawn.
          -> Instruction ()

    -- | Concurrently execute two Process's.
    With       :: ProcessM ()             -- ^ First process.
               -> ProcessM ()             -- ^ Second process.
               -> Instruction ()          -- ^ No result, asynchronous execution of both Processes.


-- | A pattern of one or many patterns on Channels. Type variable denotes
-- the type of a function accepting the type of each conjunctive Channel in
-- order and produces a ProcessM (). This type corresponds to the type of
-- the function in the RHS of a join definition.
--
-- For convenience, a DefPattern may be built using 'on' and infix
-- functions '&' and '&|'.
-- E.G. Matching on (a, b, c, ..., n, m) == (a & b & c & ...n &| m)
data DefPattern p where
    LastPattern :: ChannelLike c a => ChannelPattern c a -> DefPattern (a -> ProcessM ())
    AndPattern  :: ChannelLike c a => ChannelPattern c a -> DefPattern b -> DefPattern (a -> b)

data ChannelPattern c a where
    All  :: c a -> ChannelPattern c a
    Only :: Eq a => c a -> a -> ChannelPattern c a

instance Show (DefPattern p) where
    show (LastPattern (All c))    = show c
    show (LastPattern (Only c _)) = show c ++ "=value"

    show (AndPattern (All c) d) = show c ++ " & " ++ show d
    show (AndPattern (Only c _) d) = show c ++ "=value" ++ " & " ++ show d

-- | Infix, cons a Channel to a ChannelPattern.
(&) :: ChannelLike c a => ChannelPattern c a -> DefPattern p -> DefPattern (a -> p)
p & ps = p `AndPattern` ps
infixr 7 &

-- | Infix, cons a Channel to a final Channel of a ChannelPattern.
{-(&|) :: (Spawnable c0 a, Spawnable c1 b) => c0 a -> c1 b -> ChannelPattern (a -> b -> ProcessM ())-}
(&|) :: (ChannelLike c0 a, ChannelLike c1 b) => ChannelPattern c0 a -> ChannelPattern c1 b -> DefPattern (a -> b -> ProcessM ())
c &| d = c & LastPattern d

-- | 'Promote' a single Channel to a ChannelPattern.
on :: ChannelLike c a => ChannelPattern c a -> DefPattern (a -> ProcessM ())
on = LastPattern




-- | A single join Process. Implemented monadically over Instruction with the
-- Operational Monad.
type ProcessM a = ProgramT Instruction IO a

-- | Enter a single Def Instruction into ProcessM.
def :: forall p. DefPattern p -> p -> ProcessM ()
def c p = singleton $ Def c p

-- | Enter a single Inert Instruction into ProcessM.
inert :: ProcessM ()
inert = singleton Inert

-- | Enter a single NewChannel Instruction into ProcessM.
newChannel :: forall a. ProcessM (Channel a)
newChannel = singleton NewChannel

-- | Enter a single Send Instruction into ProcessM.
send :: forall c a. ChannelLike c a => c a -> a -> ProcessM ()
send c a = singleton $ Send c a

-- | Enter a single Spawn Instruction into ProcessM.
spawn :: ProcessM () -> ProcessM ()
spawn p = singleton $ Spawn p

-- | Enter a single With Instruction into ProcessM.
with :: ProcessM () -> ProcessM () -> ProcessM ()
with p q = singleton $ With p q


-- | Request a new Synchronous Channel in ProcessM.
newSyncChannel :: ProcessM (SyncChannel a)
newSyncChannel = do
    s <- newChannel
    r <- newChannel
    return $ SyncChannel s r

-- | Spawn a message on the reply channel of a synchronous channel.
reply :: SyncChannel a -> a -> ProcessM ()
reply (SyncChannel _ r) = send r

-- | Interpret a ProcessM computation with a given Interpretation.
interpret :: MonadIO io => Interpretation io -> ProcessM a -> io ()
interpret i m = do inst <- liftIO $ viewT m
                   case inst of
                        Return _ -> return ()

                        Def        c p :>>= k -> iDef i c p >> interpret i (k ())

                        Inert          :>>= _ -> iInert i >> return ()

                        NewChannel     :>>= k -> iNewChannel i >>= interpret i . k

                        Send       c a :>>= k -> iSend i c a >> interpret i (k ())

                        Spawn      p   :>>= k -> iSpawn i p >> interpret i (k ())

                        With       p q :>>= k -> iWith i p q >> interpret i (k ())

-- | An Interpretation is a collection of functions used to interpret
-- a ProcessM computation.
data Interpretation m = Interpretation
    { iDef        :: forall p. DefPattern p -> p -> m ()
    , iInert      :: m ()
    , iNewChannel :: forall a. m (Channel a)
    , iSend       :: forall c a. ChannelLike c a => c a -> a -> m ()
    , iSpawn      :: ProcessM () -> m ()
    , iWith       :: ProcessM () -> ProcessM () -> m ()
    }

