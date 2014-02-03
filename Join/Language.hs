{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Join.Language
    ( Instruction(..)
    , ProcessM
    , def
    , inert
    , newChannel
    , spawn
    , with

    , newSyncChannel
    , reply

    , Interpretation(..)
    , interpret

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
    Def        :: ChannelPattern p        -- ^ Channels matched for.
               -> p                       -- ^ Handler function called.
               -> Instruction ()          -- ^ No result, asynchronous.

    -- | End process.
    Inert      :: Instruction ()          -- ^ No result, end interpretation of ProcessM.

    -- | Request a new typed Channel.
    NewChannel :: Instruction (Channel a) -- ^ Result in typed Channel, synchronous.

    -- | Spawn a value on a Channel.
    Spawn      :: Spawnable c a
               => c a                     -- ^ Channel spawned on.
               -> a                       -- ^ Value spawned.
               -> Instruction ()          -- ^ No result, asynchronous.

    -- | Concurrently execute two Process's.
    With       :: ProcessM ()             -- ^ First process.
               -> ProcessM ()             -- ^ Second process.
               -> Instruction ()          -- ^ No result, asynchronous execution of both Processes.



-- | A patten of one or many Channel's. Type variable denotes the type of
-- a function which accepts the type of each conjunctive Channel in order
-- and produces a ProcessM (). This type corresponds to the type of the
-- function in the RHS of a join definition.
--
-- For convenience, a ChannelPattern may be built using 'on' and infix functions '&'
-- '&|'.
-- E.g. Matching on (a, b, c, ... ,n ,m) == (a & b & c &...n &| m)
data ChannelPattern p where
    LastChannel :: Spawnable c a => c a -> ChannelPattern (a -> ProcessM ())
    AndChannels :: Spawnable c a => c a -> ChannelPattern p -> ChannelPattern (a -> p)

instance Show (ChannelPattern p) where
    show (LastChannel c   ) = show c
    show (AndChannels c cs) = show c ++ " & " ++ show cs

-- | Infix, cons a Channel to a ChannelPattern.
(&) :: Spawnable c a => c a -> ChannelPattern p -> ChannelPattern (a -> p)
c & cs = c `AndChannels` cs
infixr 7 &

-- | Infix, cons a Channel to a final Channel of a ChannelPattern.
(&|) :: (Spawnable c0 a, Spawnable c1 b) => c0 a -> c1 b -> ChannelPattern (a -> b -> ProcessM ())
c &| d = c & LastChannel d

-- | 'Promote' a single Channel to a ChannelPattern.
on :: Spawnable c a => c a -> ChannelPattern (a -> ProcessM ())
on = LastChannel



-- | A single join Process. Implemented monadically over Instruction with the
-- Operational Monad.
type ProcessM a = ProgramT Instruction IO a

-- | Enter a single Def Instruction into ProcessM.
def :: forall p. ChannelPattern p -> p -> ProcessM ()
def c p = singleton $ Def c p

-- | Enter a single Inert Instruction into ProcessM.
inert :: ProcessM ()
inert = singleton Inert

-- | Enter a single NewChannel Instruction into ProcessM.
newChannel :: forall a. ProcessM (Channel a)
newChannel = singleton NewChannel

-- | Enter a single Spawn Instruction into ProcessM.
spawn :: forall c a. Spawnable c a => c a -> a -> ProcessM ()
spawn c a = singleton $ Spawn c a

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
reply (SyncChannel _ r) = spawn r

-- | Interpret a ProcessM computation with a given Interpretation.
interpret :: MonadIO io => Interpretation io -> ProcessM a -> io ()
interpret i m = do inst <- liftIO $ viewT m
                   case inst of
                        Return _ -> return ()

                        Def        c p :>>= k -> iDef i c p >> interpret i (k ())

                        Inert          :>>= _ -> iInert i >> return ()

                        NewChannel     :>>= k -> iNewChannel i >>= interpret i . k

                        Spawn      c a :>>= k -> iSpawn i c a >> interpret i (k ())

                        With       p q :>>= k -> iWith i p q >> interpret i (k ())

-- | An Interpretation is a collection of functions used to interpret
-- a ProcessM computation.
data Interpretation m = Interpretation
    { iDef        :: forall p. ChannelPattern p -> p -> m ()
    , iInert      :: m ()
    , iNewChannel :: forall a. m (Channel a)
    , iSpawn      :: forall c a. Spawnable c a => c a -> a -> m ()
    , iWith       :: ProcessM () -> ProcessM () -> m ()
    }

