{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Join.Interpretation.Simple
Copyright   : (c) Samuel A. Yallop, 2014
Maintainer  : syallop@gmail.com
Stability   : experimental

This module defines a quick, unrefined interpretation of "Join.Language" that may
be used to run 'ProcessM' computations.

-}
module Join.Interpretation.Simple
    (run
    ) where

import Prelude hiding (lookup)

import Join.Interpretation.Simple.Types
import Join.Language
import Join.Types

import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad
import Control.Monad.Operational
import Control.Monad.IO.Class
import Data.Map hiding (map)
import Data.Serialize (Serialize)

import Join.Language.Examples

-- | State of the entire interpretation.
data IState = IState
    { msgQueue   :: MsgQueue
    , matchRules :: MatchRules
    }

-- | Reference to state of interpretation.
data IStateRef = IStateRef (MVar IState)

-- | Get a reference to the state of a new interpretation.
newIStateRef :: IO IStateRef
newIStateRef = liftM IStateRef (newMVar $ IState mkMsgQueue [])

-- | From a reference to the interpretation state, take the current state.
takeIStateRef :: IStateRef -> IO IState
takeIStateRef (IStateRef r) = takeMVar r

-- | Into an reference to the interpretation state, put a new state.
putIStateRef ::IStateRef -> IState -> IO ()
putIStateRef (IStateRef r) = putMVar r

-- | State of interpretation of a single ProcessM.
data PState = PState
    { iStateRef     :: IStateRef
    , replyChannels :: Map ChanId ChanId
    }

-- | Create a new top-level Process state (new IState).
newPState :: IO PState
newPState = do
    is <- newIStateRef
    return $ PState is empty

-- | Create a Process execution state, given an existing interpretation
-- state.
mkPState :: IStateRef -> PState
mkPState iStR = PState iStR empty

-- | Run a top-level ProcessM.
run :: ProcessM a -> IO a
run p = do
    st    <- newPState
    forkIO $ ruleHandler (iStateRef st)
    runWith st p

-- | With state to be ran under, run a ProcessM to produce an IO action.
runWith :: PState -> ProcessM a -> IO a
runWith st@(PState iStR rcs) p = do
    instr <- liftIO $ viewT p
    case instr of

        -- Operational return.
        Return a     -> return a

        -- Def pattern.
        Def dp p
            :>>= k -> do iSt <- takeIStateRef iStR
                         let rs   = matchRules iSt
                             rs'  = registerMatchRule (mkChanMatches dp) p rs
                             iSt' = iSt{matchRules = rs'}
                         putIStateRef iStR iSt'
                         runWith st (k ())

        -- Request a new Channel.
        NewChannel
            :>>= k -> do iSt <- takeIStateRef iStR
                         cId <- newChanId
                         let c     = inferSync (getChanId cId)
                             msgQ  = msgQueue iSt
                             msgQ' = registerChanId cId msgQ
                             iSt'  = iSt{msgQueue = msgQ'}
                         putIStateRef iStR iSt'
                         runWith st (k c)

        -- Send a message to a Channel.
        Send c m
            :>>= k -> do iSt <- takeIStateRef iStR
                         let msgQ  = msgQueue iSt
                             msgQ' = registerMsg (mkChanId c) (mkMsg (mkMsgData m) Nothing) msgQ
                             iSt'  = iSt{msgQueue = msgQ'}
                         putIStateRef iStR iSt'
                         runWith st (k ())

        -- Spawn a concurrent execution of a ProcessM.
        Spawn p
            :>>= k -> do forkIO $ runWith st p
                         runWith st (k ())

        -- Synchronously spawn a value on a Channel, and wait for
        -- a result.
        Sync s m
            :>>= k -> do iSt <- takeIStateRef iStR
                         rId <- newChanId
                         let r     = inferSync (getChanId rId)
                             msgQ  = msgQueue iSt
                             msgQ' = registerMsg (mkChanId s) (mkMsg (mkMsgData m) (Just rId)) msgQ
                             iSt'  = iSt{msgQueue = msgQ'}
                         putIStateRef iStR iSt'
                         v <- new
                         forkIO $ waitOn iStR r v
                         runWith st (k v)

        -- Spawn a value on the reply Channel of a SyncChannel.
        Reply s m
            :>>= k -> do iSt <- takeIStateRef iStR
                         let rs   = replyChannels st
                             i    = mkChanId s
                             mj   = lookup i rs
                             msgQ = msgQueue iSt
                         case mj of
                             Nothing -> error "No reply Channel."
                             Just j  -> let msgQ' = registerMsg j (mkMsg (mkMsgData m) Nothing) msgQ
                                            iSt'  = iSt{msgQueue = msgQ'}
                                           in putIStateRef iStR iSt'
                                           >> runWith st (k ())

        With p q
            :>>= k -> do forkIO $ runWith st p
                         forkIO $ runWith st q
                         runWith st (k ())

-- | Responsible for triggering matching rule definitions.
ruleHandler :: IStateRef -> IO ()
ruleHandler iStR = forever $ do
    iSt <- takeIStateRef iStR
    let rs   = matchRules iSt
        msgQ = msgQueue iSt
    msgQ' <- tryAllRules rs msgQ
    let iSt' = iSt{msgQueue = msgQ'}
    putIStateRef iStR iSt'
    yield
    threadDelay 100000
  where
    -- | Try and execute all rules against a MsgQueue.
    tryAllRules :: MatchRules -> MsgQueue -> IO MsgQueue
    tryAllRules []     msgQ = return msgQ
    tryAllRules (r:rs) msgQ = case tryMatchRule r msgQ of
        Nothing                    -- Rule failed
            -> tryAllRules rs msgQ -- Try the next..

        Just (msgQ', msgs)                 -- Rule succeeded
            -> do forkIO $ execRule r msgs -- Concurrently trigger the rule
                  tryAllRules rs msgQ'     -- Try the next..

    -- | Execute a matched MatchRule on the corresponding Messages.
    execRule :: MatchRule -> Msgs -> IO ()
    execRule (MatchRule cms f) msgs =
        let rs    = extractReplyIds cms msgs
            iStR' = (mkPState iStR){replyChannels = fromList rs}
            bs    = map (unMsgData . fst . unMsg) msgs
            p     = unsafeApply f bs
           in runWith iStR' p

-- | Wait for any message on a given channel, and write it to a SyncVal.
waitOn :: Serialize a => IStateRef -> Channel S a -> SyncVal a -> IO ()
waitOn iStR c r = do
    iSt <- takeIStateRef iStR

    let msgQ     = msgQueue iSt
        matchAll = tryChanMatch (mkChanMatch c) msgQ

    case matchAll of
        Nothing
          -> do putIStateRef iStR iSt
                yield
                threadDelay 100000
                waitOn iStR c r

        Just (msgQ',m)
          -> do putIStateRef iStR (iSt{msgQueue = msgQ'})
                let (d,_) = unMsg m
                write r (unsafeDecodeMsgData d)

