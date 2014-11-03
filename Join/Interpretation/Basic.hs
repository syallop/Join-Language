{-# LANGUAGE GADTs #-}
{-|
Module      : Join.Interpretation.Basic
Copyright   : (C) Samuel A. Yallop, 2014
Maintainer  : syallop@gmail.com
Stability   : experimental

This module exports an interpreter for 'Process a' s written in Join.Language.
-}
module Join.Interpretation.Basic
    (run
    ) where

import Prelude hiding (lookup)

import Join
import Join.Interpretation.Basic.Debug
import Join.Interpretation.Basic.Rule

import           Control.Applicative                ((<$>),(<*>),pure)
import           Control.Concurrent                 (forkIO,newMVar,newEmptyMVar,threadDelay)
import           Control.Concurrent.MVar            (MVar,takeMVar,putMVar,readMVar)
import           Control.Monad                      (liftM)
import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Operational
import qualified Data.Bimap                as Bimap
import           Data.List                          (nub)
import           Data.Map                           (Map,map,union,lookup,empty,delete)
import           Data.Maybe                         (fromJust)
import           Data.Unique                        (hashUnique,newUnique)

-- | Associate ChanId's to a reference to the containing Rule.
-- Caching the corresponding BoxId.
type Rules = Map ChanId (RuleRef,BoxId)
type RulesRef = MVar Rules

-- | Concurrent reference to a 'Rule'.
type RuleRef = MVar Rule

-- | The state of an interpretation pass.
data State = State
    { rules        :: RulesRef -- ^ All defined rules in scope.
    , replyContext :: ReplyCtx -- ^ Where 'Reply' messages should be written/ where the sender is waiting
                               -- within the calling context.
    }

-- | The initial empty 'State'. No defined rules, no replyContext.
mkState :: IO State
mkState = State <$> newMVar empty <*> pure empty

-- | Lookup a 'ChanId's associated 'ReplyChan' within the 'State's
-- replyContext.
lookupReplyChan :: State -> ChanId -> ReplyChan
lookupReplyChan st cId = fromJust $ lookup cId $ replyContext st

-- | Take the 'Rules' mapping from the 'State'.
takeRules :: State -> IO Rules
takeRules = takeMVar . rules

-- | Place a 'Rules' mapping in the 'State'.
putRules :: State -> Rules -> IO ()
putRules st = putMVar (rules st)

-- | On the given 'State's pass the 'Rules' to a function 'f'
-- which returns an updated 'Rules' (which is replaced in the 'State') and
-- a value 'a', which is returned.
withRules :: State -> (Rules -> IO (Rules,a)) -> IO a
withRules st f = takeRules st >>= f >>= (\(rs,a) -> putRules st rs >> return a)

-- | 'withRules' but the given update function is pure.
withRules' :: State -> (Rules -> (Rules,a)) -> IO a
withRules' st f = withRules st (return . f)

-- | Apply a transformation function to a 'State's 'Rules'.
onRules :: State -> (Rules -> IO Rules) -> IO ()
onRules st f = takeRules st >>= f >>= putRules st

-- | Apply a pure transformation function to a 'State's 'Rules'.
onRules' :: State -> (Rules -> Rules) -> IO ()
onRules' st f = onRules st (return . f)

-- | Look up the 'Rule' and 'BoxId' associated with a 'ChanId' within the
-- 'State' which is passed to the function 'f' which returns an updated
-- 'Rule' (which is replaced in the 'State') and a value 'a' which is
-- returned.
withRule :: State -> ChanId -> ((Rule,BoxId) -> IO (Rule,a)) -> IO a
withRule st cId f = do
    rs <- takeRules st
    putRules st rs

    let (ruleRef,boxId) = fromJust $ lookup cId rs

    rl <- takeMVar ruleRef
    traceIO ("withRule.before: " ++ showRule rl)

    (rl',a) <- f (rl,boxId)
    traceIO ("withRule.after: " ++ showRule rl')

    putMVar ruleRef rl'

    return a

-- | 'withRule' but the given update function is pure.
withRule' :: State -> ChanId -> ((Rule,BoxId) -> (Rule,a)) -> IO a
withRule' st cId f = do
    r <- withRule st cId (return . f)
    return r



-- | Run a 'Process a' computation in IO.
run :: Process a -> IO a
run p = mkState >>= (`runWith` p)
  where
    -- | Run a 'Process a' computation under some interpretation 'State' in
    -- IO.
    runWith :: State -> Process a -> IO a
    runWith state p = do
        instr <- viewT p
        case instr of

            -- End of Process
            Return a -> return a

            -- New Join definition.
            --
            -- Build and store a corresponding rule, indexed in the State by each ChanId
            -- it mentions.
            Def jp
                :>>= k -> do traceIO "DEF"
                             rId <- newRuleId
                             let joinPattern = describe jp
                                 rule        = mkRule joinPattern rId
                             traceIO $ showRule rule

                             rlRef <- newMVar rule
                             let additionalMappings =  (\boxId -> (rlRef,boxId)) <$> (Bimap.toMap $ _chanMapping rule)
                             onRules' state (additionalMappings `union`)

                             traceIO "/DEF"
                             runWith state (k ())

            -- Create a New Channel with inferred type and synchronicity.
            NewChannel
                :>>= k -> do traceIO "NEWCHANNEL"
                             id <- newChanId
                             traceIO "/NEWCHANNEL"
                             runWith state (k $ inferSync id)

            -- To a channel, send a message.
            --
            -- Add the message to the corresponding rule, concurrently
            -- executing any matching trigger that is returned.
            Send c m
                :>>= k -> do traceIO "SEND"
                             mProcCtx <- withRule' state (getId c) (\(rl,boxId) -> addMessage (forgetMessageType m,Nothing) (getId c) rl)
                             case mProcCtx of
                                 Nothing
                                   -> do traceIO "/SEND"
                                         runWith state (k ())
                                 Just (p,replyCtx)
                                   -> do forkIO $ runWith (state{replyContext = replyCtx}) p
                                         traceIO "/SEND"
                                         runWith state (k ())

            -- Spawn a Process to be executed concurrently to the remaining
            -- computation.
            Spawn p
                :>>= k -> do traceIO "SPAWN"
                             forkIO (runWith state p)
                             traceIO "/SPAWN"
                             runWith state (k ())

            -- To a channel, send a synchronous message.
            --
            -- - Create an internal replyChan for a response to be written to
            --   and an encapsulating response to return.
            -- - Concurrently wait on a value being written into the
            --   replyChan to be passed into the external response.
            -- - Add the message to the corresponding rule, concurrently
            --   executing any matching trigger that is returned.
            Sync s m
                :>>= k -> do traceIO "SYNC"
                             replyChan <- newEmptyMVar
                             response <- emptyResponse
                             forkIO $ waitOnReply replyChan response
                             mProcCtx <- withRule' state (getId s) (\(rl,boxId) -> addMessage (forgetMessageType m,Just replyChan) (getId s) rl)
                             case mProcCtx of
                                 Nothing
                                   -> do traceIO "/SYNC"
                                         runWith state (k response)
                                 Just (p,replyCtx)
                                   -> do forkIO $ runWith (state{replyContext = replyCtx}) p
                                         traceIO "/SYNC triggered"
                                         runWith state (k response)

            -- To a channel, reply with a synchronous message.
            --
            -- - Lookup the replyChan associated with the channel message
            --   which caused this Process to trigger and write the reply
            --   message to it.
            Reply s m
                :>>= k -> do traceIO "REPLY"
                             let replyChan = lookupReplyChan state (getId s)
                             putMVar replyChan (forgetMessageType m)
                             traceIO "/REPLY"
                             runWith state (k ())

            -- Concurrently execute two Process's.
            With p q
                :>>= k -> do traceIO "WITH"
                             forkIO (runWith state p)
                             forkIO (runWith state q)
                             traceIO "/WITH"
                             runWith state (k ())

    -- Return a unique Int Id each call.
    newId :: IO Int
    newId = hashUnique <$> newUnique

    -- Return a unique ChanId each call.
    newChanId :: IO ChanId
    newChanId = ChanId <$> newId

    -- Return a unique RuleId each call.
    newRuleId :: IO RuleId
    newRuleId = RuleId <$> newId

    -- Wait for a message to be written into an internal 'ReplyChan' to be
    -- passed in turn to some external response which may be waiting for the
    -- reply.
    waitOnReply :: MessageType r => ReplyChan -> Response r -> IO ()
    waitOnReply replyChan response = do
        rawMsg <- takeMVar replyChan
        case recallMessageType rawMsg of
            Nothing -> error "Mistyped value in reply."
            Just r -> writeResponse response r

    -- Determine which rules are overlapping with a list of ChanId's.
    overlappingRules :: State -> [ChanId] -> IO [Rule]
    overlappingRules state cIds = withRules state (collectOverlaps' cIds)
      where
        collectOverlaps' :: [ChanId] -> Rules -> IO (Rules,[Rule])
        collectOverlaps' cIds rs = do
            let (rs',overlapRefs) = foldr (\cId (rs,acc) -> maybe (rs,acc) (\(mr,_) -> (delete cId rs, mr:acc)) $ lookup cId rs) (rs,[]) cIds
                overlapRefs'      = nub overlapRefs
            overlapRules <- mapM takeMVar overlapRefs'
            return (rs',overlapRules)

