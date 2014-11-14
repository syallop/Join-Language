{-# LANGUAGE DataKinds
            ,GADTs
            ,KindSignatures
            ,PolyKinds
            ,RankNTypes
  #-}
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
import Join.Pattern.Rep
import Join.Interpretation.Basic.Debug
import Join.Interpretation.Basic.Status
import Join.Interpretation.Basic.MessageBox
import Join.Interpretation.Basic.Rule

import           Control.Applicative                ((<$>),(<*>),pure)
import           Control.Concurrent                 (forkIO,newMVar,newEmptyMVar,threadDelay)
import           Control.Concurrent.MVar            (MVar,takeMVar,putMVar,readMVar)
import           Control.Monad                      (liftM)
import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Operational
import qualified Data.Bimap                as Bimap
import           Data.List                          (nub)
import qualified Data.Map                  as Map
import           Data.Maybe                         (fromJust,fromMaybe)
import           Data.Typeable
import           Data.Unique                        (hashUnique,newUnique)

-- | Some 'Rule tss StatusPattern' with any 'tss'.
data SomeRule = forall tss. SomeRule (Rule tss StatusPattern)

-- | A MVar reference to SomeRule
type RuleRef = MVar SomeRule

-- | Associate ChanId's to the RuleRef which is responsible for it.
newtype RuleMap = RuleMap{_ruleMap :: Map.Map ChanId RuleRef}

-- | A MVar reference to a RuleMap.
type RuleMapRef = MVar RuleMap

-- | Create a new empty RuleMapRef
newRuleMapRef :: IO RuleMapRef
newRuleMapRef = newMVar (RuleMap Map.empty)

-- | Run a 'Process a' computation in IO.
run :: Process a -> IO a
run p = do
  ruleMapRef<- newRuleMapRef
  let replyCtx = Map.empty
  runWith ruleMapRef replyCtx p

-- | Run a 'Process a' computation in IO under the calling context of a
-- 'RuleMapRef' (mapping ChanId's to responsible Rules)
-- and a 'ReplyCtx' (mapping replied to ChanId's to the locations waiting for a response.)
runWith :: RuleMapRef -> ReplyCtx -> Process a -> IO a
runWith ruleMapRef replyCtx p = do
  instr <- viewT p
  case instr of

    Return a -> return a

    Def definition
      :>>= k -> do registerDefinition (toDefinitionsRep definition) ruleMapRef
                   runWith ruleMapRef replyCtx (k ())

    NewChannel
      :>>= k -> do cId <- newChanId
                   runWith ruleMapRef replyCtx (k $ inferSync cId)

    Send c m
      :>>= k -> do registerMessage c m ruleMapRef
                   runWith ruleMapRef replyCtx (k ())

    Spawn p
      :>>= k -> do forkIO $ runWith ruleMapRef replyCtx p
                   runWith ruleMapRef replyCtx (k ())

    Sync sc sm
      :>>= k -> do syncVal <- registerSyncMessage sc sm ruleMapRef
                   runWith ruleMapRef replyCtx (k syncVal)

    Reply sc m
      :>>= k -> do putMVar (lookupReplyChan replyCtx (getId sc)) m
                   runWith ruleMapRef replyCtx (k ())

    With p q 
      :>>= k -> do mapM_ (forkIO . runWith ruleMapRef replyCtx) [p,q]
                   runWith ruleMapRef replyCtx (k ())

-- | Lookup a ChanId's associated ReplyChan 'r' within a ReplyCtx.
-- The ChanId must have an associated ReplyChan and it must
-- have the expected type.
lookupReplyChan :: MessageType r => ReplyCtx -> ChanId -> ReplyChan r
lookupReplyChan replyCtx cId = case Map.lookup cId replyCtx of
  Nothing -> error "ChanId has no associated ReplyChan in this context."
  Just (SomeReplyChan replyChan) -> case cast replyChan of
    Nothing -> error "ReplyChan does not have the assumed type."
    Just replyChan' -> replyChan'

-- | On an Asynchronous Channel, register a message 'a'.
registerMessage :: MessageType a => Channel A (a :: *) -> a -> RuleMapRef -> IO ()
registerMessage chan msg ruleMapRef = do
  (RuleMap ruleMap) <- readMVar ruleMapRef
  let cId     = getId chan
      ruleRef = fromMaybe (error "registerMessage: ChanId has no RuleRef") $ Map.lookup cId ruleMap

  (SomeRule rule) <- takeMVar ruleRef
  let (rule',mProcCtx) = addMessage (Message msg) cId rule
  putMVar ruleRef (SomeRule rule')

  case mProcCtx of
    Nothing -> return ()
    Just (p,replyCtx) -> (forkIO $ runWith ruleMapRef replyCtx p) >> return ()

-- | On a Synchronous Channel, register a message 'a' and return a 'Response r' on which a response can
-- be waited.
registerSyncMessage :: (MessageType a,MessageType r) => Channel (S r) a -> a -> RuleMapRef -> IO (Response r)
registerSyncMessage chan msg ruleMapRef = do
  (RuleMap ruleMap) <- readMVar ruleMapRef
  let cId = getId chan
      ruleRef = fromMaybe (error "registerSyncMessage: ChanId has no RuleRef") $ Map.lookup cId ruleMap

  (SomeRule rule) <- takeMVar ruleRef
  replyChan <- newEmptyMVar
  response <- emptyResponse
  forkIO $ waitOnReply replyChan response
  let (rule',mProcCtx) = addMessage (SyncMessage msg replyChan) cId rule
  putMVar ruleRef (SomeRule rule')

  case mProcCtx of
    Nothing -> return response
    Just (p,replyCtx) -> (forkIO $ runWith ruleMapRef replyCtx p) >> return response
  where
    waitOnReply :: MessageType r => ReplyChan r -> Response r -> IO ()
    waitOnReply replyChan response = takeMVar replyChan >>= writeResponse response

-- | Register a new Join Definition, associating all contained Channels
-- with the stored RuleRef.
registerDefinition :: DefinitionsRep tss Inert -> RuleMapRef -> IO ()
registerDefinition definition ruleMapRef = do
  (RuleMap ruleMap) <- takeMVar ruleMapRef
  rId <- newRuleId
  let someRule = SomeRule $ mkRule definition rId
      cIds     = uniqueIds definition
  rlRef <- newMVar someRule
  let additionalMappings = Map.fromSet (const rlRef) cIds
      ruleMap'           = additionalMappings `Map.union` ruleMap :: Map.Map ChanId RuleRef
  putMVar ruleMapRef (RuleMap ruleMap')

-- | New unique ChanId.
newChanId :: IO ChanId
newChanId = ChanId <$> newId

-- | New unique RuleId.
newRuleId :: IO RuleId
newRuleId = RuleId <$> newId

-- | New unique Int id.
newId :: IO Int
newId = hashUnique <$> newUnique

