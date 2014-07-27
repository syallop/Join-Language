{-# LANGUAGE GADTs #-}
module Join.Interpretation.Complex where

import Join.Interpretation.Complex.Rule

import Join.Language
import Join.Types

import Prelude hiding (lookup)

import Control.Applicative        ((<$>),(<*>),pure)
import Control.Concurrent         (forkIO,newMVar,newEmptyMVar,threadDelay)
import Control.Concurrent.MVar    (MVar,takeMVar,putMVar,readMVar)
import Control.Monad              (liftM)
import Data.List                  (nub)
import Data.Map                   (Map,map,union,lookup,empty,delete)
import Data.Maybe                 (fromJust)
import Data.Serialize             (Serialize,encode,decode)
import Control.Monad.IO.Class     (liftIO)
import Control.Monad.Operational
import Data.Unique                (hashUnique,newUnique)

type Rules = Map ChanId (MVar Rule,ChanIx)

data State = State
    { rules        :: MVar Rules
    , replyContext :: ReplyCtx
    }

mkState :: IO State
mkState = State <$> newMVar empty <*> pure empty

takeRules :: State -> IO Rules
takeRules = takeMVar . rules

putRules :: State -> Rules -> IO ()
putRules st = putMVar (rules st)

withRules :: State -> (Rules -> IO (Rules,a)) -> IO a
withRules st f = takeRules st >>= f >>= (\(rs,a) -> putRules st rs >> return a)

withRules' :: State -> (Rules -> (Rules,a)) -> IO a
withRules' st f = withRules st (return . f)

onRules :: State -> (Rules -> IO Rules) -> IO ()
onRules st f = takeRules st >>= f >>= putRules st

onRules' :: State -> (Rules -> Rules) -> IO ()
onRules' st f = onRules st (return . f)


withRule :: State -> ChanId -> ((Rule,ChanIx) -> IO (Rule,a)) -> IO a
withRule st cId f = do
    rs <- takeRules st
    putRules st rs

    let (ruleRef,ix) = fromJust $ lookup cId rs

    rl <- takeMVar ruleRef
    (rl',a) <- f (rl,ix)
    putMVar ruleRef rl'

    return a

withRule' :: State -> ChanId -> ((Rule,ChanIx) -> (Rule,a)) -> IO a
withRule' st cId f = withRule st cId (return . f)

lookupChanId :: State -> ChanId -> IO (MVar Rule,ChanIx)
lookupChanId st cId = liftM (fromJust . lookup cId) (takeRules st)

lookupReply :: State -> ChanId -> ReplyChan
lookupReply st cId = fromJust $ lookup cId $ replyContext st



run :: Process a -> IO a
run p = mkState >>= (`runWith` p)


runWith :: State -> Process a -> IO a
runWith state p = do
    instr <- viewT p
    case instr of

        Return a -> return a

        Def p f
            :>>= k -> do let cIds = ChanId . fst <$> rawPattern p
                         overlappingRules <- collectOverlaps state cIds -- :: [Rule]
                         rId <- RuleId <$> newId
                         let rule = mergeNewRule cIds (TriggerF f) overlappingRules rId

                         rlRef <- newMVar rule
                         let newRules = (\ix -> (rlRef,ix)) <$> chanMapping' rule
                         onRules' state (newRules `union`)

                         runWith state (k ())

        NewChannel
            :>>= k -> do id <- newId
                         runWith state (k $ inferSync id)

        Send c m
            :>>= k -> do mProcCtx <- withRule' state (ChanId $ getId c) (\(rl,ix) -> addMessage ix (encode m,Nothing) rl)
                         case mProcCtx of
                             Nothing
                               -> runWith state (k ())
                             Just (p,replyCtx)
                               -> do forkIO $ runWith (state{replyContext = replyCtx}) p
                                     runWith state (k ())

        Spawn p
            :>>= k -> do forkIO (runWith state p)
                         runWith state (k ())

        Sync s m
            :>>= k -> do replyChan <- newEmptyMVar
                         syncVal   <- new
                         forkIO $ waitOnReply replyChan syncVal
                         mProcCtx <- withRule' state (ChanId $ getId s) (\(rl,ix) -> addMessage ix (encode m,Just replyChan) rl)
                         case mProcCtx of
                             Nothing -> do runWith state (k syncVal)
                             Just (p,replyCtx)
                               -> do forkIO $ runWith (state{replyContext = replyCtx}) p
                                     runWith state (k syncVal)

        Reply s m
            :>>= k -> do let replyChan = lookupReply state (ChanId $ getId s)
                         putMVar replyChan (encode m)
                         runWith state (k ())

        With p q
            :>>= k -> do forkIO (runWith state p)
                         forkIO (runWith state q)
                         runWith state (k ())

newId :: IO Int
newId = hashUnique <$> newUnique

waitOnReply :: Serialize a => ReplyChan -> SyncVal a -> IO ()
waitOnReply replyChan syncVal = do
    rawMsg <- takeMVar replyChan
    case decode rawMsg of
        Left  _ -> error "Mistyped value in reply."
        Right a -> write syncVal a

collectOverlaps :: State -> [ChanId] -> IO [Rule]
collectOverlaps state cIds = withRules state (collectOverlaps' cIds)
  where
    collectOverlaps' :: [ChanId] -> Rules -> IO (Rules,[Rule])
    collectOverlaps' cIds rs = do
        let (rs',overlapRefs) = foldr (\cId (rs,acc) -> maybe (rs,acc) (\(mr,_) -> (delete cId rs, mr:acc)) $ lookup cId rs) (rs,[]) cIds
            overlapRefs'      = nub overlapRefs
        overlapRules <- mapM takeMVar overlapRefs'
        return (rs',overlapRules)

