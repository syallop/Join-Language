{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Join.Interpretation.Basic.Rule
    ( Rule()
    , mkRule
    , mergeNewRule

    , ruleId
    , chanMapping
    , chanMapping'
    , maxIndex

    , addMessage

    , ChanId(..)
    , ChanIx()
    , RuleId(..)
    , RawMessage
    , ReplyChan
    , ReplyCtx
    , Message
    , Messages
    , TriggerF(..)
    ) where

import Join.Interpretation.Basic.Status

import Join

import Prelude hiding (lookup)

import Control.Applicative      ((<$>))
import Control.Concurrent.MVar  (MVar)
import Data.ByteString          (ByteString)
import Data.List                (nub)
import Data.Map                 (Map,fromList,lookup,assocs,insertWith,insert,empty)
import Data.Maybe               (fromJust,mapMaybe)
import Data.Tuple               (swap)

-- | System-wide unique channel id.
newtype ChanId = ChanId {unChanId :: Int} deriving (Show,Eq,Ord,Enum,Num)

-- | Rule-wide unique channel id.
newtype ChanIx = ChanIx {unChanIx :: Int} deriving (Show,Eq,Ord,Enum,Num)

-- | System-wide unique rule id.
newtype RuleId = RuleId {unRuleId :: Int} deriving (Show)

-- | Type of raw, encoded message data.
type RawMessage = ByteString

-- | Location for a single RawMessage reply to a sync.
type ReplyChan = MVar RawMessage

-- | Reply context for 'Sync'/'Reply' instructions. Associates the ChanId
-- of synchronous channel calls to a one-use reply channel.
type ReplyCtx  = Map ChanId ReplyChan

-- | A Message is raw message data with a possible associated reply
-- channel.
type Message  = (RawMessage,Maybe ReplyChan)
type Messages = [Message]

instance Show ReplyChan where
    show _ = "REPLYCHAN"

data Trigger = Trigger TriggerF [ChanIx] deriving Show
data TriggerF = forall f. Apply f Inert => TriggerF f

instance Show TriggerF where show _ = "TRIGGERF"

-- | A Rule encapsulates the state of a single Join synchronisation
-- pattern and is responsible for tracking all messages sent on the
-- contained channels and determining when triggers are to be ran.
data Rule = Rule
    { ruleId      :: RuleId                     -- Uniquely identify a rule
    , chanMapping :: Map ChanIx ChanId          -- Map each contained channel index to an external id
    , maxIndex    :: ChanIx                     -- Maximum contained channel index
    , status      :: Status                     -- Status of messages waiting on rule
    , patterns    :: [(StatusPattern,Trigger)]  -- Collection of patterns and associated triggers
    , messages    :: Map ChanIx Messages        -- Stored messages waiting on the rule
    } deriving Show

chanId :: ChanIx -> Rule -> ChanId
chanId cIx = fromJust . lookup cIx . chanMapping

chanIx :: ChanId -> Rule -> ChanIx
chanIx cId = fromJust . lookup cId . swapMap . chanMapping

chanMapping' :: Rule -> Map ChanId ChanIx
chanMapping' = swapMap . chanMapping

swapMap :: Ord b => Map a b -> Map b a
swapMap = fromList . map swap . assocs

-- | Given a new synchronisation pattern (which shouldnt overlap with any
-- prior pattern) create a corresponding Rule with a new unique Id.
mkRule :: [([ChanId],TriggerF)] -> RuleId -> Rule
mkRule ps rId = Rule
    { ruleId      = rId
    , chanMapping = ixTid
    , maxIndex    = ChanIx maxIx
    , status      = mkStatus idCount
    , patterns    = (\(ids,f) -> (buildPattern ids,Trigger f (convertIds ids))) <$> ps
    , messages    = const [] <$> ixTid
    }
  where
    cIds    = nub $ concatMap fst ps    -- :: [ChanId]
    ixTid   = fromList $ zip [0..] cIds -- :: Map ChanIx ChanId
    idTix   = swapMap ixTid
    idCount = length cIds
    maxIx   = idCount - 1

    buildPattern :: [ChanId] -> StatusPattern
    buildPattern pIds = mkStatusPattern idCount (unChanIx <$> convertIds pIds)

    convertIds :: [ChanId] -> [ChanIx]
    convertIds = map (\cId -> fromJust $ lookup cId idTix)

-- | Given a rule, decompose it to a raw pattern
unRule :: Rule -> [([ChanId],TriggerF)]
unRule rl = destroyTrigger . snd <$> patterns rl
  where
    destroyTrigger :: Trigger -> ([ChanId],TriggerF)
    destroyTrigger (Trigger f cIxs) = (convertIxs cIxs, f)

    convertIxs :: [ChanIx] -> [ChanId]
    convertIxs = map (\cIx -> fromJust $ lookup cIx (chanMapping rl))

-- | Given a new, one clause synchronisation pattern and a map from all
-- overlapping channel ids to their corresponding rules, merge into a new
-- rule.
mergeNewRule :: [ChanId] -> TriggerF -> [Rule] -> RuleId -> Rule
mergeNewRule cIds f rs = mkRule ((cIds,f) : concatMap unRule rs)

-- | For an index, add a message within a rule, producing the updated rule
-- and a possible triggered process under a reply context.
addMessage :: ChanIx -> Message -> Rule -> (Rule,Maybe (Process (),ReplyCtx))
addMessage ix msg rl

    -- There is already at least one message on the channel.
    -- => No change in rule status
    -- => No patterns need to be considered
    | status rl `index` unChanIx ix
     = (rl{messages = messages'},Nothing)

    -- There are no existing messages on the channel.
    | otherwise
     = let status' = status rl `set` unChanIx ix
          in case identifyMatches (patterns rl) status' of

            -- Adding the message does not require a trigger.
            [] -> (rl{messages = messages', status = status'}, Nothing)

            -- Adding the message causes at least one trigger.
            -- Choose the first, apply to matching messages and produce the
            -- resulting rule and applied processM to be ran.
            ((p,f):_) -> let (rl',procCtx) = applyMatch (p,f) rl{messages = messages', status = status'}
                            in (rl',Just procCtx)

  where
    nonEmptyChannel :: Bool
    nonEmptyChannel = status rl `index` unChanIx ix

    messages' :: Map ChanIx Messages
    messages' = insertWith (++) ix [msg] $ messages rl

    identifyMatches :: [(StatusPattern,Trigger)] -> Status -> [(StatusPattern,Trigger)]
    identifyMatches ps st = mapMaybe (\(p,f) -> if st `match` p then Just (p,f) else Nothing) ps

    applyMatch :: (StatusPattern,Trigger) -> Rule -> (Rule,(Process (),ReplyCtx))
    applyMatch (pat,Trigger (TriggerF f) ixs) rl
        {-= let ixs   = ChanIx <$> getPatternIndexes pat -- :: [ChanIx]-}
         = let (rawMsgs,replyCtx,rl') = takeMessages rl ixs
              in (rl',(unsafeApply f (reverse rawMsgs),replyCtx))

    takeMessages :: Rule -> [ChanIx] -> ([RawMessage],ReplyCtx,Rule)
    takeMessages rl = foldr f ([],empty,rl)
      where f ix (msgs,rplyCtx,rl) = let (msg,mRply,rl') = takeMessage rl ix
                                         id = chanId ix rl'
                                        in (msgs ++ [msg]
                                           ,maybe rplyCtx (\rply -> insert id rply rplyCtx) mRply
                                           ,rl'
                                           )


    takeMessage :: Rule -> ChanIx -> (RawMessage,Maybe (MVar RawMessage),Rule)
    takeMessage rl ix = case fromJust $ lookup ix (messages rl) of
        [] -> error "No message to take."

        ((m,mr):ms)
           -> (m, mr, rl{ messages = insert ix ms $ messages rl
                        , status   = if null ms
                                       then status rl `unset` unChanIx ix
                                       else status rl
                        }
              )

