{-# LANGUAGE FlexibleContexts
            ,FlexibleInstances
            ,GADTs
            ,GeneralizedNewtypeDeriving
            ,ImpredicativeTypes
            ,OverloadedStrings
            ,RankNTypes
            ,TypeSynonymInstances
 #-}
module Join.Interpretation.Basic.Rule
    ( Rule()
    , mkRule
    , mergeNewRule

    , ruleId
    , chanMapping
    , chanMapping'
    , maxIndex

    , addMessage

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
import Data.Maybe               (fromJust,fromMaybe,mapMaybe)
import Data.Tuple               (swap)

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
chanId cIx rl = case lookup cIx $ chanMapping rl of
    Nothing -> error $ "chanId: chanIx: " ++ show cIx
                     ++ " has no chanId in rule: " ++ show rl
    Just id -> id

chanIx :: ChanId -> Rule -> ChanIx
chanIx cId rl = case lookup cId . swapMap $ chanMapping rl of
    Nothing -> error $ "chanIx: chanId: " ++ show cId
                     ++ " hasno chanIx in rule: " ++ show rl

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
    convertIds = map (\cId -> case lookup cId idTix of
        Nothing -> error $ "convertIds: chanId: " ++ show cId
                         ++ "not in: " ++ show idTix
        Just ix -> ix)

-- | Given a rule, decompose it to a raw pattern
unRule :: Rule -> [([ChanId],TriggerF)]
unRule rl = destroyTrigger . snd <$> patterns rl
  where
    destroyTrigger :: Trigger -> ([ChanId],TriggerF)
    destroyTrigger (Trigger f cIxs) = (convertIxs cIxs, f)

    convertIxs :: [ChanIx] -> [ChanId]
    convertIxs = map (\cIx -> case lookup cIx (chanMapping rl) of
        Nothing -> error $ "convertIxs: chanIx: " ++ show cIx
                         ++ "not in: " ++ show (chanMapping rl)
        Just id -> id)

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

    -- existing messages ++ new msg's
    messages' :: Map ChanIx Messages
    messages' = insertWith (++) ix [msg] $ messages rl

    identifyMatches :: [(StatusPattern,Trigger)] -> Status -> [(StatusPattern,Trigger)]
    identifyMatches ps st = mapMaybe (\(p,f) -> if st `match` p then Just (p,f) else Nothing) ps

    applyMatch :: (StatusPattern,Trigger) -> Rule -> (Rule,(Process (),ReplyCtx))
    applyMatch (pat,Trigger (TriggerF f) ixs) rl
         = let (rawMsgs,replyCtx,rl') = takeMessages rl ixs
               rawMsgs' = dropUnits rawMsgs
              in (rl',(unsafeApply f (reverse rawMsgs'),replyCtx))

    -- Drop messages which are identical to the encoded unit value ().
    dropUnits :: [RawMessage] -> [RawMessage]
    dropUnits = filter (/= "")

    takeMessages :: Rule -> [ChanIx] -> ([RawMessage],ReplyCtx,Rule)
    takeMessages rl = foldr f ([],empty,rl)
      where f ix (msgs,rplyCtx,rl) = let (msg,mRply,rl') = takeMessage rl ix
                                         id = chanId ix rl'
                                        in (msgs ++ [msg]
                                           ,maybe rplyCtx (\rply -> insert id rply rplyCtx) mRply
                                           ,rl'
                                           )


    takeMessage :: Rule -> ChanIx -> (RawMessage,Maybe (MVar RawMessage),Rule)
    takeMessage rl ix = case fromMaybe 
                               (error $ "takeMessage: chanIx: " ++ show ix ++ " has no message in "
                                     ++ show (messages rl))
                               $ lookup ix (messages rl) of
        [] -> error "No message to take."

        ((m,mr):ms)
           -> (m, mr, rl{ messages = insert ix ms $ messages rl
                        , status   = if null ms
                                       then status rl `unset` unChanIx ix
                                       else status rl
                        }
              )

