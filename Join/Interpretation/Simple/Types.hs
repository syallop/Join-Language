{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_HADDOCK hide #-}
module Join.Interpretation.Simple.Types
    (-- Encode Message/ Channel types.
     ChanId()
    ,ReplyId
    ,MsgData()
    ,Msg()
    ,Msgs
    ,MsgQueue()
    -- Smart constructors of Message/ Channel types.
    , newChanId
    , mkChanId
    , getChanId
    , mkMsgData
    , unMsgData
    , unsafeDecodeMsgData
    , mkMsg
    , unMsg
    , mkMsgQueue

    -- Encode Def Pattern types.
    ,ChanMatch()
    ,ChanMatches()
    ,MatchRule(..)
    ,MatchRules
    , mkChanMatch
    , mkChanMatches
    , mkMatchRule

    ,registerChanId
    ,registerMsg
    ,registerMatchRule

    ,tryMsgMatch
    ,tryChanMatch
    ,tryChanMatches
    ,tryMatchRule

    ,extractReplyIds
    ) where

import Prelude hiding (lookup)

import Join.Language
import Join.Language.Types

import Control.Arrow       (first)
import Control.Applicative ((<$>))
import Data.ByteString     (ByteString)
import Data.Map            (Map,empty,insert,insertWith,lookup)
import Data.Serialize      (decode,encode,Serialize)
import Data.Unique         (newUnique,hashUnique)

{-= Encode Message/ Channel types =-}
-- | Unique identifier of a Channel.
newtype ChanId = ChanId Int deriving (Eq, Ord)

-- | Potential identifier on which a Reply should be sent.
type ReplyId = Maybe ChanId

-- | Encoding of a value send on a Channel.
newtype MsgData = MsgData {unMsgData :: ByteString} deriving (Eq,Ord)

-- | A single encoded value as sent on a Channel, tagged with a possible
-- return ChanId.
newtype Msg = Msg {unMsg :: (MsgData, ReplyId)}

-- | Many Msg's.
type Msgs = [Msg]

-- | Map ChannelId's to all the Msg's waiting on them.
newtype MsgQueue = MsgQueue {unMsgQueue :: Map ChanId Msgs}

-- | Get a new, ChanId, guaranteed to be unique.
newChanId :: IO ChanId
newChanId = ChanId <$> hashUnique <$> newUnique

mkChanId :: Channel t a -> ChanId
mkChanId = ChanId . getId

-- | Get an Int representation of a ChanId.
getChanId :: ChanId -> Int
getChanId (ChanId i) = i

mkMsgData :: Serialize a => a -> MsgData
mkMsgData = MsgData <$> encode

unsafeDecodeMsgData :: Serialize a => MsgData -> a
unsafeDecodeMsgData (MsgData s) = case decode s of
    Left  _ -> error "Invalid decode."
    Right a -> a

mkMsg :: MsgData -> ReplyId -> Msg
mkMsg d i = Msg (d,i)

mkMsgQueue :: MsgQueue
mkMsgQueue = MsgQueue empty



{-= Encode Def Pattern types =-}
-- | Encodes a single item on the LHS of a Def Pattern.
-- Match the first Msg sent to a ChanId/ first Msg equal to MsgData.
newtype ChanMatch = ChanMatch (ChanId, Maybe MsgData)

-- | Encodes the LHS of a Def Pattern.
-- Match when all individual ChanMatch's simultaneously match.
type ChanMatches = [ChanMatch]

-- | Encodes an entire Def Pattern.
-- When the ChanMatches match, a function should be triggered.
data MatchRule = forall f. Apply f => MatchRule ChanMatches f

-- Many Def Pattern's.
type MatchRules = [MatchRule]

-- | Lift a Language SubPattern into an internal ChanMatch.
mkChanMatch :: SubPattern t p => t -> ChanMatch
mkChanMatch t = let (i,md) = rawSubPattern t
                    in ChanMatch (ChanId i,MsgData <$> md)

-- | Lift a Language Pattern into an internal ChanMatches.
mkChanMatches :: Pattern t p => t -> ChanMatches
mkChanMatches p = (\(i,md) -> ChanMatch (ChanId i, MsgData <$> md)) <$> rawPattern p

-- | Lift the LHS and RHS of a Language Def into an internal MatchRule.
mkMatchRule :: (Apply f, Pattern t f) => t -> f -> MatchRule
mkMatchRule t = MatchRule (mkChanMatches t)

{-= Primitive functions =-}
-- | Register a new ChanId (With no waiting Msg's) in a MsgQueue.
registerChanId :: ChanId -> MsgQueue -> MsgQueue
registerChanId i = MsgQueue . insert i [] . unMsgQueue

-- | Register, on a ChanId, a new Msg in a MsgQueue.
registerMsg :: ChanId -> Msg -> MsgQueue -> MsgQueue
registerMsg i m = MsgQueue . insertWith (++) i [m] . unMsgQueue

-- | Given ChanMatches and a corresponding trigger function, register it in
-- a MatchRules collection.
registerMatchRule :: Apply f => ChanMatches -> f -> MatchRules -> MatchRules
registerMatchRule cms f rs = MatchRule cms f : rs

-- | With a possible MsgData to require equality with, extract the first
-- matching Msg from some Msgs.
tryMsgMatch :: Maybe MsgData -> Msgs -> Maybe (Msgs,Msg)
tryMsgMatch _         []    = Nothing
tryMsgMatch Nothing  (m:ms) = Just (ms,m)
tryMsgMatch (Just s) (Msg (s', mid) : ms)
    -- String-match => Extract&return
    | s == s'   = Just (ms,Msg (s', mid))

    -- Recurse on remaining messages, re-prepending the non-matching message
    -- to the result when Just.
    | otherwise = first ((:) (Msg (s', mid))) <$> tryMsgMatch (Just s) ms

-- | Given a ChanMatch, attempt to extract a matching Msg within a MsgQueue.
tryChanMatch :: ChanMatch -> MsgQueue -> Maybe (MsgQueue, Msg)
tryChanMatch (ChanMatch (i,mm)) q = do
    ms <- lookup i (unMsgQueue q) -- Lookup corresponding messages
    (ms',m) <- tryMsgMatch mm ms  -- take the first match
    return (MsgQueue $ insert i ms' (unMsgQueue q), m)

-- | Given a collection of ChanMatch's, extract a set of matching Msgs when
-- each ChanMatch is successful.
tryChanMatches :: ChanMatches -> MsgQueue -> Maybe (MsgQueue, Msgs)
tryChanMatches []       q = Just (q,[])
tryChanMatches (cm:cms) q = do
    (q',m)   <- tryChanMatch cm q
    (q'',ms) <- tryChanMatches cms q'
    return (q'',m:ms)

-- | Given a MatchRule and MessageQueues, test the rule, if successful
-- returning the updated MessageQueues and the matching Messages.
tryMatchRule :: MatchRule -> MsgQueue -> Maybe (MsgQueue, Msgs)
tryMatchRule (MatchRule cms _) = tryChanMatches cms


extractReplyIds :: ChanMatches -> Msgs -> [(ChanId,ChanId)]
extractReplyIds = extractReplyIds' []
  where
    extractReplyIds' :: [(ChanId,ChanId)] -> ChanMatches -> Msgs -> [(ChanId,ChanId)]
    extractReplyIds' acc []          []                     = acc
    extractReplyIds' acc (_:cms)     (Msg (_,Nothing) : ms) = extractReplyIds' acc cms ms
    extractReplyIds' acc (ChanMatch (i,_) : cms) (Msg (_,Just j) : ms)  = extractReplyIds' ((i,j):acc) cms ms

