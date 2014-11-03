{-# LANGUAGE FlexibleContexts
            ,FlexibleInstances
            ,GADTs
            ,ImpredicativeTypes
            ,OverloadedLists
            ,OverloadedStrings
            ,RankNTypes
            ,TypeSynonymInstances
            #-}
module Join.Interpretation.Basic.Rule
    (Rule()
    ,ReplyCtx
    ,ReplyChan
    ,RuleId(..)
    ,BoxId(..)

    ,mkRule
    ,addMessage

    ,_chanMapping
    ,showRule
    ) where

import Join

import Join.Interpretation.Basic.Debug
import Join.Interpretation.Basic.MessageBox
import Join.Interpretation.Basic.Status

import           Control.Arrow
import qualified Data.Bimap     as Bimap
import           Data.List               (intercalate)
import qualified Data.Map       as Map
import qualified Data.Set       as Set
import           Data.Maybe

import Prelude hiding (take)

-- | System-wide unique rule id.
newtype RuleId = RuleId {unRuleId :: Int} deriving Show

-- | Internal representation of a single clause of a join definition.
-- Encapsulates a 'RequiredMessages' describing the messages required for
-- the clause to match alongside a 'TriggerF' - the trigger function
-- the matching messages should be passed to.
data StoredPattern = StoredPattern RequiredMessages (TriggerF Inert) deriving Show
instance Show (TriggerF r) where show _ = "TRIGGERF"

-- | Ordered list of 'RequiredMessage's
type RequiredMessages = [RequiredMessage]

-- | MessageBoxId, possible subBox index, whether message should be passed
-- to trigger.
type RequiredMessage = (BoxId,Maybe BoxIx,Bool)

-- | A 'StatusIx' is an int index into a 'Status'.
type StatusIx = Int

-- | Associate 'ChanId's to 'BoxId's.
type ChanMapping = Bimap.Bimap ChanId BoxId

-- | Associate 'BoxId's to 'MessageBox's.
type MessageBoxes = Map.Map BoxId MessageBox

-- | Describes a location where messages may be retrieved.
-- Nothing => Any message from the corresponding MessageBox.
type MessageLocation = (BoxId,Maybe BoxIx)

-- | Associate 'MessageLocation's to 'StatusIx's.
type StatusIxs = Map.Map MessageLocation StatusIx

-- | Reply context for 'Sync'/'Reply' instructions.
-- Associate the triggered MessageBox location to a one-use reply channel.
type ReplyCtx = Map.Map ChanId ReplyChan

-- | A Rule encapsulates the state of a single Join synchronisation
-- definition and is responsible for tracking all messages sent on the
-- contained channels and determining when triggers are to be ran.
--
-- - 'mkRule'     : Create a new rule representing a Join Definition
-- - 'addMessage' : Add a message on a contained 'ChanId', returning
--                  a 'Process's and 'ReplyCtx's if triggered.
data Rule = Rule
    { _ruleId       :: RuleId                          -- ^ Uniquely identify the 'Rule'
    , _chanMapping  :: ChanMapping                     -- ^ Map each contained ChanId to a BoxId
    , _messageBoxes :: MessageBoxes                    -- ^ Map each contained BoxId to a MessageBox
    , _statusIxs    :: StatusIxs                       -- ^ Associate 'MessageLocation's to StatusIx's (in the Status) caching their emptyness.
    , _status       :: Status                          -- ^ Cache the emptyness of all known MessageLocations used by the rule.
    , _patterns     :: [(StatusPattern,StoredPattern)] -- ^ Collection of StatusPatterns and corresponding StoredPatterns.
                                                       --   each StatusPattern can be quickly compared against the Status to determine whether a pattern
                                                       --   has been matched. If so, the associated StoredPattern describes how to perform the match.
                                                       --   (which messages should be taken, what order, whether they should be passed)
    } deriving Show

-- | From a list of 'PatternDescription's and associated 'TriggerF'
-- functions, build a corresponding 'Rule'.
mkRule :: [(PatternDescription,TriggerF Inert)] -> RuleId -> Rule
mkRule desc rId = rule
 where
    -- Every unique channelId passed in desc.
    cIds :: Set.Set ChanId
    cIds = Set.fromList $ map fst $ concatMap fst desc

    -- Map every ChanId to a BoxId
    chanMapping :: ChanMapping
    chanMapping = Bimap.fromList $ zip (Set.toList cIds) [0..]

    -- Map every (boxId,Nothing) location to a StatusIx.
    statusIxs :: StatusIxs
    statusIxs = Map.fromList $ zip (map (\boxId -> (boxId,Nothing)) $ Bimap.keysR chanMapping) [0..]

    -- Empty MessageBox for each BoxId.
    uninitialisedMessageBoxes :: MessageBoxes
    uninitialisedMessageBoxes = Map.fromList $ zip (Bimap.keysR chanMapping) (repeat emptyMessageBox)

    -- Complete built rule
    rule :: Rule
    rule = let uninitialisedRule = Rule
                 {_ruleId       = rId
                 ,_chanMapping  = chanMapping

                 ,_messageBoxes = uninitialisedMessageBoxes
                 ,_statusIxs    = statusIxs

                 -- Status can't be built until we know it's size, which we can't know until we know the total number of unique channelPatterns
                 -- Calculating the number of unique channel patterns
                 -- requires traversing the entire
                 -- description, so we might as well initialise the statusIxs at the same time (the mapping of boxIx's to statusIxs).
                 --
                 -- BoxIx's are only known once we add the corresponding subbox,
                 -- so we also initialise the messageBoxes at the same
                 -- time.
                 --
                 -- Initialising all of this data requires an input
                 -- structure similar to a Rule but without a status and
                 -- ruleId so instead of creating a new record, we pass an
                 -- unfinished rule with status=undefined. This is OK
                 -- because we don't touch the status, and overwrite it
                 -- before returning.
                 ,_status       = undefined
                 ,_patterns     = []
                 }
               (rl,nextStatusIx,storedPatterns) = foldr (\pc (accRl,nextStatusIx,storedPatterns)
                                                          -> let (accRl',nextStatusIx',storedPattern) = preparePatternClause pc accRl nextStatusIx
                                                                in (accRl',nextStatusIx',storedPatterns ++ [storedPattern])
                                                        )
                                                        (uninitialisedRule,Set.size cIds,[])
                                                        desc
               status                           = mkStatus (nextStatusIx) -- -1
               patterns                         = buildStatusPatterns storedPatterns (_statusIxs rl) (nextStatusIx) -- -1
             in rl{_status   = status
                  ,_patterns = patterns
                  }

    -- Given a list of StoredPatterns, the StatusIxs mapping and the size
    -- of the status, associate each StoredPattern with the StatusPattern
    -- that decides when it triggers.
    buildStatusPatterns :: [StoredPattern] -> StatusIxs -> Int -> [(StatusPattern,StoredPattern)]
    buildStatusPatterns storedPatterns statusIxs size = map (\storedPattern -> (buildStatusPattern (extractLocations storedPattern) statusIxs size,storedPattern)) storedPatterns
      where 
        extractLocations :: StoredPattern -> [(BoxId,Maybe BoxIx)]
        extractLocations (StoredPattern reqMsgs _) = map (\(boxId,mboxIx,_) -> (boxId,mboxIx)) reqMsgs

        buildStatusPattern :: [(BoxId,Maybe BoxIx)] -> StatusIxs -> Int -> StatusPattern
        buildStatusPattern locations statusIxs size =
          let reqIxs = foldr (\loc acc -> acc ++ [fromJust $ Map.lookup loc statusIxs]) [] locations
             in mkStatusPattern size reqIxs


    -- Given a single internal pattern clause (with associated TriggerF)
    -- build a corresponding StoredPattern while with a rule:
    -- - Adding appropriate subboxes to messageBoxes
    -- - Mapping the subBox MessageLocations to StatusIxs
    --
    -- In addition, thread an 'Int' representing the next free StatusIx.
    preparePatternClause :: ([(ChanId, MatchType)],TriggerF Inert) -- ^ Single internal pattern clause.
                         -> Rule -> Int -> (Rule,Int,StoredPattern)
    preparePatternClause (idesc,trigger) rl nextStatusIx =
      let (rl',nextStatusIx',reqMsgs) = foldr preparePatternClause' (rl,nextStatusIx,[]) idesc
         in (rl',nextStatusIx',StoredPattern (reverse reqMsgs) trigger)
      where
        preparePatternClause' :: (ChanId, MatchType) -> (Rule,Int,RequiredMessages) -> (Rule,Int,RequiredMessages)
        preparePatternClause' (chanId,matchType) (accRl, nextStatusIx,reqMsgs) =
           let boxId  = fromJust $ Bimap.lookup chanId (_chanMapping accRl)
               msgBox = fromJust $ Map.lookup boxId (_messageBoxes accRl)
              in case matchType of
                  MatchAll shouldPass -> (accRl
                                         ,nextStatusIx
                                         ,reqMsgs ++ [(boxId,Nothing,shouldPass)]
                                         )

                  MatchWhen pred shouldPass -> let (msgBox',boxIx) = addSubBox (\(m,_) -> pred (fromJust $ decodeMessage m)) msgBox
                                                  in (accRl{_messageBoxes = Map.insert boxId msgBox' (_messageBoxes accRl)
                                                           ,_statusIxs    = Map.insert (boxId,Just boxIx) nextStatusIx (_statusIxs accRl)
                                                           }
                                                     ,nextStatusIx+1
                                                     ,reqMsgs ++ [(boxId,Just boxIx,shouldPass)]
                                                     )

-- | Add a message on a contained 'ChanId', returning a 'Process' and
-- 'ReplyCtx' if triggered.
--
-- If the addition of the new Message causes a pattern to be matched then
-- the matching messages are applied to the corresponding trigger function
-- to produce a 'Process ()' to execute under the given 'ReplyCtx' (which
-- describes the specific reply location(s) of the messages which caused
-- the trigger).
addMessage :: Message -> ChanId -> Rule -> (Rule,Maybe (Process (),ReplyCtx))
addMessage msg cId rl


    -- Either: No specific patterns are matched (not in subBoxes) and theres
    -- at least one message on the channel.
    -- Or: Some specific patterns are matched but they're all already
    -- matched.
    --
    -- => status doesnt change => no new rules may fire so theres no need
    -- to check.
    {-| (inNoSubBoxes && alreadySomeMessages) || inNonEmptySubBoxesOnly = -}
        {-(rl{_messageBoxes = Map.insert boxId msgBox (_messageBoxes rl)},Nothing)-}

    -- New message doesnt match any specific patterns/ not in subBoxes
    -- and there are no messages already kept on the channel.
    -- => Update status & check for patterns waiting on this channel to
    -- fire.
    {-| inNoSubBovxes && noMessagesAlready =-}

    | otherwise =
        let -- Updated status with the index for each subBox the new msg is contained in set.
            status' = setMany (status `set` cStatusIx) (Set.foldr (\boxIx acc -> acc ++ [getStatusIx (boxId,Just boxIx) rl]) [] (containedIn strdMsg))

            -- Rule with new msg and status
            rl'     = rl{_messageBoxes = Map.insert boxId msgBox (_messageBoxes rl)
                        ,_status       = status'
                        }
           in case identifyMatches (_patterns rl') status' of

             -- No matching patterns
             [] -> (rl',Nothing)

             -- At least one match. Pick the first.
             (storedPattern:_) -> trace ("matched:" ++ show storedPattern) $
                let (rl'',procCtx) = applyMatch storedPattern rl'
                   in (rl'',Just procCtx)

  where
    inSomeSubBoxes         = not inNoSubBoxes
    inNoSubBoxes           = Set.null inBoxIxs
    alreadySomeMessages    = status `index` cStatusIx
    noMessagesAlready      = not alreadySomeMessages
    subBoxStatusChange     = Set.foldr (\ix b -> status `index` ix || b) False inStatusIxs
    noSubBoxStatusChange   = not subBoxStatusChange
    inNonEmptySubBoxesOnly = inSomeSubBoxes && noSubBoxStatusChange

    -- msgBox containing new strdMsg
    (msgBox,strdMsg) = insertMessage' msg (getChanMessageBox cId rl)
    boxId = getBoxId cId rl

    -- subBoxes the message is contained within
    inBoxIxs = containedIn strdMsg
    inStatusIxs = Set.map (\boxIx -> getStatusIx (boxId,Just boxIx) rl) inBoxIxs

    -- Status Bit-vector for rule BEFORE addition of this message
    status = _status rl

    -- StatusIx caching whether any messages are stored on the channel
    cStatusIx = getChanAnyIx cId rl


    -- | Identify all matching 'StatusPatterns' of a 'Status'.
    identifyMatches :: [(StatusPattern,StoredPattern)] -> Status -> [StoredPattern]
    identifyMatches ps status = trace "identifymatches" $ map snd $ filter (\(statusPattern,_) -> status `match` statusPattern) ps

    -- | Given a pattern which is claimed to have been matched, collect the
    -- matching messages apply them to the matching trigger function to
    -- produce a 'Process ()' to be executed under the ReplyCtx.
    applyMatch :: StoredPattern -> Rule -> (Rule,(Process(),ReplyCtx))
    applyMatch (StoredPattern reqMsgs (TriggerF trigger)) rl =
        let (rl',rawMsgs, replyCtx) = takeMessages reqMsgs rl
           in (rl',(unsafeApply trigger rawMsgs,replyCtx))

    -- Currently not unsetting sub-box indexes when removed
    takeMessages :: RequiredMessages -> Rule -> (Rule,[RawMessage],ReplyCtx)
    takeMessages reqMsgs rl = foldr f (rl,[],Map.empty) reqMsgs
      where f :: RequiredMessage -> (Rule,[RawMessage],ReplyCtx) -> (Rule,[RawMessage],ReplyCtx)
            f reqMsg@(boxId,_,_) (accRl,accRawMsgs,accReplyCtx) =
              let (accRl',mRawMsg,mReplyChan) = takeMessage reqMsg accRl
                 in (accRl'
                    ,maybe accRawMsgs (\rm -> accRawMsgs ++ [rm]) mRawMsg
                    ,maybe accReplyCtx (\replyChan -> Map.insert (getChanId boxId accRl') replyChan accReplyCtx) mReplyChan
                    )

    -- Given a RequiredMessage description (which is known will succeed)
    -- , take the Message from a Rule.
    takeMessage :: RequiredMessage -> Rule -> (Rule,Maybe RawMessage, Maybe ReplyChan)
    takeMessage (boxId,mBoxIx,shouldPass) rl =
        let msgBox          = getMessageBox boxId rl
            (strdMsg,
             newlyEmptySubBoxes,
             msgBox'
             )              = fromMaybe (error "No such StoredMessage") $ take mBoxIx msgBox
            allMsgIx        = getStatusIx (boxId,Nothing) rl
            status'         = if noStoredMessages msgBox' then unset (_status rl) allMsgIx else _status rl
            newStatus       = Set.foldr (\ix st -> unset st $ getStatusIx (boxId,Just ix) rl) status' newlyEmptySubBoxes
            newMessageBoxes = Map.insert boxId msgBox' (_messageBoxes rl)
           in (rl{_messageBoxes = newMessageBoxes
                 ,_status       = newStatus
                 }
              ,if shouldPass then Just $ fst $ message strdMsg else Nothing
              ,snd $ message strdMsg
              )

-- | Get the MessageBox associated with the ChanId
getChanMessageBox :: ChanId -> Rule -> MessageBox
getChanMessageBox cId rl =
    let boxId = getBoxId cId rl
       in getMessageBox boxId rl

-- | Get the StatusIx associated with tracking whether there are any
-- messages on a channel.
getChanAnyIx :: ChanId -> Rule -> StatusIx
getChanAnyIx cId rl =
    let boxId     = getBoxId cId rl
        mStatusIx = Map.lookup (boxId,Nothing) (_statusIxs rl)
       in fromMaybe (error "BoxId has no StatusIx") mStatusIx

-- | Get the BoxId associated with a ChanId
getBoxId :: ChanId -> Rule -> BoxId
getBoxId cId rl =
    let mBoxId = Bimap.lookup cId (_chanMapping rl)
       in fromMaybe (error "ChanId has no BoxId") mBoxId

-- | Get the ChanId associated with a BoxId
getChanId :: BoxId -> Rule -> ChanId
getChanId boxId rl =
    let mChanId = Bimap.lookupR boxId (_chanMapping rl)
       in fromMaybe (error "BoxId has no ChanId") mChanId

-- | Get the MessageBox associated with a BoxId
getMessageBox :: BoxId -> Rule -> MessageBox
getMessageBox boxId rl =
    let mMsgBox = Map.lookup boxId (_messageBoxes rl)
       in fromMaybe (error "BoxId has no MessageBox") mMsgBox

-- | Get the StatusIx associated with a MessageLocation
getStatusIx :: MessageLocation -> Rule -> StatusIx
getStatusIx mLoc rl =
    let mStIx = Map.lookup mLoc (_statusIxs rl)
       in fromMaybe (error "MessageLocation has no StatusIx") mStIx

-- | Format a human-readable representation of a 'Rule'.
showRule :: Rule -> String
showRule rl = "Rule " ++ showRuleId (_ruleId rl) ++ "{ \n"
           ++ showChanMapping (_chanMapping rl)
           ++ "Status:\n" ++ showStatus (_status rl)
           ++ showStatusIxs (_statusIxs rl)
           ++ showPatterns (_patterns rl)
           ++ showMessageBoxes (_messageBoxes rl)
           ++ "\n}\n"

showRuleId :: RuleId -> String
showRuleId (RuleId rId) = "rId"++show rId

showChanMapping :: ChanMapping -> String
showChanMapping cm = "ChanId <-> BoxId:\n"
                  ++ concatMap showChanMap (Bimap.toList cm)
                  ++ "\n"

showChanMap :: (ChanId,BoxId) -> String
showChanMap (ChanId cId,BoxId bId) = "cId"++ show cId ++ "  <->" ++ " bId"++ show bId ++ "\n"

showStatusIxs :: StatusIxs -> String
showStatusIxs ixs = "\n\nMessageLocation -> StatusIx:\n"
                 ++ (intercalate "\n" $ map showStatusIx (Map.toList ixs))

showStatusIx :: (MessageLocation,StatusIx) -> String
showStatusIx ((BoxId bId,mBoxIx),statusIx) = show bId ++ "," ++ (maybe "_" (\(BoxIx ix) -> show ix) mBoxIx) ++ " -> " ++ show statusIx

showPatterns :: [(StatusPattern,StoredPattern)] -> String
showPatterns ps = "\n\nStatusPattern => StoredPattern\n"
               ++ concatMap showPattern ps

showPattern :: (StatusPattern,StoredPattern) -> String
showPattern (statusPattern,storedPattern) = showStatusPattern statusPattern ++ " => " ++ showStoredPattern storedPattern ++ "\n"

showStoredPattern :: StoredPattern -> String
showStoredPattern (StoredPattern reqMsgs _) = intercalate " THEN " $ map showRequiredMessage reqMsgs

showRequiredMessage :: RequiredMessage -> String
showRequiredMessage (BoxId bId,mBoxIx,shouldPass) = (if shouldPass then "pass " else "keep ")
                                             ++ show bId ++ "," ++ maybe "_" (\(BoxIx bIx) -> show bIx) mBoxIx

showMessageBoxes :: MessageBoxes -> String
showMessageBoxes msgBoxes = "\n\n" ++ (intercalate "\n\n" $ map (\(BoxId bId,msgBox) -> "bId"++show bId ++ " :\n" ++ showMessageBox msgBox) (Map.toList msgBoxes))

