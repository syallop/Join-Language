{-# LANGUAGE DataKinds
            ,FlexibleContexts
            ,FlexibleInstances
            ,GADTs
            ,KindSignatures
            ,ImpredicativeTypes
            ,OverloadedLists
            ,OverloadedStrings
            ,RankNTypes
            ,ScopedTypeVariables
            ,TypeSynonymInstances
            #-}
module Join.Interpreter.Basic.Rule
    (Rule()
    ,ReplyCtx
    ,ReplyChan
    ,RuleId(..)
    ,BoxId(..)

    ,mkRule
    ,addMessage

    ,SomeReplyChan(..)

    ,_chanMapping
    ,showRule
    ) where

import Join
import Join.Pattern.Rep

import Join.Interpreter.Basic.Debug
import Join.Interpreter.Basic.StoredDefinitions
import Join.Interpreter.Basic.MessageBox
import Join.Interpreter.Basic.Status

import           Control.Arrow
import           Data.Dynamic
import qualified Data.Bimap     as Bimap
import           Data.List               (intercalate)
import qualified Data.Map       as Map
import qualified Data.Set       as Set
import           Data.Typeable
import           Data.Maybe

import Prelude hiding (take)

-- | System-wide unique rule id.
newtype RuleId = RuleId {unRuleId :: Int} deriving Show

-- | Internal representation of a single clause of a join definition.
-- Encapsulates a 'RequiredMessages' describing the messages required for
-- the clause to match alongside a 'TriggerF' - the trigger function
-- the matching messages should be passed to.
{-data StoredPattern = StoredPattern RequiredMessages (TriggerF Inert) deriving Show-}

-- | Ordered list of 'RequiredMessage's
{-type RequiredMessages = [RequiredMessage]-}

-- | MessageBoxId, possible subBox index, whether message should be passed
-- to trigger.
type RequiredMessage = (BoxId,Maybe BoxIx,Bool)

-- | A 'StatusIx' is an int index into a 'Status'.
type StatusIx = Int

-- | Associate 'ChanId's to 'BoxId's.
type ChanMapping = Bimap.Bimap ChanId BoxId

-- | Associate 'BoxId's to 'MessageBox's.
type MessageBoxes = Map.Map BoxId SomeMessageBox
data SomeMessageBox = forall s a. (MessageType a,Typeable (MessageBox s a)) => SomeMessageBox (MessageBox s a)

-- | Describes a location where messages may be retrieved.
-- Nothing => Any message from the corresponding MessageBox.
type MessageLocation = (BoxId,Maybe BoxIx)

-- | Associate 'MessageLocation's to 'StatusIx's.
type StatusIxs = Map.Map MessageLocation StatusIx

-- | Reply context for 'Sync'/'Reply' instructions.
-- Associate the triggered MessageBox location to a one-use reply channel.
type ReplyCtx = Map.Map ChanId SomeReplyChan
data SomeReplyChan = forall r. Typeable r => SomeReplyChan (ReplyChan r)

-- | A Rule encapsulates the state of a single Join synchronisation
-- definition and is responsible for tracking all messages sent on the
-- contained channels and determining when triggers are to be ran.
--
-- - 'mkRule'     : Create a new rule representing a Join Definition
-- - 'addMessage' : Add a message on a contained 'ChanId', returning
--                  a 'Process's and 'ReplyCtx's if triggered.
data Rule tss refine = Rule
    { _ruleId       :: RuleId                          -- ^ Uniquely identify the 'Rule'
    , _chanMapping  :: ChanMapping                     -- ^ Map each contained ChanId to a BoxId
    , _messageBoxes :: MessageBoxes                    -- ^ Map each contained BoxId to a MessageBox
    , _statusIxs    :: StatusIxs                       -- ^ Associate 'MessageLocation's to StatusIx's (in the Status) caching their emptyness.
    , _status       :: Status                          -- ^ Cache the emptyness of all known MessageLocations used by the rule.
    , _patterns     :: StoredDefinitions tss refine
                                                       -- ^ Collection of StatusPatterns and corresponding StoredPatterns.
                                                       --   each StatusPattern can be quickly compared against the Status to determine whether a pattern
                                                       --   has been matched. If so, the associated StoredPattern describes how to perform the match.
                                                       --   (which messages should be taken, what order, whether they should be passed)
    }


-- | From a list of 'PatternDescription's and associated 'TriggerF'
-- functions, build a corresponding 'Rule'.
mkRule :: forall tss. Definitions tss Inert -> RuleId -> Rule tss StatusPattern
mkRule definitions rId =
  let -- Every unique channelId passed in desc.
      cIds :: Set.Set ChanId
      cIds = uniqueIds definitions

      -- Map every ChanId to a BoxId
      chanMapping :: ChanMapping
      chanMapping = Bimap.fromList $ zip (Set.toList cIds) [0..]

      -- Map every (boxId,Nothing) location to a StatusIx.
      statusIxs :: StatusIxs
      statusIxs = Map.fromList $ zip (map (\boxId -> (boxId,Nothing)) $ Bimap.keysR chanMapping) [0..]

      messageBoxes :: MessageBoxes
      messageBoxes = Map.empty

      firstStatusIx :: Int
      firstStatusIx = Set.size cIds

      -- Complete built rule
      ((chanMapping',
        messageBoxes',
        statusIxs',
        lastStatusIx
       ),
       storedDefinitions
       ) = initialiseDefinitions definitions (chanMapping,messageBoxes,statusIxs,firstStatusIx)
     in Rule{_ruleId       = rId
            ,_chanMapping  = chanMapping'
            ,_messageBoxes = messageBoxes'
            ,_statusIxs    = statusIxs'
            ,_status       = mkStatus lastStatusIx
            ,_patterns     = tagStatusPatterns storedDefinitions chanMapping' statusIxs' lastStatusIx
            }
  where

    initialiseDefinitions :: Definitions tss Inert
                          -> (ChanMapping,MessageBoxes,StatusIxs,Int)
                          -> ((ChanMapping,MessageBoxes,StatusIxs,Int),StoredDefinitions tss ())
    initialiseDefinitions definitions acc = storeDefinitionsWith assignBoxIx acc definitions
      where
        assignBoxIx :: forall (s :: Synchronicity *) m tss. (MessageType m,Typeable s)
                    => Channel s m
                    -> Match m
                    -> (ChanMapping,MessageBoxes,StatusIxs,Int)
                    -> ((ChanMapping,MessageBoxes,StatusIxs,Int), Maybe BoxIx)
        assignBoxIx chan match (chanMapping,messageBoxes,statusIxs,nextStatusIx) =
          let boxId           = fromJust $ Bimap.lookup (getId chan) chanMapping
              msgBox          = takeMessageBox' boxId messageBoxes :: MessageBox s m --tospecialise
             in case match of
                MatchAll -> ((chanMapping
                             ,Map.insert boxId (SomeMessageBox (msgBox :: MessageBox s m)) messageBoxes
                             ,statusIxs
                             ,nextStatusIx
                             )
                            ,Nothing
                            )
                MatchWhen pred -> let (msgBox',boxIx) = addSubBox pred msgBox
                                     in ((chanMapping
                                         ,Map.insert boxId (SomeMessageBox msgBox') messageBoxes
                                         ,Map.insert (boxId,Just boxIx) nextStatusIx statusIxs
                                         ,nextStatusIx+1
                                         )
                                        ,Just boxIx
                                        )

    -- Tag prepared definitions with StatusPattern's which decide when they trigger.
    tagStatusPatterns :: StoredDefinitions tss () -> ChanMapping -> StatusIxs -> Int -> StoredDefinitions tss StatusPattern
    tagStatusPatterns sdr cM statusIxs size = mapStoredDefinitions (assignStatusPattern (statusIxs,size)) sdr
      where
        assignStatusPattern :: (StatusIxs,Int) -> StoredPatterns ts -> StatusPattern
        assignStatusPattern (statusIxs,size) spsr
          = let locations = foldStoredPatterns extractLocations [] spsr
               in buildStatusPattern locations statusIxs size

        extractLocations :: MessageType m => Channel (s :: Synchronicity *) m -> Maybe BoxIx -> ShouldPass p -> [MessageLocation] -> [MessageLocation]
        extractLocations c m sp acc = acc ++ [((fromJust $ Bimap.lookup (getId c) cM),m)]

        buildStatusPattern :: [MessageLocation]-> StatusIxs -> Int -> StatusPattern
        buildStatusPattern locations statusIxs size =
          let reqIxs = map (\loc -> fromJust $ Map.lookup loc statusIxs) locations
             in mkStatusPattern size reqIxs

-- | Add a message on a contained 'ChanId', returning a 'Process' and
-- 'ReplyCtx' if triggered.
--
-- If the addition of the new Message causes a pattern to be matched then
-- the matching messages are applied to the corresponding trigger function
-- to produce a 'Process ()' to execute under the given 'ReplyCtx' (which
-- describes the specific reply location(s) of the messages which caused
-- the trigger).
addMessage :: forall s a tss
            . (MessageType a,Typeable s)
           => Message s a -> ChanId -> Rule tss StatusPattern -> (Rule tss StatusPattern,Maybe (Process (),ReplyCtx))
addMessage msg cId rl

    | otherwise =
        let -- Updated status with the index for each subBox the new msg is contained in set.
            status' = setMany (status `set` cStatusIx) (Set.foldr (\boxIx acc -> acc ++ [takeStatusIx (boxId,Just boxIx) (_statusIxs rl)]) [] (containedIn strdMsg))

            -- Rule with new msg and status
            rl'     = rl{_messageBoxes = Map.insert boxId (SomeMessageBox msgBox) (_messageBoxes rl)
                        ,_status       = status'
                        }
           in identifyMatches (_patterns rl') rl'
  where
    inSomeSubBoxes         = not inNoSubBoxes
    inNoSubBoxes           = Set.null inBoxIxs
    alreadySomeMessages    = status `index` cStatusIx
    noMessagesAlready      = not alreadySomeMessages
    subBoxStatusChange     = Set.foldr (\ix b -> status `index` ix || b) False inStatusIxs
    noSubBoxStatusChange   = not subBoxStatusChange
    inNonEmptySubBoxesOnly = inSomeSubBoxes && noSubBoxStatusChange

    -- msgBox containing new strdMsg
    (msgBox,strdMsg) = insertMessage' msg (takeChanMessageBox cId (_chanMapping rl) (_messageBoxes rl))
    boxId = takeBoxId cId (_chanMapping rl)

    -- subBoxes the message is contained within
    inBoxIxs = containedIn strdMsg

    inStatusIxs = Set.map (\boxIx -> takeStatusIx (boxId,Just boxIx) (_statusIxs rl)) inBoxIxs

    -- Status Bit-vector for rule BEFORE addition of this message
    status = _status rl

    -- StatusIx caching whether any messages are stored on the channel
    cStatusIx = takeChanCatchAll cId (_chanMapping rl) (_statusIxs rl)

    -- | Identify all matching 'StatusPatterns' of a 'Status'.
    identifyMatches :: forall tss'. StoredDefinitions tss' StatusPattern
                    -> Rule tss StatusPattern
                    -> (Rule tss StatusPattern,Maybe (Process (),ReplyCtx))
    identifyMatches (OneStoredDefinition sdr)      rl = identifyMatches' sdr rl
    identifyMatches (AndStoredDefinition sdr sdrs) rl = case identifyMatches' sdr rl of
      (rl',Nothing) -> identifyMatches sdrs rl'
      (rl',Just r ) -> (rl',Just r)

    identifyMatches' :: forall ts tr tss
                      .StoredDefinition ts tr StatusPattern
                     -> Rule tss StatusPattern
                     -> (Rule tss StatusPattern, Maybe (Process (),ReplyCtx))
    identifyMatches' (StoredDefinition spr (Trigger tr) statusPattern) rl
      | (_status rl) `match` statusPattern =
          let (status',msgBoxes',msgs,replyCtx) = takeRequestedMessages spr (_chanMapping rl) (_statusIxs rl) (_status rl) (_messageBoxes rl)
             in (rl{_status       = status'
                   ,_messageBoxes = msgBoxes'
                   }
                ,Just (unsafeApply tr msgs
                      ,replyCtx)
                )
      | otherwise = (rl,Nothing)

takeRequestedMessages :: StoredPatterns ts
                      -> ChanMapping
                      -> StatusIxs
                      -> Status
                      -> Map.Map BoxId SomeMessageBox
                      -> (Status,Map.Map BoxId SomeMessageBox,[Dynamic],ReplyCtx)
takeRequestedMessages spsr chanMapping statusIxs status msgBoxes =
  let (_,_,status',msgBoxes',msgs,replyCtx) = foldStoredPatterns takeMessage (chanMapping,statusIxs,status,msgBoxes,[],Map.empty) spsr
     in (status',msgBoxes',msgs,replyCtx)

takeMessage :: forall m s p. (MessageType m,Typeable s)
            => Channel (s::Synchronicity *) m
            -> Maybe BoxIx
            -> ShouldPass p
            -> (ChanMapping,StatusIxs,Status,Map.Map BoxId SomeMessageBox,[Dynamic],ReplyCtx)
            -> (ChanMapping,StatusIxs,Status,Map.Map BoxId SomeMessageBox,[Dynamic],ReplyCtx)
takeMessage chan mBoxIx shouldPass (chanMapping,statusIxs,status,msgBoxes,msgs,replyCtx) =
  let boxId    = fromJust $ Bimap.lookup (getId chan) chanMapping -- todo
      msgBox   = takeMessageBoxAs boxId msgBoxes :: MessageBox s m
      (strdMsg,
       newlyEmptySubBoxes,
       msgBox'
       )       = fromMaybe (error "No such StoredMessage") $ take mBoxIx msgBox
      allMsgIx = takeStatusIx (boxId,Nothing) statusIxs
      status'  = if noStoredMessages msgBox' then status `unset` allMsgIx else status
      newStatus = Set.foldr (\ix st -> unset st $ takeStatusIx (boxId,Just ix) statusIxs) status' newlyEmptySubBoxes
      newMessageBoxes = Map.insert boxId (SomeMessageBox msgBox') msgBoxes
     in (chanMapping
        ,statusIxs
        ,newStatus
        ,newMessageBoxes
        ,case shouldPass of
           DoPass   -> (msgs ++ [toDyn $ unMessage $ message strdMsg])
           DontPass -> msgs
        ,case message strdMsg of
            (Message _) -> replyCtx
            (SyncMessage _ rchan) -> Map.insert (takeChanId boxId chanMapping) (SomeReplyChan rchan) replyCtx
        )


takeStatusIx :: MessageLocation -> StatusIxs -> StatusIx
takeStatusIx mLoc statusIxs =
    let mStIx = Map.lookup mLoc statusIxs
       in fromMaybe (error "MessageLocation has no StatusIx") mStIx

takeChanId :: BoxId -> ChanMapping -> ChanId
takeChanId boxId chanMapping =
    let mChanId = Bimap.lookupR boxId chanMapping
       in fromMaybe (error "BoxId has no ChanId") mChanId

takeMessageBoxAs :: Typeable (MessageBox s a) => BoxId -> MessageBoxes -> MessageBox s a
takeMessageBoxAs boxId msgBoxes = case Map.lookup boxId msgBoxes of
  Nothing -> error $ "takeMessageBoxAs: " ++ show boxId ++ " not contained in messagebox collection"
  Just (SomeMessageBox msgBox) -> case cast msgBox of
    Nothing -> error $ "takeMessageBoxAs: Invalid typecast"
    Just msgBox' -> msgBox'

-- | Get the MessageBox associated with the ChanId
takeChanMessageBox :: Typeable (MessageBox s a) => ChanId -> ChanMapping -> MessageBoxes -> MessageBox s a
takeChanMessageBox cId chanMapping messageBoxes =
    let boxId = takeBoxId cId chanMapping
       in takeMessageBoxAs boxId messageBoxes

-- | Get the StatusIx associated with tracking whether there are any
-- messages on a channel.
takeChanCatchAll :: ChanId -> ChanMapping -> StatusIxs -> StatusIx
takeChanCatchAll cId chanMapping statusIxs =
    let boxId     = takeBoxId cId chanMapping
        mStatusIx = Map.lookup (boxId,Nothing) statusIxs
       in fromMaybe (error "BoxId has no StatusIx") mStatusIx

-- | Get the BoxId associated with a ChanId
takeBoxId :: ChanId -> ChanMapping -> BoxId
takeBoxId cId chanMapping =
    let mBoxId = Bimap.lookup cId chanMapping
       in fromMaybe (error "ChanId has no BoxId") mBoxId

takeMessageBox' :: Typeable (MessageBox s a) => BoxId -> MessageBoxes -> MessageBox s a
takeMessageBox' boxId messageBoxes = case Map.lookup boxId messageBoxes of
  Nothing -> emptyMessageBox
  Just (SomeMessageBox msgBox) -> case cast msgBox of
    Nothing -> error "getMessageBox: MessageBox doesnt have requested type"
    Just msgBox' -> msgBox'


-- | Format a human-readable representation of a 'Rule'.
showRule :: Rule tss StatusPattern -> String
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

showPatterns :: StoredDefinitions tss StatusPattern -> String
showPatterns sdr = foldStoredDefinitions showPattern "\n\nRefine => StoredPattern\n" sdr

showPattern :: forall ts tr. StoredDefinition ts tr StatusPattern -> String -> String
showPattern (StoredDefinition spr tr statusPattern) acc = acc ++ (showStatusPattern statusPattern) ++ " => " ++ (showStoredPatterns spr) ++ "\n"

showStoredPatterns :: StoredPatterns ts -> String
showStoredPatterns spsr = foldStoredPatterns showStoredPattern "" spsr

showStoredPattern :: (MessageType m,Typeable s) => Channel (s :: Synchronicity *) m -> Maybe BoxIx -> ShouldPass p -> String -> String
showStoredPattern chan mBoxIx sp acc = acc ++ show chan ++ show mBoxIx

showRequiredMessage :: RequiredMessage -> String
showRequiredMessage (BoxId bId,mBoxIx,shouldPass) = (if shouldPass then "pass " else "keep ")
                                             ++ show bId ++ "," ++ maybe "_" (\(BoxIx bIx) -> show bIx) mBoxIx

showMessageBoxes :: MessageBoxes -> String
showMessageBoxes msgBoxes = "\n\n MessageBoxes:\n" ++ (intercalate "\n\n" $ map (\(BoxId bId,SomeMessageBox msgBox) -> "bId"++show bId ++ " :\n" ++ showMessageBox msgBox) (Map.toList msgBoxes))

