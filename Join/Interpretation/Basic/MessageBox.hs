{-# LANGUAGE GeneralizedNewtypeDeriving
           , OverloadedStrings
           , FlexibleInstances
 #-}
{-|
Module     : Join.Interpreter.Basic.MessageBox
Copyright  : (c) Samuel A. Yallop, 2014
Maintainer : syallop@gmail.com
Stability  : experimental

This module provides a data structure for storing messages which are automatically
sub-categorised by arbitrary predicates.
-}

module Join.Interpretation.Basic.MessageBox 
    (
    -- * Types
     Message
    ,RawMessage
    ,ReplyChan
    ,BoxId(..)
    ,BoxIx(..),BoxIxs
    ,MsgIx(),MsgIxs
    ,MessagePredicate(),MessagePredicates()

    -- * MessageBox
    ,MessageBox(),StoredMessage(),StoredMessages(),SubBoxes()
    ,emptyMessageBox

    -- ** Adding Messages/ subBoxes
    ,addSubBox
    ,insertMessage
    ,insertMessage'

    -- ** Removing Messages
    ,take
    ,takeAny
    ,takeFrom

    -- ** Misc Operations
    ,message
    ,allStoredMessages
    ,matches
    ,noStoredMessages
    ,valid
    ,containedIn
    ,showMessageBox
    ) where

import Join.Interpretation.Basic.Status

import           Control.Concurrent.MVar
import qualified Data.ByteString         as BS
import           Data.Coerce
import           Data.List                      hiding (take)
import qualified Data.Map                as Map
import           Data.Maybe
import qualified Data.Set                as Set

import Prelude hiding (take)

{- Types -}

-- | A 'Message' contains a 'RawMessage' and 'Maybe' a 'ReplyChan' if the
-- message was sent to a synchronous Channel which expects a response.
type Message = (RawMessage,Maybe ReplyChan)

-- | A 'RawMessage' is an encoding of the data comprising a message.
type RawMessage = BS.ByteString

-- | A 'ReplyChan' is an 'MVar' where a 'reply' value to a synchronous
-- message should be placed.
type ReplyChan = MVar RawMessage

newtype BoxId = BoxId {unBoxId :: Int} deriving (Eq,Ord,Enum,Num,Show) -- ^ System-wide unique 'MessageBox' id.
newtype BoxIx = BoxIx {unBoxIx :: Int} deriving (Eq,Ord,Enum,Num,Show) -- ^ MessageBox-wide unique box index.
newtype MsgIx = MsgIx {unMsgIx :: Int} deriving (Eq,Ord,Enum,Num) -- ^ MessageBox-wide unique Message index.

type BoxIxs = Set.Set BoxIx -- ^ A 'Set' of 'BoxIx'
type MsgIxs = Set.Set MsgIx -- ^ A 'Set' of 'MsgIx'

-- | A 'MessagePredicate' is a predicate that decides membership within
-- a 'BoxIx'.
newtype MessagePredicate = MessagePredicate {unMessagePredicate :: (Message -> Bool,BoxIx)}
instance Eq  MessagePredicate where (MessagePredicate (_,ix0))  ==       (MessagePredicate (_,ix1)) = ix0  ==       ix1
instance Ord MessagePredicate where (MessagePredicate (_,ix0)) `compare` (MessagePredicate (_,ix1)) = ix0 `compare` ix1

-- | 'MessagePredicates' is a 'Set' of 'MessagePredicate'.
-- It is a collection of predicated on Messages which decide membership
-- within a 'BoxIx' where each 'BoxIx' is decided by (at most) one predicate.
newtype MessagePredicates = MessagePredicates { unMessagePredicates :: (Set.Set MessagePredicate)}



{- 'MessageBox' creation -}

-- | A 'MessageBox' stores 'Messages' which are automatically sorted into
-- subBoxes by declared 'MessagePredicate's.
--
-- 'emptyMessageBox' - initialise a new empty 'MessageBox'
-- 'addSubBox'       - supply a sorting predicate and have existing and new
--                     'Message's sorted into a subBox.
-- 'insertMessage'   - insert a new message, automatically sorting it into
--                     appropriate subBoxes
--
-- 'takeAny'         - attempt to take any held message
-- 'takeFrom'        - attempt to take a message stored in a given subBox
data MessageBox = MessageBox
    { _storedMessages   :: StoredMessages    -- ^ Associate all 'MsgIx's to their 'StoredMessage'
    , _subBoxes         :: SubBoxes          -- ^ Associate sub' BoxIx's to sets of 'MsgIx's they contain.
    , _subBoxPredicates :: MessagePredicates -- ^ List of predicates which decide a Messages membership
                                             --   within SubBox's.

    , _freshMsgIx :: MsgIx -- ^ Next 'MsgIx' to use.
    , _freshBoxIx :: BoxIx -- ^ Next 'BoxIx' to use.
    }
instance Show MessageBox where
    show = showMessageBox

-- | A Message as stored within some 'MessageBox' with meta-data.
data StoredMessage = StoredMessage
    { _msgIx         :: MsgIx   -- ^ Index, unique within a 'MessageBox' used to refer to this 'StoredMessage'
    , _containedIn   :: BoxIxs  -- ^ 'Set' of 'BoxIx's the message is contained within.
    , _message       :: Message -- ^ The encapsulated 'Message' itself.
    } deriving Eq
instance Ord StoredMessage where compare (StoredMessage ix0 _ _) (StoredMessage ix1 _ _) = compare ix0 ix1

type StoredMessages = Map.Map MsgIx StoredMessage -- ^ A Collection of 'StoredMessage's is indexed
                                                  -- by their 'MsgIx'.
type SubBoxes       = Map.Map BoxIx MsgIxs        -- ^ A Collection of sub 'BoxIx's is a 'Map' of 'BoxIx's
                                                  -- to the 'MsgIxs' they contain.

-- | The initial empty 'MessageBox' containing no messages and with no
-- declared subBoxes.
emptyMessageBox :: MessageBox
emptyMessageBox = MessageBox
    {_storedMessages   = Map.empty
    ,_subBoxes         = Map.empty
    ,_subBoxPredicates = MessagePredicates Set.empty
    ,_freshMsgIx       = MsgIx 0
    ,_freshBoxIx       = BoxIx 0
    }


{- Adding Messages/ subBoxes -}

-- | Add a new subBox to a 'MessageBox' which contains Messages matching the
-- given predicate. Return the updated 'MessageBox' (with existing stored
-- messages sorted into the new subBox if matching) with the 'BoxIx' used to
-- reference to the subBox.
addSubBox :: (Message -> Bool) -> MessageBox -> (MessageBox,BoxIx)
addSubBox pred msgBox =
    (msgBox {_storedMessages   = newStoredMessages
            ,_subBoxes         = newSubBoxes
            ,_freshBoxIx       = boxIx + 1
            ,_subBoxPredicates = newSubBoxPredicates
            }
    ,boxIx
    )
  where
    boxIx               = _freshBoxIx msgBox

    (matchingMessages,
     newStoredMessages) = updateStoredMessages (_storedMessages msgBox) pred boxIx
    newSubBoxes         = Map.insert boxIx matchingMessages (_subBoxes msgBox)

    newSubBoxPredicates = insertMessagePredicate (MessagePredicate (pred,boxIx)) (_subBoxPredicates msgBox)

    -- Given a map of stored messages and a predicate for a new 'BoxIx'
    -- , update matching 'StoredMessages' to cache their new membership as well as
    -- returning the 'MsgIx's which were changed because they match
    -- the new predicate.
    updateStoredMessages :: StoredMessages -> (Message -> Bool) -> BoxIx -> (MsgIxs,StoredMessages)
    updateStoredMessages strdMsgs pred subBoxIx = Map.mapAccumWithKey
        (\acc msgIx strdMsg -> if matches pred strdMsg
            then let newAcc     = Set.insert msgIx acc
                     newStrdMsg = strdMsg{_containedIn = Set.insert subBoxIx (_containedIn strdMsg)}
                   in (newAcc,newStrdMsg)
            else (acc,strdMsg)
        )
        Set.empty
        strdMsgs

    -- Insert a 'MessagePredicate' within a 'MessagePredicates' set.
    insertMessagePredicate :: MessagePredicate -> MessagePredicates -> MessagePredicates
    insertMessagePredicate mp (MessagePredicates mps) = MessagePredicates $ Set.insert mp mps

insertMessage :: Message -> MessageBox -> MessageBox
insertMessage msg msgBox = fst $ insertMessage' msg msgBox

-- | Store the given message in the 'MessageBox', internally sorting the
-- message into any existing subBoxes by their corresponding predicate.
insertMessage' :: Message -> MessageBox -> (MessageBox,StoredMessage)
insertMessage' msg msgBox =
    let msgIx       = _freshMsgIx msgBox                           -- New 'MsgIx'
        withinBoxes = whichSubBoxes msg (_subBoxPredicates msgBox) -- Set of 'BoxIx's message should be contained in
        strdMessage = StoredMessage msgIx withinBoxes msg          -- Built 'StoredMessage' value
       in (registerStoredMessage strdMessage msgBox,strdMessage)
  where
    -- Register a (correct) 'StoredMessage' within a 'MessageBox',
    -- adding it to the global storedMessages and within the subBoxes it
    -- claims membership of.
    registerStoredMessage :: StoredMessage -> MessageBox -> MessageBox
    registerStoredMessage sm msgBox =
        msgBox{_storedMessages = Map.insert (_msgIx sm) sm (_storedMessages msgBox)
              ,_subBoxes       = Map.unionWith Set.union
                                               (Map.fromSet (const $ Set.singleton (_msgIx sm)) (_containedIn sm))
                                               (_subBoxes msgBox)
              ,_freshMsgIx = _freshMsgIx msgBox + 1
              }


{- Removing Messages -}

-- | Nothing => Seek any message.
--   Just ix => Seek messages in the given sub 'BoxIx'.
--
-- If there is a message stored at the requested location, then remove it,
-- returning a tuple of:
-- - The removed message
-- - The subBoxes which are now empty as a result of the removal
-- - The updated 'MessageBox'
take :: Maybe BoxIx -> MessageBox -> Maybe (StoredMessage,BoxIxs,MessageBox)
take Nothing      = takeAny
take (Just boxIx) = takeFrom boxIx

-- | If there is a message stored in the 'MessageBox', then remove it,
-- returning a tuple of:
-- - The removed message
-- - The subBoxes which are now empty as a result of the removal
-- - The updated 'MessageBox'
takeAny :: MessageBox -> Maybe (StoredMessage,BoxIxs,MessageBox)
takeAny msgBox = do
    (sm@(StoredMessage msgIx inSBs msg),newStoredMessages) <- Map.minView (_storedMessages msgBox)
    let (newSubBoxes,newlyEmptySubBoxes) = removeFromSubBoxes' inSBs msgIx (_subBoxes msgBox)
    return (sm
           ,newlyEmptySubBoxes
           ,msgBox{_storedMessages = newStoredMessages
                  ,_subBoxes       = newSubBoxes
                  }
           )
-- | If there is message stored in the 'MessageBox' at the given subBoxIx,
-- then remove it, returning a tuple of:
-- - The removed message
-- - The subBoxes which are now empty as a result of the removal
-- - The updated 'MessageBox'.
takeFrom :: BoxIx -> MessageBox -> Maybe (StoredMessage,BoxIxs,MessageBox)
takeFrom boxIx msgBox = do
    sm@(StoredMessage msgIx inSBs msg) <- anyStoredMessageInBox boxIx msgBox
    let (newSubBoxes,newlyEmptySubBoxes) = removeFromSubBoxes' inSBs msgIx (_subBoxes msgBox)
        newStoredMessages                = Map.delete msgIx (_storedMessages msgBox)

    return (sm
           ,newlyEmptySubBoxes
           ,msgBox{_storedMessages = newStoredMessages
                  ,_subBoxes       = newSubBoxes
                  }
           )
  where
    -- Lookup any 'StoredMessage' contained within the 'BoxIx'.
    anyStoredMessageInBox :: BoxIx -> MessageBox -> Maybe StoredMessage
    anyStoredMessageInBox boxIx msgBox = anyMsgIxInBox boxIx msgBox >>= (`lookupStoredMessage` msgBox)

    -- Lookup any 'MsgIx' contained within the 'BoxIx'
    anyMsgIxInBox :: BoxIx -> MessageBox -> Maybe MsgIx
    anyMsgIxInBox boxIx msgBox = Map.lookup boxIx (_subBoxes msgBox) >>= fmap fst . Set.minView

    lookupStoredMessage' :: MsgIx -> MessageBox -> StoredMessage
    lookupStoredMessage' msgIx msgBox = fromMaybe (error "MsgIx is not stored in this MessageBox") $ lookupStoredMessage msgIx msgBox




{- Misc Operations -}

-- | Extract the 'Message' from a 'StoredMessage'.
message :: StoredMessage -> Message
message = _message

-- | Test whether the 'MessageBox's internal structure is valid
-- In particular, test:
--
-- 1.) Every 'StoredMessage' is correctly cached by it's 'MsgIx'
--
-- 2.) For every 'StoredMessage':
--  2.1.) The 'StoredMessage' is within all subBoxes it claims to be within
--  2.2.) The 'StoredMessage' claims to be within all SubBoxes it should be
--        within.
--
-- 3.) For every subBox:
--   3.1.) The subBox has a corresponding predicate.
--   3.2.) Every contained 'MsgIx' corresponds to a 'StoredMessage'.
--     3.2.1) Every contained 'StoredMessage' is cached as 'containedIn' the
--            subBox's ix.
--     3.2.2) Every contained 'StoredMessage' matches the predicate of the
--            subBox it is contained within.
--
-- 3.) The cached 'freshMsgIx' is greater than all used 'MsgIx's
--
-- 4.) The cached 'freshBoxIx' is greater than all used 'BoxIx's
valid :: MessageBox -> Bool
valid msgBox = and [validMsgIxs
                   ,validSubBoxCaches
                   ,validSubBoxesContents
                   ,validFreshMsgIx
                   ,validFreshBoxIx
                   ]
  where
    -- Test 1
    validMsgIxs = Map.foldWithKey (\ix ix' b -> ix==ix' && b) True ixCache
      where
        ixCache = Map.map _msgIx (_storedMessages msgBox) :: Map.Map MsgIx MsgIx

    -- Test 2
    validSubBoxCaches = foldr (\storedMessage b -> validSubBoxCache storedMessage && b) True storedMessages
      where
        storedMessages = Map.elems (_storedMessages msgBox) :: [StoredMessage]

        -- Test 2.*
        validSubBoxCache :: StoredMessage -> Bool
        validSubBoxCache strdMsg = (_containedIn strdMsg) == (whichSubBoxes (_message strdMsg) (_subBoxPredicates msgBox))

    -- Test 3
    validSubBoxesContents = Map.foldWithKey (\boxIx msgIxs b -> validSubBoxContents boxIx msgIxs && b) True (_subBoxes msgBox)
      where
        -- Test 3.*
        validSubBoxContents :: BoxIx -> MsgIxs -> Bool
        validSubBoxContents boxIx msgIxs = case lookupBoxIxsPredicate boxIx msgBox of
            Nothing -> False -- subBox exists without a corresponding predicate
            Just (MessagePredicate (mp,_))
              -> Set.fold (\msgIx b -> case lookupStoredMessage msgIx msgBox of
                                           Nothing -> False -- 'MsgIx' in subBox not stored
                                           Just strdMsg
                                             -> matches mp strdMsg && cachedInSubBox strdMsg boxIx && b
                          ) True msgIxs

        -- The given 'StoredMessage' is cached as being contained within the
        -- given subBoxIx.
        cachedInSubBox :: StoredMessage -> BoxIx -> Bool
        cachedInSubBox strdMsg boxIx = Set.member boxIx (_containedIn strdMsg)

    -- Test 4
    validFreshMsgIx = all (< (_freshMsgIx msgBox)) (Map.keys $ _storedMessages msgBox)

    -- Test 5
    validFreshBoxIx = all (< (_freshBoxIx msgBox)) (Map.keys $ _subBoxes msgBox)

-- | View all 'StoredMessages'
allStoredMessages :: MessageBox -> Set.Set StoredMessage
allStoredMessages msgBox = Set.fromList $ Map.elems $ _storedMessages msgBox

-- | Does a 'StoredMessage' match a given predicate?
matches :: (Message -> Bool)->  StoredMessage -> Bool
matches pred = pred . _message

-- | Is a 'MessageBox' empty of 'StoredMessages'?
noStoredMessages :: MessageBox -> Bool
noStoredMessages = Map.null . _storedMessages

-- | Which sub 'BoxIxs' is a 'StoredMessage' contained in?
containedIn :: StoredMessage -> BoxIxs
containedIn = _containedIn

-- | Format a human-readable representation of a 'MessageBox'.
showMessageBox :: MessageBox -> String
showMessageBox msgBox =
    "MessageBox { "
    ++ "\n MSGS{ " ++ (showStoredMessages $ _storedMessages msgBox) ++ "\n     }"
    ++ "\n BOXES{ " ++ (showBoxes $ _subBoxes msgBox) ++ "\n      }"
    ++ "\n}"
  where
    showBoxes :: SubBoxes -> String
    showBoxes mp = intercalate "\n      , " $ map (\(BoxIx ix,msgIxs) -> (show ix) ++ ":" ++ (show $ map unMsgIx $ Set.toList msgIxs)) $ Map.toList mp

    showStoredMessages :: StoredMessages -> String
    showStoredMessages sm = intercalate "\n     , " $ map showStoredMessage (Map.elems sm)

    showStoredMessage :: StoredMessage -> String
    showStoredMessage sm =
      (show $ unMsgIx $ _msgIx sm) ++ ":" ++
      (showMessage $ _message sm) ++ " ∈ " ++
      (show $ Set.toList $ Set.map unBoxIx $ _containedIn sm)

    showMessage :: Message -> String
    showMessage (msg,replyChan) = case replyChan of
        Nothing -> show msg
        Just _  -> '#' : show msg


{- Internal -}

-- | Lookup the 'MessagePredicate' that corresponds to the given 'BoxIx'.
lookupBoxIxsPredicate :: BoxIx -> MessageBox -> Maybe MessagePredicate
lookupBoxIxsPredicate boxIx msgBox = case Set.toList $ Set.filter (\(MessagePredicate (pred,boxIx')) -> boxIx == boxIx') $ unMessagePredicates (_subBoxPredicates msgBox) of
    []        -> Nothing
    (x:x':[]) -> error "More than one MessagePredicate mapsto this BoxIx"
    [x]       -> Just x

-- | The given 'MsgIx' should be found within the given sub 'BoxIx's. Remove it
-- from these SubBoxes.
removeFromSubBoxes :: BoxIxs -> MsgIx -> SubBoxes -> SubBoxes
removeFromSubBoxes sbs msgIx mp = Set.foldr (Map.adjust (Set.delete msgIx)) mp sbs

-- | The given 'MsgIx' should be found within the given sub 'BoxIxs'. Remove it
-- from these SubBoxes and track which are now empty as a result.
removeFromSubBoxes' :: BoxIxs -> MsgIx -> SubBoxes -> (SubBoxes,BoxIxs)
removeFromSubBoxes' sbs msgIx mp =
    let mp' = removeFromSubBoxes sbs msgIx mp
        newlyEmptySubBoxes = setMapMaybe (\sbIx -> if Set.null (fromJust $ Map.lookup sbIx mp') then Just sbIx else Nothing) sbs
       in (mp',newlyEmptySubBoxes)

-- Lookup a 'MsgIx's corresponding 'StoredMessage' data, provided the index
-- is valid.
lookupStoredMessage :: MsgIx -> MessageBox -> Maybe StoredMessage
lookupStoredMessage msgIx msgBox = Map.lookup msgIx (_storedMessages msgBox)

-- Which subBoxes does a 'Message' belong in?
whichSubBoxes :: Message -> MessagePredicates -> BoxIxs
whichSubBoxes msg (MessagePredicates mps) = setMapMaybe (\(MessagePredicate (pred,boxIx)) -> if pred msg then Just boxIx else Nothing) mps

setMapMaybe :: Ord b => (a -> Maybe b) -> Set.Set a -> Set.Set b
setMapMaybe f = Set.fromList . mapMaybe f . Set.toList

