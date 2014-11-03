module Join.Interpretation.Basic.DynamicMessageBox
  (
  -- * Types
   Message
  ,ReplyChan
  ,BoxId(..)
  ,BoxIx(..)
  ,MsgIx(),MsgIxs
  ,MessagePredicate(),MessagePredicates()

  -- * MessageBox
  ,MessageBox(),StoredMessage(),StoredMessages(),SubBoxes
  ,emptyMessageBox

  -- ** Adding Messages/ subBoxes
  ,addSubBox
  ,insertMessage
  ,insertMessage'

  -- * Removing Messages
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

import Prelude hiding (take)

import qualified Data.Set                                  as Set
import qualified Join.Interpretation.Basic.TypedMessageBox as T
import           Join.Types.Message

import Join.Interpretation.Basic.TypedMessageBox (BoxId(..)
                                                 ,BoxIx(..)
                                                 ,BoxIxs
                                                 ,MsgIx()
                                                 ,MsgIxs
                                                 ,SubBoxes
                                                 )

type Message = T.Message Dynamic Dynamic

type ReplyChan = T.ReplyChan Dynamic

type MessagePredicate = T.MessagePredicate Dynamic

type MessagePredicates = T.MessagePredicates Dynamic

type MessageBox = T.MessageBox Dynamic Dynamic

type StoredMessages = T.StoredMessages Dynamic Dynamic

type StoredMessage = T.StoredMessage Dynamic Dynamic

emptyMessageBox :: MessageBox
emptyMessageBox = T.emptyMessageBox

addSubBox :: (Dynamic -> Bool) -> MessageBox -> (MessageBox,BoxIx)
addSubBox = T.addSubBox

insertMessage :: Message -> MessageBox -> MessageBox
insertMessage = T.insertMessage

insertMessage' :: Message -> MessageBox -> (MessageBox,StoredMessage)
insertMessage' = T.insertMessage'

take :: Maybe BoxIx -> MessageBox -> Maybe (StoredMessage,BoxIxs,MessageBox)
take = T.take

takeAny :: MessageBox -> Maybe (StoredMessage,BoxIxs,MessageBox)
takeAny = T.takeAny

takeFrom :: BoxIx -> MessageBox -> Maybe (StoredMessage,BoxIxs,MessageBox)
takeFrom = T.takeFrom

message :: StoredMessage -> Message
message = T.message

allStoredMessages :: MessageBox -> Set.Set StoredMessage
allStoredMessages = T.allStoredMessages

matches :: (Dynamic -> Bool) ->  StoredMessage -> Bool
matches = T.matches

noStoredMessages :: MessageBox -> Bool
noStoredMessages = T.noStoredMessages

valid :: MessageBox -> Bool
valid = T.valid

containedIn :: StoredMessage -> BoxIxs
containedIn = T.containedIn

showMessageBox :: MessageBox -> String
showMessageBox = T.showMessageBox

