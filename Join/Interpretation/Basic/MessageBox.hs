module Join.Interpretation.Basic.MessageBox
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

type Message = T.Message ByteString ByteString

type ReplyChan = T.ReplyChan ByteString

type MessagePredicate = T.MessagePredicate ByteString

type MessagePredicates = T.MessagePredicates ByteString

type MessageBox = T.MessageBox ByteString ByteString

type StoredMessages = T.StoredMessages ByteString ByteString

type StoredMessage = T.StoredMessage ByteString ByteString

emptyMessageBox :: MessageBox
emptyMessageBox = T.emptyMessageBox

addSubBox :: (ByteString -> Bool) -> MessageBox -> (MessageBox,BoxIx)
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

matches :: (ByteString -> Bool) ->  StoredMessage -> Bool
matches = T.matches

noStoredMessages :: MessageBox -> Bool
noStoredMessages = T.noStoredMessages

valid :: MessageBox -> Bool
valid = T.valid

containedIn :: StoredMessage -> BoxIxs
containedIn = T.containedIn

showMessageBox :: MessageBox -> String
showMessageBox = T.showMessageBox

