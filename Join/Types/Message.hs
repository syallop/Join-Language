{-# LANGUAGE FlexibleInstances
            ,UndecidableInstances
  #-}
module Join.Types.Message
  (MessageType(encodeMessage
              ,decodeMessage
              ,forgetMessageType
              ,recallMessageType
              )
  ,ByteString
  ,Dynamic
  ) where

import Data.ByteString
import Data.Dynamic
import Data.Serialize

class (Serialize m,Typeable m)
   => MessageType m where
  encodeMessage :: m -> ByteString
  encodeMessage = encode

  decodeMessage :: ByteString -> Maybe m
  decodeMessage bs = case decode bs of
    Left _ -> Nothing
    Right msg -> Just msg

  forgetMessageType :: m -> Dynamic
  forgetMessageType = toDyn

  recallMessageType :: Dynamic -> Maybe m
  recallMessageType = fromDynamic

instance (Serialize m,Typeable m) => MessageType m

