{-# LANGUAGE FlexibleInstances
            ,UndecidableInstances
  #-}
module Join.Types.Message
  (MessageType(encodeMessage,decodeMessage)
  ,ByteString
  ) where

import Data.ByteString
import Data.Serialize

class Serialize m => MessageType m where
  encodeMessage :: m -> ByteString
  encodeMessage = encode

  decodeMessage :: ByteString -> Maybe m
  decodeMessage bs = case decode bs of
    Left _ -> Nothing
    Right msg -> Just msg
instance Serialize m => MessageType m
