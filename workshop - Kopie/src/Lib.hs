{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib where
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import GHC.Generics
import Data.Aeson
import Control.Applicative

data ClientAction = Quit | Nickname Text | Message Text
    deriving (Show, Eq, Ord, Generic)

instance ToJSON ClientAction where
    toJSON Quit = String "Quit"
    toJSON (Message msg) = object ["message" .= msg]
    toJSON (Nickname nick) = object ["nick" .= nick]

instance FromJSON ClientAction where
    parseJSON value = quit value <|> message value <|> nickname value
        where
            quit = withText "Quit expected" (\x -> 
                if x == "Quit"
                    then pure Quit
                    else empty)
            message = withObject "Message expected" (\obj -> do
                msg <- obj .: "message"
                pure (Message msg)
                )
            nickname = withObject "Nickname expected" (\obj -> do
                msg <- obj .: "nick"
                pure (Nickname msg)
                )


serialize :: ClientAction -> ByteString
serialize = BSL.toStrict.encode
deserialize :: ByteString->Either String ClientAction
deserialize = eitherDecodeStrict