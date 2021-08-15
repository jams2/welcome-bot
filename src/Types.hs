{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Types
  ( AppServiceConfig (..),
    emptyObject,
    ErrorResponse (..),
    RoomEvents (..),
  )
where

import Data.Aeson
import Data.Aeson.Casing (snakeCase)
import Data.Text.Lazy
import GHC.Generics

encodeKeys :: String -> String
encodeKeys cs = if cs == "eventType" then "type" else snakeCase cs

data RoomEvents = RoomEvents
  { events :: [MRoomMemberEvent]
  }
  deriving (Generic, Show)

instance ToJSON RoomEvents where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON RoomEvents

data MRoomMemberEvent = MRoomMemberEvent
  { content :: MRoomMemberEventContent,
    eventType :: Text,
    eventId :: Text,
    sender :: Text,
    originServerTs :: Int,
    unsigned :: Maybe UnsignedData,
    roomId :: Text,
    stateKey :: Text
  }
  deriving (Generic, Show)

instance ToJSON MRoomMemberEvent where
  toEncoding = genericToEncoding defaultOptions {fieldLabelModifier = encodeKeys}

instance FromJSON MRoomMemberEvent where
  parseJSON = withObject "MRoomMemberEvent" $ \v -> do
    content <- v .: "content"
    eventType <- v .: "type"
    eventId <- v .: "event_id"
    sender <- v .: "sender"
    originServerTs <- v .: "origin_server_ts"
    unsigned <- v .:? "unsigned"
    roomId <- v .: "room_id"
    stateKey <- v .: "state_key"
    return MRoomMemberEvent {..}

data MRoomMemberEventContent = MRoomMemberEventContent
  { avatarUrl :: Maybe Text,
    displayname :: Maybe Text,
    membership :: Membership,
    isDirect :: Maybe Bool
  }
  deriving (Generic, Show)

instance ToJSON MRoomMemberEventContent where
  toEncoding = genericToEncoding defaultOptions {fieldLabelModifier = encodeKeys}

instance FromJSON MRoomMemberEventContent where
  parseJSON = withObject "MRoomMemberEventContent" $ \v -> do
    avatarUrl <- v .:? "avatar_url"
    displayname <- v .:? "displayname"
    membership <- v .: "membership"
    isDirect <- v .:? "is_direct"
    return MRoomMemberEventContent {..}

data Membership = Invite | Join | Knock | Leave | Ban deriving (Eq, Show)

instance ToJSON Membership where
  toJSON a = case a of
    Invite -> String "invite"
    Join -> String "join"
    Knock -> String "knock"
    Leave -> String "leave"
    Ban -> String "ban"

instance FromJSON Membership where
  parseJSON a = case a of
    String "invite" -> return Invite
    String "join" -> return Join
    String "knock" -> return Knock
    String "leave" -> return Leave
    String "ban" -> return Ban
    _ -> fail $ "Unknown membership value: " ++ show a

data UnsignedData = UnsignedData
  {age :: Maybe Int, transactionId :: Maybe Text}
  deriving (Generic, Show)

instance ToJSON UnsignedData where
  toEncoding = genericToEncoding defaultOptions {fieldLabelModifier = encodeKeys}

instance FromJSON UnsignedData where
  parseJSON = withObject "UnsignedData" $ \v -> do
    age <- v .:? "age"
    transactionId <- v .:? "transaction_id"
    return UnsignedData {..}

data AppServiceConfig = AppServiceConfig
  { id :: Text,
    url :: Text,
    asToken :: Text,
    hsToken :: Text,
    senderLocalPart :: Text
  }
  deriving (Generic, Show)

instance FromJSON AppServiceConfig where
  parseJSON = withObject "AppServiceConfig" $ \v -> do
    serviceId <- v .: "id"
    url <- v .: "url"
    asToken <- v .: "as_token"
    hsToken <- v .: "hs_token"
    senderLocalPart <- v .: "sender_localpart"
    return AppServiceConfig {id = serviceId, ..}

data EmptyObject = EmptyObject deriving (Generic, Show)

emptyObject :: EmptyObject
emptyObject = EmptyObject

instance ToJSON EmptyObject where
  toJSON _ = object []

data ErrorResponse = ErrorResponse
  { errcode :: Text,
    error :: Text
  }
  deriving (Generic, Show)

instance ToJSON ErrorResponse where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON ErrorResponse
