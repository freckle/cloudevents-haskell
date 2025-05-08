module CloudEvent.V1.Event.Data
  ( CloudEvent
  , MkCloudEventArg (..)
  , mkCloudEvent
  , EmptyExtension (..)
  ) where

import Prelude

import Autodocodec
  ( Autodocodec (..)
  , HasCodec (..)
  , HasObjectCodec (objectCodec)
  , object
  )
import CloudEvent.V1.Event.Internal.Data (CloudEvent (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Iri.Data (Iri)

-- | Arguments for creating a CloudEvent
data MkCloudEventArg = MkCloudEventArg
  { ceId :: Text
  -- ^ Unique identifier for the event
  , ceSource :: Iri
  -- ^ Identifies the context where the event occurred
  , ceEventType :: Text
  -- ^ Describes the type of event
  , ceDataContentType :: Maybe Text
  -- ^ Content type of the event data (e.g., application/json)
  , ceDataSchema :: Maybe Iri
  -- ^ Schema that the event data adheres to
  , ceSubject :: Maybe Text
  -- ^ Subject of the event within the context
  , ceTime :: UTCTime
  -- ^ Timestamp when the event occurred
  }

mkCloudEvent
  :: MkCloudEventArg
  -> eventData
  -> extensionAttributeRecord
  -> CloudEvent extensionAttributeRecord eventData
mkCloudEvent x eventData ext =
  CloudEvent
    { ceId = x.ceId
    , ceSource = x.ceSource
    , ceSpecVersion = "1.0"
    , ceType = x.ceEventType
    , ceDataContentType = x.ceDataContentType
    , ceDataSchema = x.ceDataSchema
    , ceSubject = x.ceSubject
    , ceTime = x.ceTime
    , ceData = eventData
    , ceDatabase64 = Nothing
    , extensionAttributes = ext
    }

-- | A placeholder type representing an empty set of extension attributes.
--
-- This type is useful when you need to create a CloudEvent with no extension
-- attributes but require a concrete type parameter for the extension.
-- It encodes and decodes to an empty JSON object.
--
-- Example usage:
--
-- @
-- myEvent :: CloudEvent EmptyExtension MyDataType
-- myEvent = mkCloudEvent args myData EmptyExtension
-- @
data EmptyExtension = EmptyExtension
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Binary)
  deriving (FromJSON, ToJSON) via (Autodocodec EmptyExtension)

instance HasCodec EmptyExtension where
  codec = object "EmptyExtension" objectCodec

instance HasObjectCodec EmptyExtension where
  objectCodec = pure EmptyExtension
