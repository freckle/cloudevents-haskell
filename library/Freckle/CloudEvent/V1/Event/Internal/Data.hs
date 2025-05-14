module Freckle.CloudEvent.V1.Event.Internal.Data
  ( CloudEvent (..)
  ) where

import Prelude

import Autodocodec
  ( Autodocodec (..)
  , HasCodec (..)
  , HasObjectCodec (objectCodec)
  , object
  , optionalField'
  , requiredField'
  , (.=)
  )
import Freckle.CloudEvent.V1.Event.Internal.Orphans ()
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Iri.Data (Iri)

-- | A CloudEvent represents the standardized format for describing event data.
-- This implementation follows the CloudEvents specification version 1.0.
-- CloudEvents are designed to provide interoperability across services,
-- platforms and systems.
data CloudEvent extension eventData = CloudEvent
  { ceId :: Text
  -- ^ Identifies the event. Producers must ensure that 'source' + 'id' is unique
  -- for each distinct event. If a duplicate event is re-sent (e.g. due to a network error)
  -- it may have the same 'id'. Consumers may assume that events with identical
  -- 'source' and 'id' are duplicates.
  --
  -- Examples:
  -- * An event counter maintained by the producer
  -- * A UUID
  , ceSource :: Iri
  -- ^ Identifies the context in which an event happened. Often this will include
  -- information such as the type of the event source, the organization publishing the event,
  -- or the process that produced the event.
  --
  -- Examples:
  -- * https://github.com/cloudevents
  -- * mailto:cncf-wg-serverless@lists.cncf.io
  -- * urn:uuid:6e8bc430-9c3a-11d9-9669-0800200c9a66
  -- * /sensors/tn-1234567/alerts
  , ceSpecVersion :: Text
  -- ^ The version of the CloudEvents specification which the event uses.
  -- This enables the interpretation of the context.
  -- For CloudEvents Spec v1.0, this is always "1.0".
  , ceType :: Text
  -- ^ This attribute contains a value describing the type of event related to the
  -- originating occurrence. Often this attribute is used for routing, observability,
  -- policy enforcement, etc.
  --
  -- Examples:
  -- * com.github.pull_request.opened
  -- * com.example.object.deleted.v2
  , ceDataContentType :: Maybe Text
  -- ^ Content type of the data value. This attribute enables data to carry any
  -- type of content, whereby format and encoding might differ from that of the chosen
  -- event format. For example, an event rendered using the JSON format might carry an
  -- XML payload in data.
  --
  -- Examples:
  -- * text/xml
  -- * application/json
  -- * image/png
  -- * application/octet-stream
  , ceDataSchema :: Maybe Iri
  -- ^ Identifies the schema that data adheres to. Incompatible changes to the schema
  -- should be reflected by a different URI.
  , ceSubject :: Maybe Text
  -- ^ This describes the subject of the event in the context of the event producer.
  -- In publish-subscribe scenarios, a subscriber will typically subscribe to events
  -- emitted by a source, but the source identifier alone might not be sufficient as
  -- a qualifier for any specific event if the source context has internal sub-structure.
  --
  -- Example:
  -- * A storage system might have an event source (storage container) but include the name
  --   of the modified file in the subject: "mynewfile.jpg"
  , ceTime :: UTCTime
  -- ^ Timestamp of when the occurrence happened. If the time of the occurrence
  -- cannot be determined then this attribute may be set to some other time
  -- (such as the current time) by the CloudEvents producer.
  --
  -- Example:
  -- * 2018-04-05T17:31:00Z
  , ceData :: eventData
  -- ^ The actual event data. This may be structured data formatted according to
  -- the 'datacontenttype' attribute. This specification does not place any restriction
  -- on the type of this information.
  , ceDatabase64 :: Maybe Text
  -- ^ Base64 encoded event payload. Must adhere to RFC4648.
  -- This is used for data that is binary in nature and can't be directly included
  -- in standard formats like JSON.
  , extensionAttributes :: extension
  -- ^ User-defined extension attributes to the CloudEvent. Extension attributes
  -- have no defined meaning in the core CloudEvent specification.
  -- They allow external systems to attach metadata to an event.
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec (CloudEvent extension eventData))
  deriving anyclass (Binary)

instance
  (HasCodec eventData, HasObjectCodec extension)
  => HasCodec (CloudEvent extension eventData)
  where
  codec =
    object "CloudEvent"
      $ CloudEvent
        <$> requiredField' "id" .= (.ceId)
        <*> requiredField' "source" .= (.ceSource)
        <*> requiredField' "specversion" .= (.ceSpecVersion)
        <*> requiredField' "type" .= (.ceType)
        <*> optionalField' "datacontenttype" .= (.ceDataContentType)
        <*> optionalField' "dataschema" .= (.ceDataSchema)
        <*> optionalField' "subject" .= (.ceSubject)
        <*> requiredField' "time" .= (.ceTime)
        <*> requiredField' "data" .= (.ceData)
        <*> optionalField' "data_base64" .= (.ceDatabase64)
        <*> objectCodec @extension .= (.extensionAttributes)
