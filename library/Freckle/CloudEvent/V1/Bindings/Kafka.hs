-- | Provides functionality for integrating CloudEvents with Apache Kafka.
-- This module implements the CloudEvents Kafka Protocol Binding specification v1.0.2
-- which defines how CloudEvents are mapped to Kafka messages.
--
-- The binding supports two content modes:
--
-- * Binary mode: The CloudEvent's data is placed directly in the Kafka message value,
--   with all other attributes being placed in the message headers with a 'ce_' prefix.
--
-- * Structured mode: The entire CloudEvent, including its data, is serialized into a
--   single message value using the JSON event format.
--
-- See: <https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/bindings/kafka-protocol-binding.md>
module Freckle.CloudEvent.V1.Bindings.Kafka
  ( KafkaCloudEventExtAttributes (..)
  , toBinaryKafkaMessage
  , toStructuredKafkaMessage
  ) where

import Prelude

import Autodocodec
  ( Autodocodec (..)
  , HasCodec (..)
  , HasObjectCodec (..)
  , object
  , requiredField'
  , (.=)
  )
import Freckle.CloudEvent.V1.Event.Internal.Data (CloudEvent (..))
import Data.Aeson (FromJSON, ToJSON, encode)
import Data.Binary (Binary)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Maybe (catMaybes)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import GHC.Records (HasField)
import Iri.Rendering.Text (iri)
import Kafka.Producer
  ( ProducePartition (UnassignedPartition)
  , ProducerRecord (..)
  , TopicName
  , headersFromList
  )

-- | Extension attributes specific to Kafka CloudEvents.
-- This follows the CloudEvents Kafka Protocol Binding specification which
-- recommends mapping a partition key attribute to the Kafka record key.
newtype KafkaCloudEventExtAttributes = KafkaCloudEventExtAttributes
  { partitionKey :: Text
  -- ^ The partition key used to determine which partition a message should be sent to.
  -- This corresponds to the 'partitionkey' attribute in the CloudEvents Partitioning extension.
  -- This will be used as the Kafka message key when sending events.
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec KafkaCloudEventExtAttributes)
  deriving anyclass (Binary)

instance HasCodec KafkaCloudEventExtAttributes where
  codec = object "KafkaCloudEventExtAttributes" objectCodec

instance HasObjectCodec KafkaCloudEventExtAttributes where
  objectCodec =
    KafkaCloudEventExtAttributes
      <$> requiredField' "partitionKey" .= (.partitionKey)

-- | Converts a CloudEvent to a Kafka message in binary content mode.
--
-- In binary mode:
-- * The CloudEvent data is serialized as JSON and placed in the Kafka message value
-- * CloudEvent attributes are mapped to Kafka headers with 'ce_' prefix
-- * The content type header indicates the data format
-- * The partition key extension attribute is mapped to the Kafka message key
--
-- As specified in the Kafka Protocol Binding section 3.2:
-- <https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/bindings/kafka-protocol-binding.md#32-binary-content-mode>
toBinaryKafkaMessage
  :: forall ext eventData
   . (HasField "partitionKey" ext Text, ToJSON eventData)
  => TopicName
  -- ^ The Kafka topic to publish to
  -> CloudEvent ext eventData
  -- ^ The CloudEvent to convert
  -> ProducerRecord
  -- ^ Resulting Kafka producer record
toBinaryKafkaMessage topicName cloudEvent =
  let
    partitionKey = cloudEvent.extensionAttributes.partitionKey
    kafkaHeadersMapping :: [(ByteString, ByteString)] =
      catMaybes
        [ Just ("ce_id", encodeUtf8 cloudEvent.ceId)
        , Just ("ce_type", encodeUtf8 cloudEvent.ceType)
        , Just ("ce_source", encodeUtf8 $ iri $ cloudEvent.ceSource)
        , Just ("ce_specversion", encodeUtf8 cloudEvent.ceSpecVersion)
        , Just ("ce_time", fromString $ show $ cloudEvent.ceTime)
        , ("ce_subject",) . encodeUtf8 <$> cloudEvent.ceSubject
        , ("content-type",) . encodeUtf8 <$> cloudEvent.ceDataContentType
        , ("ce_dataschema",) . encodeUtf8 . iri <$> cloudEvent.ceDataSchema
        ]
  in
    ProducerRecord
      { prValue = Just $ BSL.toStrict $ encode cloudEvent.ceData
      , prTopic = topicName
      , prPartition = UnassignedPartition
      , prKey = Just $ encodeUtf8 partitionKey
      , prHeaders = headersFromList kafkaHeadersMapping
      }

-- | Converts a CloudEvent to a Kafka message in structured content mode AKA JSON.
--
-- In structured mode:
-- * The entire CloudEvent (attributes and data) is serialized as JSON and placed in the Kafka message value
-- * A 'content-type' header with value 'application/cloudevents+json; charset=utf-8' is added
-- * The partition key extension attribute is mapped to the Kafka message key
--
-- This format allows for simple forwarding of the same event across multiple
-- routing hops and protocols without requiring knowledge of the CloudEvent format.
--
-- As specified in the Kafka Protocol Binding section 3.3:
-- <https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/bindings/kafka-protocol-binding.md#33-structured-content-mode>
toStructuredKafkaMessage
  :: (HasCodec eventData, HasField "partitionKey" ext Text, HasObjectCodec ext)
  => TopicName
  -- ^ The Kafka topic to publish to
  -> CloudEvent ext eventData
  -- ^ The CloudEvent to convert
  -> ProducerRecord
  -- ^ Resulting Kafka producer record
toStructuredKafkaMessage topicName cloudEvent =
  let
    partitionKey = cloudEvent.extensionAttributes.partitionKey
    kafkaHeadersMapping :: [(ByteString, ByteString)] =
      [("content-type", "application/cloudevents+json; charset=utf-8")]
  in
    ProducerRecord
      { prValue = Just $ BSL.toStrict $ encode cloudEvent
      , prTopic = topicName
      , prPartition = UnassignedPartition
      , prKey = Just $ encodeUtf8 partitionKey
      , prHeaders = headersFromList kafkaHeadersMapping
      }
