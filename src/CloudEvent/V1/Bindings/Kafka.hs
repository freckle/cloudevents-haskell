module CloudEvent.V1.Bindings.Kafka
  ( toBinaryKafkaMessage
  , toStructuredKafkaMessage
  , KafkaCloudEventExtAttributes (..)
  ) where

import Prelude

import CloudEvent.V1.Event.Data (CloudEventRec, CloudEventRows, ceQuery)
import CloudEvent.V1.Orphans (TypeableCodec)
import Data.Aeson (ToJSON, encode)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Data (Typeable)
import Data.Generics.Labels ()
import Data.Maybe (catMaybes)
import Data.Row (type (.+), type (.==))
import Data.Row.Records qualified as Rec
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Iri.Rendering.Text (iri)
import Kafka.Producer.Types
  ( ProducePartition (UnassignedPartition)
  , ProducerRecord (..)
  , TopicName
  , headersFromList
  )

newtype KafkaCloudEventExtAttributes = KafkaCloudEventExtAttributes
  { partitionKey :: Text
  }
  deriving stock (Eq, Generic, Show)

toBinaryKafkaMessage
  :: (Rec.Subset (CloudEventRows a .+ ("partitionKey" .== Text)) r, ToJSON a)
  => TopicName -> CloudEventRec r -> ProducerRecord
toBinaryKafkaMessage topicName cloudEventRec =
  let
    partitionKey = ceQuery cloudEventRec #partitionKey
    kafkaHeadersMapping :: [(ByteString, ByteString)] =
      catMaybes
        [ Just ("ce_id", encodeUtf8 $ ceQuery cloudEventRec #ceId)
        , Just ("ce_type", encodeUtf8 $ ceQuery cloudEventRec #ceType)
        , -- , Just ("ce_source", encodeUtf8 $ iri $ ceQuery cloudEventRec #ceSource)
          Just ("ce_source", encodeUtf8 $ ceQuery cloudEventRec #ceSource)
        , Just
            ("ce_specversion", encodeUtf8 $ ceQuery cloudEventRec #ceSpecVersion)
        , -- , Just ("ce_time", fromString $ show $ ceQuery cloudEventRec #ceTime)
          Just ("ce_time", encodeUtf8 $ ceQuery cloudEventRec #ceTime)
        , ("ce_subject",) . encodeUtf8 <$> ceQuery cloudEventRec #ceSubject
        , ("content-type",) . encodeUtf8 <$> ceQuery cloudEventRec #ceDataContentType
        , ("ce_dataschema",) . encodeUtf8 . iri <$> ceQuery cloudEventRec #ceDataSchema
        ]
  in
    ProducerRecord
      { prValue = Just $ BSL.toStrict $ encode $ ceQuery cloudEventRec #ceData
      , prTopic = topicName
      , prPartition = UnassignedPartition
      , prKey = Just $ encodeUtf8 partitionKey
      , prHeaders = headersFromList kafkaHeadersMapping
      }

toStructuredKafkaMessage
  :: ( Rec.AllUniqueLabels r
     , Rec.Forall r Typeable
     , Rec.Forall r TypeableCodec
     , Rec.Subset (CloudEventRows a .+ ("partitionKey" .== Text)) r
     )
  => TopicName -> CloudEventRec r -> ProducerRecord
toStructuredKafkaMessage topicName cloudEventRec =
  let
    partitionKey = ceQuery cloudEventRec #partitionKey
    kafkaHeadersMapping :: [(ByteString, ByteString)] =
      [("content-type", "application/cloudevents+json; charset=utf-8")]
  in
    ProducerRecord
      { prValue = Just $ BSL.toStrict $ encode cloudEventRec
      , prTopic = topicName
      , prPartition = UnassignedPartition
      , prKey = Just $ encodeUtf8 partitionKey
      , prHeaders = headersFromList kafkaHeadersMapping
      }
