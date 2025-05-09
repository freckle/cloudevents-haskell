{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module CloudEvent.V1.Event.Data
  ( CloudEventRec
  , mkCloudEventRec
  , CloudEventRows
  , getCloudEventRec
  , NullCloudEventExtensionAttributesRec (..)
  , MkCloudEventArg (..)

    -- * Operations
  , ceQuery
  , ceUpdate
  , ceFocus
  , ceMultiFocus
  ) where

import Prelude

import Autodocodec (HasCodec)
import CloudEvent.V1.Orphans
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Data (Typeable)
import Data.Generics.Labels ()
import Data.Generics.Product.Fields (HasField (..), HasField' (..))
import Data.Row
import Data.Row.Records qualified as Rec
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Iri.Data (Iri)

newtype CloudEventRec r = CloudEventRec {getRec :: Rec r}
  deriving stock (Generic)

type CloudEventRows eventData =
  "ceId" .== Text
    .+ "ceSource" .== Text -- TODO: Replace with Iri
    .+ "ceSpecVersion" .== Text
    .+ "ceType" .== Text
    .+ "ceDataContentType" .== Maybe Text
    .+ "ceDataSchema" .== Maybe Iri
    .+ "ceSubject" .== Maybe Text
    .+ "ceTime" .== Text -- TODO replace with UTCTime
    .+ "ceData" .== eventData
    .+ "ceDataBase64" .== Maybe Text

data NullCloudEventExtensionAttributesRec = NullCloudEventExtensionAttributesRec
  deriving stock (Eq, Generic, Show)

deriving stock instance Forall r Show => Show (CloudEventRec r)
deriving newtype instance Forall r Eq => Eq (CloudEventRec r)
deriving newtype instance (Forall r Eq, Forall r Ord) => Ord (CloudEventRec r)
deriving newtype instance
  (AllUniqueLabels r, Forall r Bounded) => Bounded (CloudEventRec r)

instance
  {-# OVERLAPPING #-}
  ((r .! name) ≈ a, KnownSymbol name, r ~ Rec.Modify name a r)
  => HasField' name (CloudEventRec r) a
  where
  field' f r = CloudEventRec <$> Rec.focus (Label @name) f r.getRec

instance
  {-# OVERLAPPING #-}
  ( (r .! name) ≈ a
  , (r' .! name) ≈ b
  , KnownSymbol name
  , r ~ Rec.Modify name a r'
  , r' ~ Rec.Modify name b r
  )
  => HasField name (CloudEventRec r) (CloudEventRec r') a b
  where
  field f r = CloudEventRec <$> Rec.focus (Label @name) f r.getRec

deriving newtype instance
  ( AllUniqueLabels r
  , Forall r Typeable
  , Forall r TypeableCodec
  )
  => HasCodec (CloudEventRec r)

deriving newtype instance
  ( AllUniqueLabels r
  , Forall r Typeable
  , Forall r TypeableCodec
  )
  => ToJSON (CloudEventRec r)

deriving newtype instance
  ( AllUniqueLabels r
  , Forall r Typeable
  , Forall r TypeableCodec
  )
  => FromJSON (CloudEventRec r)

deriving newtype instance
  ( AllUniqueLabels r
  , Forall r FromJSON
  , Forall r ToJSON
  )
  => Binary (CloudEventRec r)

data MkCloudEventArg = MkCloudEventArg
  { id :: Text
  -- ^ Unique identifier for the event that enables event traceability and deduplication
  , source :: Text -- TODO: Replace with Iri

  -- ^ Identifies where the event originated, such as a system, service, or component. Must be an absolute URI
  , eventType :: Text
  -- ^ Describes what kind of event occurred, typically using a domain-specific convention. Event versioning info can be included here [More Info](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/primer.md#the-role-of-the-type-attribute-within-versioning)
  , dataContentType :: Maybe Text
  -- ^ Format of the event data (e.g., "application/json") to help receivers parse it correctly. MIME type for the event data
  , dataSchema :: Maybe Iri
  -- ^ Reference to a schema that describes the structure and validation rules for the event data. Must be an absolute URI
  , subject :: Maybe Text
  -- ^ Specific subject or target that the event relates to within the event source's context
  , time :: Text -- TODO replace with UTCTime

  -- ^ When the event occurred, helping establish chronology in event sequences
  }

mkCloudEventRec
  :: ( Rec.FromNative extensionAttributesNativeRecord
     , Rec.NativeRow extensionAttributesNativeRecord ~ extensionAttributesRows
     )
  => MkCloudEventArg
  -> eventData
  -> extensionAttributesNativeRecord
  -> CloudEventRec (CloudEventRows eventData .+ extensionAttributesRows)
mkCloudEventRec x eventData extensionAttributesNativeRecord =
  let extensionAttributesRec = Rec.fromNative extensionAttributesNativeRecord
  in  CloudEventRec
        $ #ceId .== x.id
          .+ #ceSource .== x.source
          .+ #ceSpecVersion .== ("1.0" :: Text)
          .+ #ceType .== x.eventType
          .+ #ceDataContentType .== x.dataContentType
          .+ #ceDataSchema .== x.dataSchema
          .+ #ceSubject .== x.subject
          .+ #ceTime .== x.time
          .+ #ceData .== eventData
          .+ #ceDataBase64 .== (Nothing :: Maybe Text)
          .+ extensionAttributesRec

getCloudEventRec :: CloudEventRec r -> Rec r
getCloudEventRec = (.getRec)

ceQuery :: KnownSymbol l => CloudEventRec r -> Label l -> r .! l
ceQuery r l = getCloudEventRec r .! l

ceUpdate
  :: (KnownSymbol l, r .! l ≈ a)
  => Label l -> a -> CloudEventRec r -> CloudEventRec r
ceUpdate l v r =
  let
    rec = getCloudEventRec r
    updatedRec = Rec.update l v rec
  in
    CloudEventRec updatedRec

ceFocus
  :: ( Functor f
     , KnownSymbol l
     , r .! l ≈ a
     , r ~ Rec.Modify l a r'
     , r' .! l ≈ b
     , r' ~ Rec.Modify l b r
     )
  => Label l -> (a -> f b) -> CloudEventRec r -> f (CloudEventRec r')
ceFocus l f r =
  let
    rec = getCloudEventRec r
    updatedRec = Rec.focus l f rec
  in
    CloudEventRec <$> updatedRec

ceMultiFocus
  :: forall u v r f
   . ( Disjoint u r
     , Disjoint v r
     , Functor f
     )
  => (Rec u -> f (Rec v)) -> CloudEventRec (u .+ r) -> f (CloudEventRec (v .+ r))
ceMultiFocus f (CloudEventRec (u :+ r)) = CloudEventRec . (.+ r) <$> f u

exampleEvent :: Rec (CloudEventRows Text)
exampleEvent =
  #ceId .== "unique-event-id-12345"
    .+ #ceSource .== "urn:example:source"
    .+ #ceSpecVersion .== ("1.0" :: Text)
    .+ #ceType .== "com.example.custom_event.v1"
    .+ #ceDataContentType .== Just "application/json"
    .+ #ceDataSchema .== (Nothing :: Maybe Iri)
    .+ #ceSubject .== Just "entity-instance-42"
    .+ #ceTime .== "2023-10-26T12:34:56Z"
    .+ #ceData .== "Hello World"
    .+ #ceDataBase64 .== (Nothing :: Maybe Text)
