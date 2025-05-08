{-# LANGUAGE AllowAmbiguousTypes #-}

module CloudEvent.V1.Event.Validation
  ( isValidCloudEvent
  ) where

import Prelude

import Autodocodec.Schema (jsonSchemaViaCodec, validateAccordingTo)
import CloudEvent.V1.Event.Data (CloudEventRec, CloudEventRows)
import CloudEvent.V1.Orphans (TypeableCodec)
import Data.Aeson (Value)
import Data.Data (Typeable)
import Data.Kind (Type)
import Data.Row (AllUniqueLabels, Row, type (.+))
import Data.Row.Records (Forall)

-- | Will require a type application for the cloudevent event data type
isValidCloudEvent
  :: forall eventData (extensionAttributesRows :: Row Type)
   . ( AllUniqueLabels (CloudEventRows eventData .+ extensionAttributesRows)
     , Forall (CloudEventRows eventData .+ extensionAttributesRows) Typeable
     , Forall (CloudEventRows eventData .+ extensionAttributesRows) TypeableCodec
     )
  => Value -> Bool
isValidCloudEvent =
  flip
    validateAccordingTo
    ( jsonSchemaViaCodec
        @(CloudEventRec (CloudEventRows eventData .+ extensionAttributesRows))
    )
