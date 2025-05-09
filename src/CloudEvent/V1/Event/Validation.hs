{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module CloudEvent.V1.Event.Validation
  ( isValidCloudEvent
  ) where

import Prelude

import Autodocodec.Schema (jsonSchemaViaCodec, validateAccordingTo)
import CloudEvent.V1.Event.Data (CloudEventRec, CloudEventRows)
import CloudEvent.V1.Orphans (TypeableCodec)
import Data.Aeson (Value)
import Data.Data (Typeable)
import Data.Row (AllUniqueLabels, type (.+))
import Data.Row.Records (Forall)
import Data.Row.Records qualified as Rec

-- | Will require a type application for the cloudevent event data type
isValidCloudEvent
  :: forall eventData extensionAttributesNativeRecord extensionAttributesRows
   . ( AllUniqueLabels (CloudEventRows eventData .+ extensionAttributesRows)
     , Forall (CloudEventRows eventData .+ extensionAttributesRows) Typeable
     , Forall (CloudEventRows eventData .+ extensionAttributesRows) TypeableCodec
     , Rec.FromNative extensionAttributesNativeRecord
     , Rec.NativeRow extensionAttributesNativeRecord ~ extensionAttributesRows
     )
  => Value -> Bool
isValidCloudEvent =
  flip
    validateAccordingTo
    ( jsonSchemaViaCodec
        @(CloudEventRec (CloudEventRows eventData .+ extensionAttributesRows))
    )
