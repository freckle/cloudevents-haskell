{-# LANGUAGE AllowAmbiguousTypes #-}

module Freckle.CloudEvent.V1.Event.Validation
  ( isValidCloudEvent
  ) where

import Prelude

import Autodocodec (HasCodec, HasObjectCodec)
import Autodocodec.Schema (jsonSchemaViaCodec, validateAccordingTo)
import Data.Aeson (Value)
import Data.Kind (Type)
import Freckle.CloudEvent.V1.Event.Data (CloudEvent)

-- | Validates that a JSON value conforms to a specific CloudEvents specification.
--  Requires the event data type and extension attributes to be specified as type applications with the event data type first.
--
-- Example usage:
--
-- @
-- let jsonValue = ...  -- Some JSON representation of a CloudEvent
-- if isValidCloudEvent \@MyEventData \@MyExtensions jsonValue
--   then handleValidEvent jsonValue
--   else handleInvalidEvent
-- @
isValidCloudEvent
  :: forall (eventData :: Type) (extensionAttributes :: Type)
   . (HasCodec eventData, HasObjectCodec extensionAttributes) => Value -> Bool
isValidCloudEvent =
  flip
    validateAccordingTo
    ( jsonSchemaViaCodec
        @(CloudEvent extensionAttributes eventData)
    )
