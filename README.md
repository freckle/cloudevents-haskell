# CloudEvents Haskell

Unofficial SDK for the CloudEvents specification

## Overview

This library provides a Haskell implementation of the [CloudEvents](https://cloudevents.io/) specification, which defines a common format for describing event data across different services, platforms, and systems.

## Features

- Create, validate, and manipulate CloudEvents
- Support for both structured and binary content modes
- Kafka protocol binding support
- Extensible architecture for custom event data and attributes

## Installation

Add the following to your `package.yaml` or `.cabal` file:

```yaml
dependencies:
  - cloudevents-haskell
```

## Usage Example

The following example demonstrates how to:

1. Create a CloudEvent
2. Add custom data and extension attributes with type level field deduplication
3. Validate the CloudEvent
4. Encode it for Kafka transport in both binary and structured modes

```haskell
module Main where

import CloudEvent.V1.Event.Data (MkCloudEventArg(..), mkCloudEventRec, ceQuery, ceUpdate)
import CloudEvent.V1.Event.Validation (isValidCloudEvent)
import CloudEvent.V1.Bindings.Kafka (toBinaryKafkaMessage, toStructuredKafkaMessage)
import CloudEvent.V1.Orphans ()

import Data.Aeson (ToJSON, FromJSON, encode, decode)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as BSL

-- Define our custom event data type
data OrderEventData = OrderEventData
  { orderId :: Text
  , customerId :: Text
  , amount :: Double
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- Define custom extension attributes
data OrderExtensionAttributes = OrderExtensionAttributes
  { region :: Text
  , platform :: Text
  , partitionKey :: Text
  } deriving (Generic, Show)

main :: IO ()
main = do
  -- Get current time for the event timestamp
  currentTime <- getCurrentTime

  -- Create event data
  let orderData = OrderEventData
        { orderId = "ORDER-12345"
        , customerId = "CUST-789"
        , amount = 125.99
        }

  -- Create extension attributes
  let extensions = OrderExtensionAttributes
        { region = "us-west-2"
        , platform = "mobile-app"
        , partitionKey = "orders"
        }

  -- Create CloudEvent with required attributes and our custom data
  let cloudEventArgs = MkCloudEventArg
        { ceId = "event-uuid-1234-5678"
        , ceSource = "https://example.com/orders"
        , ceEventType = "com.example.order.created"
        , ceDataContentType = Just "application/json"
        , ceDataSchema = Nothing
        , ceSubject = Just "orders/create"
        , ceTime = currentTime
        }

  -- Construct the CloudEvent record with our order data and extensions
  let cloudEvent = mkCloudEventRec
        cloudEventArgs
        orderData
        extensions

  -- Print the CloudEvent as JSON
  putStrLn "CloudEvent JSON representation:"
  let jsonBS = encode cloudEvent
  BSL.putStr jsonBS

  -- Validate the CloudEvent
  let isValid = (isValidCloudEvent @OrderEvent @OrderExtensionAttributes) $ toJSON cloudEvent

  putStrLn $ "Event validation: " ++ if isValid then "Valid" else "Invalid"

  -- Access fields from the CloudEvent
  putStrLn $ "Event ID: " ++ T.unpack (ceQuery cloudEvent #ceId)
  putStrLn $ "Event Source: " ++ T.unpack (ceQuery cloudEvent #ceSource)
  putStrLn $ "Event Type: " ++ T.unpack (ceQuery cloudEvent #ceType)
  putStrLn $ "Order ID: " ++ T.unpack (orderId $ ceQuery cloudEvent #ceData)
  putStrLn $ "Region: " ++ T.unpack (ceQuery cloudEvent #region)

  -- Prepare a message for Kafka in binary mode
  putStrLn "\nPreparing Kafka message (binary mode):"
  -- Will work since out OrderExtensionAttributes record contains a "partitionKey" field
  let binaryMessage = toBinaryKafkaMessage "order-events" cloudEvent

  -- Prepare a message for Kafka in structured mode
  putStrLn "Preparing Kafka message (structured mode):"
  let structuredMessage = toStructuredKafkaMessage "order-events" cloudEvent

  -- Using freckle-kafka
  -- This function doesn't exist but shouldn't be too hard to make
  produceKeyedOnWithMessage "order-events" structuredMessage
```

## Binary Mode vs. Structured Mode

CloudEvents supports two content modes for transporting events:

1. **Binary Mode**: Event data is stored in the message body, while event attributes are stored as part of the message metadata (e.g., as headers in Kafka or HTTP).

2. **Structured Mode**: The entire event, including both data and context attributes, is encoded as a single structure (typically JSON) and stored in the message body.

This SDK supports both modes when using the Kafka protocol binding.

## Extensions

The CloudEvents specification allows for extensions through additional context attributes. This library makes it easy to add custom extension attributes to your events, as shown in the example above with the `region` and `platform` attributes. It also gives us type checking to prevent extension attributes from overlapping with core attributes. Such situations will be flagged as type errors

## Validation

You can validate CloudEvents against the specification using the `isValidCloudEvent` function, which ensures that the event conforms to the CloudEvents schema and contains all required attributes.
