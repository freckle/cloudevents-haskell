{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module CloudEvent.V1.Event.Internal.Orphans where

import Prelude

import Autodocodec (Autodocodec (..), HasCodec (codec), bimapCodec)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (first)
import Data.Binary (Binary (..))
import Data.Binary.Instances ()
import Data.Text (Text)
import Data.Text qualified as T
import Iri.Data (Iri)
import Iri.Parsing.Text qualified
import Iri.Rendering.Text qualified

instance HasCodec Iri where
  codec =
    bimapCodec
      (first T.unpack . Iri.Parsing.Text.iri)
      Iri.Rendering.Text.iri
      (codec @Text)

deriving via Autodocodec Iri instance FromJSON Iri

deriving via Autodocodec Iri instance ToJSON Iri

instance Binary Iri where
  put r = put @Text $ Iri.Rendering.Text.iri r
  get = get @Text >>= either (fail . T.unpack) pure . Iri.Parsing.Text.iri
