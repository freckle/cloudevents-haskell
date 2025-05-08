{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module CloudEvent.V1.Orphans where

import Prelude

import Autodocodec (Autodocodec (..), HasCodec (codec), bimapCodec)
import Autodocodec qualified as Auto
import Data.Aeson (FromJSON (parseJSON), Key, ToJSON (toJSON), Value)
import Data.Aeson.Types (parseEither)
import Data.Bifunctor (first)
import Data.Binary (Binary (..))
import Data.Binary.Instances ()
import Data.Data (Typeable)
import Data.Dynamic (fromDynamic)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (fromMaybe)
import Data.Row (Rec)
import Data.Row.Internal
import Data.Row.Records qualified as Rec
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import GHC.TypeLits (symbolVal)
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

instance
  forall r
   . (AllUniqueLabels r, Forall r Typeable, Forall r TypeableCodec)
  => HasCodec (Rec r)
  where
  codec =
    Auto.object "CloudEvent"
      $ Rec.fromLabelsA @TypeableCodec @(Auto.ObjectCodec (Rec r)) @r makeFieldCodecs
   where
    makeFieldCodecs
      :: forall l a
       . (KnownSymbol l, TypeableCodec a) => Label l -> Auto.ObjectCodec (Rec r) a
    makeFieldCodecs l =
      Auto.requiredField' (T.pack $ symbolVal l)
        Auto..= ( fromMaybe
                    -- This error should be impossible
                    ( error
                        ( "Something really bad has occurred. The key"
                            <> symbolVal l
                            <> "SHOULD have been a part of the object"
                        )
                    )
                    . fromDynamic
                    . (HashMap.! (T.pack $ symbolVal l))
                    . Rec.toDynamicMap
                )

deriving via
  (Autodocodec (Rec r))
  instance
    ( AllUniqueLabels r
    , Forall r Typeable
    , Forall r TypeableCodec
    )
    => ToJSON (Rec r)

deriving via
  (Autodocodec (Rec r))
  instance
    ( AllUniqueLabels r
    , Forall r Typeable
    , Forall r TypeableCodec
    )
    => FromJSON (Rec r)

instance Binary Iri where
  put r = put @Text $ Iri.Rendering.Text.iri r
  get = get @Text >>= either (fail . T.unpack) pure . Iri.Parsing.Text.iri

instance (AllUniqueLabels r, Forall r FromJSON, Forall r ToJSON) => Binary (Rec r) where
  put = put . Rec.eraseWithLabels @ToJSON @r @Key toJSON
  get = do
    kvs <- get @[(Key, Value)]
    let recOrErr = Rec.fromLabelsA @FromJSON @(Either String) @r (makeField kvs)
    either fail pure recOrErr
   where
    makeField
      :: (FromJSON a, KnownSymbol l) => [(Key, Value)] -> Label l -> Either String a
    makeField kvs l =
      maybe
        ( Left
            ( "Error: the key "
                <> symbolVal l
                <> " should have been in the payload but is missing"
            )
        )
        (parseEither parseJSON)
        $ lookup (fromString @Key $ symbolVal l) kvs

class (HasCodec a, Typeable a) => TypeableCodec a

instance TypeableCodec Iri
instance TypeableCodec Text
instance TypeableCodec a => TypeableCodec (Maybe a)
instance TypeableCodec UTCTime
