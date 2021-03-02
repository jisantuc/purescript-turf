module Test.Main where

import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Effect (Effect)
import Foreign.Object (empty)
import Prelude (class Eq, class Show, Unit, apply, map, show, unit, (<$>), (<>), (==))
import Test.QuickCheck (Result, quickCheck, (<?>))
import Turf.Helpers
  ( Feature
  , FeatureProperties
  , LineStringFeature
  , LineStringGeom
  , MultiLineStringFeature
  , MultiLineStringGeom
  , MultiPointFeature
  , MultiPointGeom
  , MultiPolygonFeature
  , MultiPolygonGeom
  , PointFeature
  , PointGeom
  , PolygonFeature
  , PolygonGeom
  , featureGeometry
  , lineString
  , multiLineString
  , multiPoint
  , multiPolygon
  , point
  , polygon
  )

main :: Effect Unit
main = ado
  quickCheck (\(x :: PointGeom) -> codecRoundTrip "PointGeom codec" x)
  quickCheck (\(x :: PointFeature) -> codecRoundTrip "PointFeature codec" x)
  quickCheck (\(x :: MultiPointGeom) -> codecRoundTrip "MultiPointGeom codec" x)
  quickCheck (\(x :: MultiPointFeature) -> codecRoundTrip "MultiPointFeature codec" x)
  quickCheck (\(x :: LineStringGeom) -> codecRoundTrip "LineStringGeom codec" x)
  quickCheck (\(x :: LineStringFeature) -> codecRoundTrip "LineStringFeature codec" x)
  quickCheck (\(x :: MultiLineStringGeom) -> codecRoundTrip "MultiLineStringGeom codec" x)
  quickCheck (\(x :: MultiLineStringFeature) -> codecRoundTrip "MultiLineStringFeature codec" x)
  quickCheck (\(x :: PolygonGeom) -> codecRoundTrip "PolygonGeom codec" x)
  quickCheck (\(x :: PolygonFeature) -> codecRoundTrip "PolygonFeature codec" x)
  quickCheck (\(x :: MultiPolygonGeom) -> codecRoundTrip "MultiPolygonGeom codec" x)
  quickCheck (\(x :: MultiPolygonFeature) -> codecRoundTrip "MultiPolygonFeature codec" x)
  quickCheck (\(x :: PointGeom) -> jsRoundTrip "point FFI" x point)
  quickCheck (\(x :: MultiPointGeom) -> jsRoundTrip "multipoint FFI" x multiPoint)
  quickCheck (\(x :: LineStringGeom) -> jsRoundTrip "lineString FFI" x lineString)
  quickCheck (\(x :: MultiLineStringGeom) -> jsRoundTrip "multiLineString FFI" x multiLineString)
  quickCheck (\(x :: PolygonGeom) -> jsRoundTrip "polygon FFI" x polygon)
  quickCheck (\(x :: MultiPolygonGeom) -> jsRoundTrip "multiPolygon FFI" x multiPolygon)
  in unit

jsRoundTrip :: forall a. Eq a => Show a => EncodeJson a => String -> a -> (a -> FeatureProperties -> Either JsonDecodeError (Feature a)) -> Result
jsRoundTrip tag geom f =
  let
    result = f geom empty
  in
    (featureGeometry <$> result) == Right geom <?> "Test: " <> tag <> ". Result: \n" <> show result <> "\nnot equal to expected:\n" <> show geom

codecRoundTrip :: forall a. Eq a => Show a => DecodeJson a => EncodeJson a => String -> a -> Result
codecRoundTrip tag a =
  let
    encoded = encodeJson a

    decoded = decodeJson encoded
  in
    decoded == Right a <?> "Test: " <> tag <> ". Result: \n" <> show decoded <> "\nnot equal to expected:\n" <> show a
