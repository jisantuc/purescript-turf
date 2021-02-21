module Test.Main where

import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Foreign.Object (empty)
import Prelude (class Eq, class Show, Unit, apply, discard, map, show, unit, ($), (<$>), (<>), (==))
import Test.QuickCheck (Result, quickCheck, (<?>))
import Test.Unit (suite, test)
import Test.Unit.Main (runTest)
import Turf.Helpers (Feature, FeatureProperties, LineStringFeature, LineStringGeom, MultiLineStringGeom, MultiPointFeature, MultiPointGeom, PointFeature, PointGeom, MultiLineStringFeature, featureGeometry, lineString, multiLineString, multiPoint, point)

main :: Effect Unit
main =
  runTest do
    suite "Round trips" do
      test "JSON codecs" $ liftEffect
        $ ado
            quickCheck (\(x :: PointGeom) -> codecRoundTrip x)
            quickCheck (\(x :: PointFeature) -> codecRoundTrip x)
            quickCheck (\(x :: MultiPointGeom) -> codecRoundTrip x)
            quickCheck (\(x :: MultiPointFeature) -> codecRoundTrip x)
            quickCheck (\(x :: LineStringGeom) -> codecRoundTrip x)
            quickCheck (\(x :: LineStringFeature) -> codecRoundTrip x)
            quickCheck (\(x :: MultiLineStringGeom) -> codecRoundTrip x)
            quickCheck (\(x :: MultiLineStringFeature) -> codecRoundTrip x)
            in unit
      test "JavaScript FFI calls"
        $ liftEffect
        $ ado
            quickCheck (\(x :: PointGeom) -> jsRoundTrip x point)
            quickCheck (\(x :: MultiPointGeom) -> jsRoundTrip x multiPoint)
            quickCheck (\(x :: LineStringGeom) -> jsRoundTrip x lineString)
            quickCheck (\(x :: MultiLineStringGeom) -> jsRoundTrip x multiLineString)
            in unit

jsRoundTrip :: forall a. Eq a => Show a => EncodeJson a => a -> (a -> FeatureProperties -> Either JsonDecodeError (Feature a)) -> Result
jsRoundTrip geom f =
  let
    result = f geom empty
  in
    (featureGeometry <$> result) == Right geom <?> "Result: \n" <> show result <> "\nnot equal to expected:\n" <> show geom

codecRoundTrip :: forall a. Eq a => Show a => DecodeJson a => EncodeJson a => a -> Result
codecRoundTrip a =
  let
    encoded = encodeJson a

    decoded = decodeJson encoded
  in
    decoded == Right a <?> "Result: \n" <> show decoded <> "\nnot equal to expected:\n" <> show a
