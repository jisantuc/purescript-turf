module Test.Main where

import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Foreign.Object (empty)
import Prelude (class Show, class Eq, ($), (<>), (<$>), (==), discard, show, Unit)
import Test.QuickCheck (Result, quickCheck, (<?>))
import Test.Unit (suite, test)
import Test.Unit.Main (runTest)
import Turf.Helpers (Feature, FeatureProperties, LineStringFeature, LineStringGeom, MultiLineStringGeom, MultiPointFeature, MultiPointGeom, PointFeature, PointGeom, MultiLineStringFeature, featureGeometry, lineString, multiLineString, multiPoint, point)

main :: Effect Unit
main =
  runTest do
    suite "Codec round trips" do
      test "PointGeom" $ liftEffect $ quickCheck (\(x :: PointGeom) -> codecRoundTrip x)
      test "PointFeature" $ liftEffect $ quickCheck (\(x :: PointFeature) -> codecRoundTrip x)
      test "MultiPointGeom" $ liftEffect $ quickCheck (\(x :: MultiPointGeom) -> codecRoundTrip x)
      test "MultiPointFeature" $ liftEffect $ quickCheck (\(x :: MultiPointFeature) -> codecRoundTrip x)
      test "LineStringGeom" $ liftEffect $ quickCheck (\(x :: LineStringGeom) -> codecRoundTrip x)
      test "LineStringFeature" $ liftEffect $ quickCheck (\(x :: LineStringFeature) -> codecRoundTrip x)
      test "MultiLineStringGeom" $ liftEffect $ quickCheck (\(x :: MultiLineStringGeom) -> codecRoundTrip x)
      test "MultiLineStringFeature" $ liftEffect $ quickCheck (\(x :: MultiLineStringFeature) -> codecRoundTrip x)
    suite "JavaScript round trips" do
      test "Construct a point" $ liftEffect $ quickCheck (\(x :: PointGeom) -> jsRoundTrip x point)
      test "Construct a multipoint" $ liftEffect $ quickCheck (\(x :: MultiPointGeom) -> jsRoundTrip x multiPoint)
      test "Construct a linestring" $ liftEffect $ quickCheck (\(x :: LineStringGeom) -> jsRoundTrip x lineString)
      test "Construct a multilinestring" $ liftEffect $ quickCheck (\(x :: MultiLineStringGeom) -> jsRoundTrip x multiLineString)

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
