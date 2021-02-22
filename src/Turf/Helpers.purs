module Turf.Helpers
  ( Coord
  , Feature(..)
  , FeatureProperties
  , featureGeometry
  , LineStringGeom(..)
  , LineStringFeature
  , lineString
  , MultiLineStringGeom(..)
  , MultiLineStringFeature
  , multiLineString
  , PointGeom(..)
  , PointFeature
  , point
  , MultiPointGeom(..)
  , MultiPointFeature
  , multiPoint
  , LinearRing
  , mkLinearRing
  , PolygonGeom
  , PolygonFeature
  , polygon
  ) where

import Data.Argonaut.Core (Json, stringify, toObject)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson, (.:), (.:?))
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Array (drop, head, last, snoc, take, (:))
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple, fst, snd)
import Foreign.Object (Object, empty)
import Prelude (class Eq, class Show, apply, bind, map, pure, ($), (&&), (<$>), (<<<), (==), (>>=))
import Test.QuickCheck (class Arbitrary, arbitrary)

type FeatureProperties
  = Object Json

type Coord
  = Tuple Number Number

noCoordinates :: JsonDecodeError
noCoordinates = TypeMismatch "Expected to decode from object including coordinates key"

newtype PointGeom
  = PointGeom Coord

instance decodeJsonPointGeom :: DecodeJson PointGeom where
  decodeJson js = case toObject js of
    Just obj -> PointGeom <$> obj .: "coordinates"
    Nothing -> Left noCoordinates

instance encodeJsonPointGeom :: EncodeJson PointGeom where
  encodeJson (PointGeom coord) = encodeJson { type: "Point", coordinates: coord }

derive newtype instance eqPointGeom :: Eq PointGeom

derive newtype instance arbPointGeom :: Arbitrary PointGeom

derive newtype instance showPointGeom :: Show PointGeom

unPointGeom :: PointGeom -> Coord
unPointGeom (PointGeom c) = c

toArray :: PointGeom -> Array Number
toArray = (\tup -> [ fst tup, snd tup ]) <<< unPointGeom

newtype MultiPointGeom
  = MultiPointGeom (Array PointGeom)

fromCoordArray :: Array Coord -> MultiPointGeom
fromCoordArray = MultiPointGeom <<< (PointGeom <$> _)

derive newtype instance eqMultiPointGeom :: Eq MultiPointGeom

derive newtype instance arbMultiPointGeom :: Arbitrary MultiPointGeom

derive newtype instance showMultiPointGeom :: Show MultiPointGeom

instance decodeJsonMultiPointGeom :: DecodeJson MultiPointGeom where
  decodeJson js = case toObject js of
    Just obj -> do
      coords <- obj .: "coordinates"
      pure <<< MultiPointGeom $ PointGeom <$> coords
    Nothing -> Left noCoordinates

instance encodeMultiPointGeom :: EncodeJson MultiPointGeom where
  encodeJson (MultiPointGeom coords) =
    encodeJson
      { type: "MultiPoint", coordinates: toArray <$> coords
      }

newtype LineStringGeom
  = LineStringGeom
  { first :: PointGeom
  , next :: PointGeom
  , rest :: Array PointGeom
  }

lineStringGeomFromArray :: Json -> Array Coord -> Either JsonDecodeError LineStringGeom
lineStringGeomFromArray js arr = case take 2 arr of
  [ first, next ] -> Right $ LineStringGeom { first: PointGeom first, next: PointGeom next, rest: PointGeom <$> drop 2 arr }
  _ -> Left $ UnexpectedValue js

lineStringGeomToArray :: LineStringGeom -> Array (Array Number)
lineStringGeomToArray (LineStringGeom { first, next, rest }) = toArray <$> first : next : rest

derive newtype instance eqLineStringGeom :: Eq LineStringGeom

derive newtype instance arbLineStringGeom :: Arbitrary LineStringGeom

derive newtype instance showLineStringGeom :: Show LineStringGeom

instance decodeJsonLineStringGeom :: DecodeJson LineStringGeom where
  decodeJson js = case toObject js of
    Just obj -> obj .: "coordinates" >>= lineStringGeomFromArray js
    Nothing -> Left noCoordinates

instance encodeJsonLineStringGeom :: EncodeJson LineStringGeom where
  encodeJson (LineStringGeom { first, next, rest }) =
    encodeJson
      { type: "LineString", coordinates: toArray <$> first : next : rest
      }

newtype MultiLineStringGeom
  = MultiLineStringGeom (Array LineStringGeom)

derive newtype instance eqMultiLineStringGeom :: Eq MultiLineStringGeom

derive newtype instance arbMultiLineStringGeom :: Arbitrary MultiLineStringGeom

derive newtype instance showMultiLineStringGeom :: Show MultiLineStringGeom

instance decodeJsonMultiLineStringGeom :: DecodeJson MultiLineStringGeom where
  decodeJson js = case toObject js of
    Just obj ->
      obj .: "coordinates"
        >>= (\arrs -> MultiLineStringGeom <$> (traverse (lineStringGeomFromArray js) arrs))
    Nothing -> Left noCoordinates

instance encodeJsonMultiLineStringGeom :: EncodeJson MultiLineStringGeom where
  encodeJson (MultiLineStringGeom lineStrings) =
    encodeJson
      { type: "MultiLineString", coordinates: lineStringGeomToArray <$> lineStrings }

newtype LinearRing
  = LinearRing
  { first :: PointGeom
  , second :: PointGeom
  , third :: PointGeom
  , rest :: Array PointGeom
  }

mkLinearRing :: PointGeom -> PointGeom -> PointGeom -> Array PointGeom -> LinearRing
mkLinearRing first second third rest = fixRing $ LinearRing { first, second, third, rest }

linearRingFromCoordinateArray :: Array Coord -> Either JsonDecodeError LinearRing
linearRingFromCoordinateArray coords = case take 3 coords of
  [ first, second, third ] ->
    Right
      $ mkLinearRing
          (PointGeom first)
          (PointGeom second)
          (PointGeom third)
          (PointGeom <$> drop 3 coords)
  _ -> Left $ TypeMismatch "Expected at least three points to construct a linear ring"

linearRingToCoordinateArray :: LinearRing -> Array (Array Number)
linearRingToCoordinateArray (LinearRing { first, second, third, rest }) = toArray <$> first : second : third : rest

derive newtype instance eqLinearRing :: Eq LinearRing

derive newtype instance showLinearRing :: Show LinearRing

instance arbLinearRing :: Arbitrary LinearRing where
  arbitrary = do
    first <- arbitrary
    second <- arbitrary
    third <- arbitrary
    pure $ mkLinearRing first second third []

newtype PolygonGeom
  = PolygonGeom
  { exteriorRing :: LinearRing
  , holes :: Array LinearRing
  }

fixRing :: LinearRing -> LinearRing
fixRing ring@(LinearRing { first, second, third, rest }) = case last rest of
  Just p -> if (first == p) then ring else LinearRing { first, second, third, rest: rest `snoc` first }
  Nothing -> LinearRing { first, second, third, rest: [ first ] }

polygonGeomFromArray :: Json -> Array (Array Coord) -> Either JsonDecodeError PolygonGeom
polygonGeomFromArray js coordArrays =
  let
    ringsDecoded = traverse linearRingFromCoordinateArray coordArrays
  in
    ringsDecoded
      >>= ( \rings -> case head rings of
            Just exterior -> Right $ PolygonGeom { exteriorRing: exterior, holes: drop 1 rings }
            Nothing -> Left $ UnexpectedValue js
        )

derive newtype instance eqPolygonGeom :: Eq PolygonGeom

instance arbPolygonGeom :: Arbitrary PolygonGeom where
  arbitrary = do
    exteriorRing <- arbitrary
    holes <- arbitrary
    pure
      $ PolygonGeom { exteriorRing, holes }

derive newtype instance showPolygonGeom :: Show PolygonGeom

instance decodeJsonPolygonGeom :: DecodeJson PolygonGeom where
  decodeJson js = case toObject js of
    Just obj ->
      obj .: "coordinates"
        >>= polygonGeomFromArray js
    Nothing -> Left $ TypeMismatch ""

instance encodeJsonPolygonGeom :: EncodeJson PolygonGeom where
  encodeJson (PolygonGeom { exteriorRing, holes }) = encodeJson { type: "Polygon", coordinates: linearRingToCoordinateArray <$> exteriorRing : holes }

newtype MultiPolygonGeom
  = MultiPolygonGeom (Array PolygonGeom)

data Feature a
  = Feature
    { geometry :: a
    , properties :: Object Json
    }

featureGeometry :: forall a. Feature a -> a
featureGeometry (Feature { geometry }) = geometry

instance decodeJsonFeature :: DecodeJson a => DecodeJson (Feature a) where
  decodeJson js = case toObject js of
    Just obj -> ado
      geometry <- obj .: "geometry"
      properties <- fromMaybe empty <$> obj .:? "properties"
      in Feature { geometry, properties }
    Nothing -> Left $ TypeMismatch "expected an object"

instance encodeJsonFeature :: EncodeJson a => EncodeJson (Feature a) where
  encodeJson (Feature { geometry, properties }) =
    "type" := encodeJson "Feature"
      ~> "geometry"
      := encodeJson geometry
      ~> "properties"
      := encodeJson properties

instance showFeature :: EncodeJson a => Show (Feature a) where
  show = stringify <<< encodeJson

instance arbFeature :: Arbitrary a => Arbitrary (Feature a) where
  arbitrary = do
    geometry <- arbitrary
    properties <- pure empty
    pure $ Feature { geometry, properties }

instance eqFeature :: Eq a => Eq (Feature a) where
  eq (Feature { geometry: geom1, properties: prop1 }) (Feature { geometry: geom2, properties: prop2 }) = geom1 == geom2 && prop1 == prop2

type PointFeature
  = Feature PointGeom

type MultiPointFeature
  = Feature MultiPointGeom

type LineStringFeature
  = Feature LineStringGeom

type MultiLineStringFeature
  = Feature MultiLineStringGeom

type PolygonFeature
  = Feature PolygonGeom

foreign import pointImpl :: Fn2 (Array Number) (Object Json) Json

point :: PointGeom -> FeatureProperties -> Either JsonDecodeError PointFeature
point geom props = decodeJson $ runFn2 pointImpl (toArray geom) props

foreign import multiPointImpl :: Fn2 (Array (Array Number)) (Object Json) Json

multiPoint :: MultiPointGeom -> FeatureProperties -> Either JsonDecodeError MultiPointFeature
multiPoint (MultiPointGeom points) props = decodeJson $ runFn2 multiPointImpl (toArray <$> points) props

foreign import lineStringImpl :: Fn2 (Array (Array Number)) (Object Json) Json

lineString :: LineStringGeom -> FeatureProperties -> Either JsonDecodeError LineStringFeature
lineString lineStringGeom props = decodeJson $ runFn2 lineStringImpl (lineStringGeomToArray lineStringGeom) props

foreign import multiLineStringImpl :: Fn2 (Array (Array (Array Number))) (Object Json) Json

multiLineString :: MultiLineStringGeom -> FeatureProperties -> Either JsonDecodeError MultiLineStringFeature
multiLineString (MultiLineStringGeom lineStrings) props = decodeJson $ runFn2 multiLineStringImpl (lineStringGeomToArray <$> lineStrings) props

foreign import polygonImpl :: Fn2 (Array (Array (Array Number))) (Object Json) Json

polygon :: PolygonGeom -> Object Json -> Either JsonDecodeError PolygonFeature
polygon (PolygonGeom { exteriorRing, holes }) props =
  let
    coords = exteriorRing : holes
  in
    decodeJson
      $ runFn2 polygonImpl (linearRingToCoordinateArray <$> coords) props
