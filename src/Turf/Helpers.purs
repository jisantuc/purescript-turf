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
  , PolygonGeom(..)
  , PolygonFeature
  , polygon
  , MultiPolygonGeom(..)
  , MultiPolygonFeature
  , multiPolygon
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

-- | FeatureProperties represent the JSON object that GeoJSON features
-- | have in their `properties` field.
type FeatureProperties
  = Object Json

-- | Coords are an alias for pairs of numbers, more or less matching the `Position`
-- | type alias in turf.
type Coord
  = Tuple Number Number

noCoordinates :: JsonDecodeError
noCoordinates = TypeMismatch "Expected to decode from object including coordinates key"

-- | A `PointGeom` is nothing more than a newtype over `Coords`
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

-- | A `MultiPointGeom` represents a collection of un-connected `PointGeoms`
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

-- | A `LineStringGeom` is at least two points forming a linestring.
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

-- | A `MultiLineStringGeom` is a collection of unrelated LineStringGeoms
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

-- | A `LinearRing` is a collection of at least four points where the first and last
-- | points are the same.
newtype LinearRing
  = LinearRing
  { first :: PointGeom
  , second :: PointGeom
  , third :: PointGeom
  , rest :: Array PointGeom
  }

-- | Construct a linear ring from the first three points and the remaining points.
-- | This method ensures that if you've failed to match the first and last points,
-- | a correct last point will be appended for you.
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

-- | A `PolygonGeom` represents an exterior with possibly zero holes.
-- | Note that nothing forbids you from making holes _outside_ the polygon.
-- | @turf/helpers `polygon` method also doesn't get mad about this, so
-- | while it doesn't really make sense, it's technically allowed.
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

polygonGeomToArray :: PolygonGeom -> Array (Array (Array Number))
polygonGeomToArray (PolygonGeom { exteriorRing, holes }) = linearRingToCoordinateArray <$> exteriorRing : holes

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
  encodeJson poly =
    encodeJson
      { type: "Polygon", coordinates: polygonGeomToArray poly
      }

-- | A `MultiPolygonGeom` represents a collection of `PolygonGeoms`.
newtype MultiPolygonGeom
  = MultiPolygonGeom (Array PolygonGeom)

derive newtype instance eqMultiPolygonGeom :: Eq MultiPolygonGeom

derive newtype instance showMultiPolygonGeom :: Show MultiPolygonGeom

derive newtype instance arbMultiPolygonGeom :: Arbitrary MultiPolygonGeom

instance decodeJsonMultiPolygonGeom :: DecodeJson MultiPolygonGeom where
  decodeJson js = case toObject js of
    Just obj ->
      obj .: "coordinates"
        >>= (\arrs -> MultiPolygonGeom <$> (traverse (polygonGeomFromArray js) arrs))
    Nothing -> Left noCoordinates

instance encodeJsonMultiPolygonGeom :: EncodeJson MultiPolygonGeom where
  encodeJson (MultiPolygonGeom polygons) =
    encodeJson
      { type: "MultiPolygon", coordinates: polygonGeomToArray <$> polygons
      }

-- | A `Feature a` represents a GeoJSON feature with geometry of type `a`.
data Feature a
  = Feature
    { geometry :: a
    , properties :: FeatureProperties
    }

-- | Since `turf` returns features from all of its helper methods, it's convenient
-- | to have a short-hand for getting the geometry back out.
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
  eq (Feature { geometry: geom1, properties: prop1 }) ( Feature
      { geometry: geom2, properties: prop2
    }
  ) = geom1 == geom2 && prop1 == prop2

-- | A `PointFeature` is a `Feature` with geometry of type `PointGeom`
type PointFeature
  = Feature PointGeom

-- | A `MultiPointFeature` is a `Feature` with geometry of type `MultiPointGeom`
type MultiPointFeature
  = Feature MultiPointGeom

-- | A `LineStringFeature` is a `Feature` with geometry of type `LineStringGeom`
type LineStringFeature
  = Feature LineStringGeom

-- | A `MultiLineStringFeature` is a `Feature` with geometry of type `MultiLineStringGeom`
type MultiLineStringFeature
  = Feature MultiLineStringGeom

-- | A `PolygonFeature` is a `Feature` with geometry of type `PolygonGeom`
type PolygonFeature
  = Feature PolygonGeom

-- | A `MultiPolygonFeature` is a `Feature` with geometry of type `MultiPolygonGeom`
type MultiPolygonFeature
  = Feature MultiPolygonGeom

foreign import pointImpl :: Fn2 (Array Number) FeatureProperties Json

-- | Construct a `PointFeature` using the @turf/helpers `point` method
point :: PointGeom -> FeatureProperties -> Either JsonDecodeError PointFeature
point geom props = decodeJson $ runFn2 pointImpl (toArray geom) props

foreign import multiPointImpl :: Fn2 (Array (Array Number)) FeatureProperties Json

-- | Construct a `MultiPointFeature` using the @turf/helpers `multiPoint` method
multiPoint :: MultiPointGeom -> FeatureProperties -> Either JsonDecodeError MultiPointFeature
multiPoint (MultiPointGeom points) props = decodeJson $ runFn2 multiPointImpl (toArray <$> points) props

foreign import lineStringImpl :: Fn2 (Array (Array Number)) FeatureProperties Json

-- | Construct a `LineStringFeature` using the @turf/helpers `lineString` method
lineString :: LineStringGeom -> FeatureProperties -> Either JsonDecodeError LineStringFeature
lineString lineStringGeom props = decodeJson $ runFn2 lineStringImpl (lineStringGeomToArray lineStringGeom) props

foreign import multiLineStringImpl :: Fn2 (Array (Array (Array Number))) FeatureProperties Json

-- | Construct a `MultiLineStringFeature` using the @turf/helpers `multiLineString` method
multiLineString :: MultiLineStringGeom -> FeatureProperties -> Either JsonDecodeError MultiLineStringFeature
multiLineString (MultiLineStringGeom lineStrings) props = decodeJson $ runFn2 multiLineStringImpl (lineStringGeomToArray <$> lineStrings) props

foreign import polygonImpl :: Fn2 (Array (Array (Array Number))) FeatureProperties Json

-- | Construct a `PolygonFeature` using the @turf/helpers `polygon` method
polygon :: PolygonGeom -> FeatureProperties -> Either JsonDecodeError PolygonFeature
polygon poly props =
  decodeJson
    $ runFn2 polygonImpl (polygonGeomToArray poly) props

foreign import multiPolygonImpl :: Fn2 (Array (Array (Array (Array Number)))) FeatureProperties Json

-- | Construct a `MultiPolygonFeature` using the @turf/helpers `multiPolygon` method
multiPolygon :: MultiPolygonGeom -> FeatureProperties -> Either JsonDecodeError MultiPolygonFeature
multiPolygon (MultiPolygonGeom polys) props =
  decodeJson
    $ runFn2 multiPolygonImpl (polygonGeomToArray <$> polys) props
