# purescript-turf

[![CI](https://github.com/jisantuc/purescript-turf/workflows/CI/badge.svg?branch=main)](https://github.com/jisantuc/purescript-turf/actions?query=workflow%3ACI+branch%3Amain)
[![Release](http://img.shields.io/github/release/jisantuc/purescript-turf.svg)](https://github.com/jisantuc/purescript-turf/releases)
[![Maintainer: jisantuc](https://img.shields.io/badge/maintainer-jisantuc-teal.svg)](http://github.com/jisantuc)

PureScript FFI bindings into the [Turf JS](https://turfjs.org/) library.

## Installation

Install `turf` with [Spago](https://github.com/purescript/spago).

```sh
spago install turf
```

## Quick Start

Currently the only implemented Turf module is the helpers module, with the exception of
the `feature`, `featureCollection`, and `geometryCollection` methods.

This means that you can construct all of the non-collection and non-feature geometry types. For example, you can construct a `PointGeom`, then ship that off to Turf to get
a `PointFeature` back with the `point` method:

```purescript
import Turf.Helpers
import Foreign.Object as Object

pointGeom = PointGeom $ Tuple 3.0 4.0
properties = Object.empty

turfPoint = point pointGeom properties
-- -> Right (Feature PointGeom)
```

You'll get back a `Feature PointGeom`, which is a GeoJSON feature with a `PointGeom`
geometry.

## Documentation

Forthcoming

## Contributing

Forthcoming