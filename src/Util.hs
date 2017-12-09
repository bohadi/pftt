{-# LANGUAGE DuplicateRecordFields #-}

module Util (
    Res(..)
  , Point(..) , Vector(..)
  , Color(..)
  , addPoint  , scaleBy
  , negateP   , subtracts
  , magnitude , magnitude2
  , normalize
  , dotProd
  , convertPV , convertVP
  , addColor  , mulColor
  , scaleColor
  , red, green, blue
  , convertCP , convertPC
  , deg2rad
) where

import Data.Word(Word8)
import Codec.Picture.Types

type Res = (Int, Int)

-- TODO use newtype, type
data Point  = Origin | Point { -- TODO remove Origin
    x_ :: Double
  , y_ :: Double
  , z_ :: Double
} deriving Show

data Vector = ZeroVec | UnitVec | Vector { -- TODO remove ZeroVec, UnitVec
    x_ :: Double
  , y_ :: Double
  , z_ :: Double
} deriving Show

-- point vector conversion
convertPV :: Point -> Vector
convertPV Origin = ZeroVec
convertPV (Point x y z) = Vector x y z
convertVP :: Vector -> Point
convertVP ZeroVec = Origin
convertVP UnitVec = Point 1 1 1
convertVP (Vector x y z) = Point x y z
-- get 1-norm squared
magnitude2 :: Vector -> Double
magnitude2 ZeroVec = 0.0
magnitude2 UnitVec = 1.0
magnitude2 (Vector x y z) = x^2 + y^2 + z^2
-- get 1-norm
magnitude :: Vector -> Double
magnitude ZeroVec = 0.0
magnitude UnitVec = 1.0
magnitude (Vector x y z) = sqrt (x^2 + y^2 + z^2)
-- normalize vector by 1-norm
normalize :: Vector -> Vector
normalize ZeroVec = ZeroVec
normalize UnitVec = UnitVec
normalize v@(Vector x y z) =
    Vector (x/n) (y/n) (z/n) where n = magnitude v
-- vector dot/inner product
dotProd :: Vector -> Vector -> Double
dotProd ZeroVec _ = 0
dotProd _ ZeroVec = 0
dotProd (Vector a b c) (Vector x y z) = (a*x)+(b*y)+(c*z)
-- scalar mult vector to point
scaleBy :: Vector -> Double -> Point
scaleBy ZeroVec _ = Origin
scaleBy UnitVec s = Point s s s
scaleBy (Vector x y z) s = Point (s*x) (s*y) (s*z)
-- point addition
addPoint :: Point -> Point -> Point 
addPoint Origin q = q
addPoint p Origin = p
addPoint (Point a b c) (Point x y z) = Point (a+x) (b+y) (c+z)
-- point negation
negateP :: Point -> Point
negateP Origin = Origin
negateP (Point x y z) = Point (-x) (-y) (-z)
-- point subtraction
subtracts :: Point -> Point -> Point 
subtracts = addPoint . negateP

data Color = White | Black | Color {
    r :: Double
  , g :: Double
  , b :: Double
} deriving Show

red, green, blue :: Color
red   = Color 1.0 0.0 0.0
green = Color 0.0 1.0 0.0
blue  = Color 0.0 0.0 1.0
-- TODO

-- color addition
addColor :: Color -> Color -> Color
addColor Black q = q
addColor p Black = p
addColor White q = White
addColor p White = White
addColor (Color a b c) (Color x y z) =
    Color (min 1.0 a+x) (min 1.0 b+y) (min 1.0 c+z)
-- color multiplication
mulColor :: Color -> Color -> Color
mulColor Black _ = Black
mulColor _ Black = Black
mulColor White q = q
mulColor p White = p
mulColor (Color a b c) (Color x y z) = Color (a*x) (b*y) (c*z)
-- color scalar multiplication
scaleColor :: Color -> Double -> Color
scaleColor Black _ = Black
scaleColor White _ = White
scaleColor (Color r g b) s = Color (min 1.0 s*r) (min 1.0 s*g) (min 1.0 s*b)
-- Color to PixelRGB8
convertCP :: Color -> PixelRGB8
convertCP Black = PixelRGB8 0 0 0
convertCP White = PixelRGB8 255 255 255
convertCP (Color r g b) = PixelRGB8 (convertCW r) (convertCW g) (convertCW b)
    where convertCW :: Double -> Word8
          convertCW x = round (x*255)
-- PixelRGB8 to Color
convertPC :: PixelRGB8 -> Color 
convertPC (PixelRGB8 r g b) = Color (convertWC r) (convertWC g) (convertWC b)
    where convertWC :: Word8 -> Double
          convertWC x = fromIntegral x/255
-- degrees to radians
deg2rad :: Double -> Double
deg2rad = (* pi) . (/ 180)
