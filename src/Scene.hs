{-# LANGUAGE DuplicateRecordFields #-}

module Scene (
    Sphere(..)
  , Plane(..)
  , PointLight(..)
  , Element(..)
  , Light(..)
  , Scene(..)
  , color , diffuse
) where

import Util

-- TODO use newtype, type
data Sphere = Sphere {
    center  :: Point
  , radius  :: Double
  , color_   :: Color
  , diffuse_ :: Double
} deriving (Show)

data Plane = Plane {
    origin  :: Point
  , normal  :: Vector
  , color_   :: Color
  , diffuse_ :: Double
} deriving (Show)

data PointLight = PointLight {
    position  :: Point
  , color_    :: Color 
  , intensity :: Double
} deriving (Show)

data Element = SElem Sphere | PElem Plane
    deriving Show

data Light = PLight PointLight
    deriving Show

data Scene = Scene {
    elements  :: [Element] -- OPT elem octree / space partitioning
  , lights    :: [Light]
} deriving Show

-- TODO lenses
color :: Either Element Light -> Color
color (Left  (SElem (Sphere _ _ c _))) = c
color (Left  (PElem (Plane  _ _ c _))) = c
color (Right (PLight (PointLight _ c _))) = c
diffuse :: Element -> Double
diffuse (SElem (Sphere _ _ _ d)) = d
diffuse (PElem (Plane  _ _ _ d)) = d






