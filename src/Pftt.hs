module Pftt (
    testPftt
  , res
  , fov
) where

import Util
import Scene
import Render

import System.TimeIt

res = (80, 60)  :: Res
fov = 70.0      :: Double

testPftt :: IO ()
testPftt = timeIt $ render res fov testScene

n = (-1)
testScene = Scene {
    elements = [
          PElem $ Plane (Point 1 0 0) (Vector n 0 0) green 1.0
        , SElem $ Sphere (Point 1 0 0) 1.0 red 1.0
      ]
  , lights = [
        PLight $ PointLight (Point 0 0 0) White 100.0
      ]
}

