module Pftt (
    testPftt
  , res
  , fov
) where

import Util
import Scene
import Render

res = (12, 8) :: Res
fov = 70.0        :: Double

testPftt :: IO ()
testPftt = render res testScene

testScene = Scene {
    elements = [
        PElem $ Plane (Point 0 (-1) 0) (Vector 0 1 0) green 1.0
      , PElem $ Plane (Point 0 0 (-10)) (Vector 0 0 1) blue 1.0
      --, SElem $ Sphere Origin 1.0 red 1.0
      ]
  , lights = [
        PLight $ PointLight (Point 0 0 0) White 100.0
      ]
}

