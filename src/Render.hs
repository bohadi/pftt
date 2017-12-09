{-# LANGUAGE DuplicateRecordFields #-}

module Render (
    render
) where

import Util
import Scene

import Data.Tuple(uncurry)
import Data.Maybe(catMaybes)
import Data.List (minimumBy, foldl')
import Data.Ord  (comparing)

import System.FilePath
import Codec.Picture

import qualified Debug.Trace as D
--debug n x = D.trace ("\t"++(show n)++") "++(show x)) x
debug n x = x

fname = "./render.png" :: FilePath
shadowBias = 1e-13     :: Double

data Ray = Ray {
    base      :: Point
  , direction :: Vector
}

data Hit = Hit {
    element  :: Element
  , distance :: Double
  , hitP :: Point
  , hitN :: Vector
} deriving Show
-- get distance from ray base on hit element
hitDistance :: Element -> Ray -> Double -> Double
hitDistance e (Ray q _) x = case e of
    SElem _               -> x
    PElem (Plane p n _ _) ->
        (dotProd n $ convertPV $ p `subtracts` q) / x
-- get point of ray hit on hit element
hitPoint :: Double -> Ray -> Point
hitPoint distance (Ray p dir) = addPoint p $ dir `scaleBy` distance
-- get normal of element at point of hit
hitPointNormal :: Point -> Element -> Vector
hitPointNormal p e = case e of
    SElem (Sphere c _ _ _) -> normalize $ convertPV $ p `subtracts` c
    PElem (Plane  _ n _ _) -> n
hitConstr :: Element -> Ray -> Double -> Hit
hitConstr e ray x = Hit e dist hitP hitN where
    dist = hitDistance e ray x
    hitP = hitPoint dist ray
    hitN = hitPointNormal hitP e

primeR :: Res -> Double -> Int -> Int -> Ray
primeR (resW, resH) fov pixWCoord pixHCoord = Ray Origin direction where
    senseX = 2.0 * (fromIntegral pixWCoord + 0.5) / fromIntegral resW  - 1.0
    senseY = 2.0 * (fromIntegral pixHCoord + 0.5) / fromIntegral resH
    fov_adj = tan $ (deg2rad fov) / 2
    aspect = (fromIntegral resW) / fromIntegral resH
    direction = Vector (senseX*fov_adj*aspect) (senseY*fov_adj) 1.0

shadowR :: Hit -> Light -> Ray
shadowR (Hit _ _ p n) (PLight (PointLight q _ _)) = Ray o d where
    o = addPoint p $ n `scaleBy` shadowBias
    d = normalize . convertPV $ q `subtracts` p

intersect :: Ray -> Element -> Maybe Hit
intersect r se@(SElem (Sphere c rd _ _)) =
    let l  = convertPV $ c `subtracts` base r
        ad = dotProd l $ direction r
        d2 = (dotProd l l) - (ad * ad)
        r2 = rd * rd
        tc = sqrt $ r2 - d2
        t0 = ad - tc
        t1 = ad + tc
    in if d2 > r2 || (t0 < 0 && t1 < 0)
        then Nothing
        else Just $ hitConstr se r $ min t0 t1
intersect r pe@(PElem (Plane _ n _ _)) =
    let denom = dotProd n $ direction r
        h = hitConstr pe r denom
    in if denom < 1e-6 || distance h < 0 || isInfinite (distance h)
        then Nothing
        else Just h

intersectAll :: Ray -> [Element] -> [Hit]
intersectAll r elems = catMaybes $ (intersect r) <$> elems

incidence :: Hit -> Light -> Vector
incidence h (PLight (PointLight p _ _)) = convertPV $ p `subtracts` (hitP h)

intensityAt :: Hit -> Light -> Double
intensityAt h l@(PLight (PointLight _ _ i)) = i / (r2 * 4.0 * pi) where
    r2 = magnitude2 $ incidence h l

powerAt :: Hit -> Light -> Double
powerAt h l@(PLight (PointLight _ _ i)) = p * i' * d / pi where
    i' = intensityAt h l
    p = max 0.0 $ dotProd (hitN h) $ normalize $ incidence h l
    d = diffuse $ element h

diffusePart :: Hit -> Light -> Color
diffusePart h l = scaleColor (debug 2 c) (debug 3 p) where  
    c = mulColor (color $ Left $ element h) (color $ Right l)
    p = powerAt h l

specularPart :: Hit -> Light -> Color -- TODO
specularPart h l = Black

accumulateC :: [Color] -> Color  
accumulateC cs = foldl' addColor Black $ debug 4 cs

shade :: [Hit] -> [Light] -> Color
shade [] lights = Black
shade hits lights = accumulateC $ (diffusePart $ debug 1 hit) <$> lights where
    hit = minimumBy (comparing distance) hits
    --sRays = (shadowR hit) <$> lights
    --isLit = fmap (fmap (intersectAll srays) elems)
    --hitIsLitLights = filter (\x,y->x|y) isLit `zip` lights
    --ltPow = fmap (hit isLit) lights
    --specs = fmap (specularPart hit) lights

trace :: Scene -> Ray -> Color
trace (Scene elems lights) r = shade (intersectAll r elems) lights

saveRender :: Res -> [PixelRGB8] -> IO ()
saveRender (resW,resH) ps = do
    putStrLn $ "writing to " ++ fname
    savePngImage fname $
        ImageRGB8 (generateImage (\x y->ps !! (x+y*resW)) resW resH)
    putStrLn "finished"
    --mapM_ putStrLn (show <$> ps)

render :: Res -> Double -> Scene -> IO ()
render r@(resW,resH) fov scene = saveRender r (convertCP <$> cs) where
    coords = (,) <$> [0..resH-1] <*> [0..resW-1]
    rays = uncurry (primeR r fov) <$> coords
    -- OPT (parMap rpar), or MVar mpsc image buffer
    cs = (trace scene) <$> rays 
