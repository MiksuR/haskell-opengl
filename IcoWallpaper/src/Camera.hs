module Camera (
    Scene,
    Camera,
    renderScene,
    NormalCam(..),
    BlurredCam(..),
) where

import Graphics.Gloss hiding (Line)
import LinearEqs
import Shapes

type Scene = [Shape]

-- Maybe redifine:
-- data Camera = Camera {pos, dir, width, dist}
-- data Renderer = Renderer {renderScene :: (Camera -> (Int, Int) -> Scene -> Picture)}

-- Define the Camera class
class Camera camera where
    position :: camera -> Vector3
    direction :: camera -> Vector3
    pictureWidth :: camera -> Float
    distanceToScreen :: camera -> Float
    renderScene :: camera -> (Int, Int) -> Scene -> Picture

-- Define Camera types
data NormalCam = NormalCam {
    pos :: Vector3,
    dir :: Vector3,
    width :: Float,
    dist :: Float}
instance Camera NormalCam where
    position = pos
    direction = dir
    pictureWidth = width
    distanceToScreen = dist
    renderScene c screen scen = pictures . concat $ map shapeToLines scen
        where shapeToLines = map $ line . (projectEdge c screen)

data BlurredCam = BlurredCam {
    bPos :: Vector3,
    bDir :: Vector3,
    bWidth :: Float,
    bDist :: Float,
    fDist :: Float}
instance Camera BlurredCam where
    position = bPos
    direction = bDir
    pictureWidth = bWidth
    distanceToScreen = bDist
    renderScene c screen scen = pointsToPicture $ map shapeToPoints scen
        where shapeToPoints = undefined

-- Camera helper functions
projectPoint :: (Camera c) => c -> (Int, Int) -> Vector3 -> Point
projectPoint camera screen point = scale $ planeCoordinates plane (planeBasis plane) intersection
    where
        center = (position camera) `vAdd` ((distanceToScreen camera) `sProd` (direction camera))
        plane = Plane center (direction camera)
        intersection = linePlaneIntrsct (Line (position camera) point) plane
        scale (a, b) = (a*ratio, b*ratio)
        ratio = (fromIntegral $ fst screen) / (pictureWidth camera)

projectEdge :: NormalCam -> (Int, Int) -> Edge -> Path
projectEdge camera screen (Edge a b) = map (projectPoint camera screen) [a, b]

projectBlurredEdge :: BlurredCam -> (Int, Int) -> Edge -> [Point]
projectBlurredEdge camera screen (Edge a b) = undefined

pointsToPicture :: [Point] -> Picture -- Change to bitmap IG
pointsToPicture = undefined
