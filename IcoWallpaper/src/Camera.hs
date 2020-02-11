{-# LANGUAGE StrictData #-}

module Camera (
    Scene,
    Camera(..),
    --renderNormal,
    renderBlurred
) where

import Graphics.Rendering.OpenGL.GL.VertexSpec (Color4)
import LinearEqs
import Shapes

-- Type definitions
type Scene = [Shape]
data Camera = Camera {
    pos :: Vector3,
    dir :: Vector3,
    width :: Float,
    dist :: Float}
type Picture = [Color4 Float]
type Renderer = Camera -> (Int, Int) -> Scene -> Picture

-- Renderers
{--
renderNormal :: Renderer
renderNormal c screen scen = pictures . concat $ map shapeToLines scen
        where shapeToLines = map $ line . (projectEdge c screen)
--}

renderBlurred :: Renderer
renderBlurred c screen scen = pointsToPicture $ map shapeToPoints scen
        where shapeToPoints = undefined

renderBlurredDistort :: Renderer
renderBlurredDistort = undefined

-- Renderer helper functions
projectPoint :: Camera -> (Int, Int) -> Vector3 -> Point
projectPoint camera screen point = scale $ planeCoordinates plane (planeBasis plane) intersection
    where
        center = (pos camera) `vAdd` ((dist camera) `sProd` (dir camera))
        plane = Plane center (dir camera)
        intersection = linePlaneIntrsct (Line (pos camera) point) plane
        scale (a, b) = (a*ratio, b*ratio)
        ratio = (fromIntegral $ fst screen) / (width camera)

projectEdge :: Camera -> (Int, Int) -> Edge -> Path
projectEdge camera screen (Edge a b) = map (projectPoint camera screen) [a, b]

projectBlurredEdge :: Camera -> (Int, Int) -> Edge -> [Point]
projectBlurredEdge camera screen (Edge a b) = undefined

pointsToPicture :: [Point] -> Picture -- Change to bitmap IG
pointsToPicture = undefined
