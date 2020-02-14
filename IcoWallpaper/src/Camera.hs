{-# LANGUAGE StrictData #-}

module Camera (
    Scene,
    Camera(..),
    renderNormal,
) where

import qualified Graphics.Rendering.OpenGL as GL
import LinearEqs
import Shapes

-- Type definitions
type Scene = [Shape]
data Camera = Camera {
    pos :: Vector3,
    dir :: Vector3,
    width :: Float,
    dist :: Float}
type Renderer = (Int, Int) -> Camera -> Scene -> IO()

-- Renderers
renderNormal :: Renderer
renderNormal screen cam scene = GL.renderPrimitive GL.Lines $
    GL.color (GL.Color3 1.0 1.0 1.0 :: GL.Color3 Float) >>=
    (\_ -> sequence_ $ map vertexify pointList)
  where
    pointList = concatMap (concatMap $ projectEdge cam screen) scene
    vertexify (a, b) = GL.vertex $ GL.Vertex2 (a/(fromIntegral $ fst screen)) (b/(fromIntegral $ snd screen))

{-- Under construction
renderBlurred :: Renderer
renderBlurred c screen scen = undefined

renderBlurredDistort :: Renderer
renderBlurredDistort = undefined
--}

-- Renderer helper functions
projectPoint :: Camera -> (Int, Int) -> Vector3 -> Point
projectPoint camera screen point = scale $ planeCoordinates plane (planeBasis plane) intersection
  where
    center = (pos camera) `vAdd` ((dist camera) `sProd` (dir camera))
    plane = Shapes.Plane center (dir camera)
    intersection = linePlaneIntrsct (Shapes.Line (pos camera) point) plane
    scale (a, b) = (a*ratio, b*ratio)
    ratio = (fromIntegral $ fst screen) / (width camera)

projectEdge :: Camera -> (Int, Int) -> Edge -> [Point]
projectEdge camera screen (Edge a b) = map (projectPoint camera screen) [a, b]

{-- Under construction
projectBlurredEdge :: Camera -> (Int, Int) -> Edge -> [Point]
projectBlurredEdge camera screen (Edge a b) = undefined
--}
