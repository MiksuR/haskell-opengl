{-# LANGUAGE StrictData #-}

module Camera (
    Scene,
    Camera(..),
    renderNormal,
    renderBlurred,
) where

import Graphics.Rendering.OpenGL as GL
import LinearEqs as L
import Shapes

-- Type definitions
type Scene = [Shape]
data Camera = Camera {
    pos :: L.Vector3,
    dir :: L.Vector3,
    width :: Float,
    dist :: Float}
type Renderer = (Int, Int) -> Camera -> Scene -> IO()

-- Renderers
renderNormal :: Renderer
renderNormal screen cam scene = GL.renderPrimitive GL.Lines $
    GL.color (Color3 1.0 1.0 1.0 :: Color3 Float) >>=
    (\_ -> sequence_ $ map vertexify pointList)
  where
    pointList = concatMap (concatMap $ projectEdge cam screen) scene
    --pointList = [(0, 0), (0, 1), (0, 0), (1, 0), (0, 0), (-1, 0), (0, 0), (0, -1)] :: [(Float, Float)]
    vertexify (a, b) = vertex $ Vertex2 (a/(fromIntegral $ fst screen)) (b/(fromIntegral $ snd screen))

{--
renderNormal :: Renderer
renderNormal c screen scen = pictures . concat $ map shapeToLines scen
        where shapeToLines = map $ line . (projectEdge c screen)
--}

renderBlurred :: Renderer
renderBlurred c screen scen = undefined

renderBlurredDistort :: Renderer
renderBlurredDistort = undefined

-- Renderer helper functions
projectPoint :: Camera -> (Int, Int) -> L.Vector3 -> Point
projectPoint camera screen point = scale $ planeCoordinates plane (planeBasis plane) intersection
    where
        center = (pos camera) `vAdd` ((dist camera) `sProd` (dir camera))
        plane = Shapes.Plane center (dir camera)
        intersection = linePlaneIntrsct (Shapes.Line (pos camera) point) plane
        scale (a, b) = (a*ratio, b*ratio)
        ratio = (fromIntegral $ fst screen) / (width camera)

projectEdge :: Camera -> (Int, Int) -> Edge -> [Point]
projectEdge camera screen (Edge a b) = map (projectPoint camera screen) [a, b]

projectBlurredEdge :: Camera -> (Int, Int) -> Edge -> [Point]
projectBlurredEdge camera screen (Edge a b) = undefined

