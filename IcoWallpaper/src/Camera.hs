{-# LANGUAGE StrictData #-}

module Camera (
    Scene,
    Camera(..),
    renderNormal,
) where

import qualified Graphics.Rendering.OpenGL as GL
import Data.Maybe (catMaybes)
import LinearEqs
import Shapes

-- Type definitions
type ScreenLine = (Point, Point)
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
    GL.color (GL.Color3 1.0 1.0 1.0 :: GL.Color3 Float) >>
    (sequence_ $ map lineify $ concatMap lineList scene)
  where
    lineList :: Shape -> [ScreenLine]
    lineList shape = catMaybes $ map (projectEdge cam screen) shape
    vertexify :: Point -> IO ()
    vertexify (a, b) = GL.vertex $ GL.Vertex2 (a/(fromIntegral $ fst screen)) (b/(fromIntegral $ snd screen))
    lineify :: ScreenLine -> IO ()
    lineify (a, b) = vertexify a >> vertexify b

{-- Under construction
renderBlurred :: Renderer
renderBlurred c screen scen = undefined

renderBlurredDistort :: Renderer
renderBlurredDistort = undefined
--}

-- Renderer helper functions
projectPoint :: Camera -> (Int, Int) -> Vector3 -> Maybe Point
projectPoint camera screen point = fmap scale $ (planeBasis plane) >>= (\basis -> planeCoordinates plane basis intersection)
  where
    center = (pos camera) `vAdd` ((dist camera) `sProd` (dir camera))
    plane = Shapes.Plane center (dir camera)
    intersection = linePlaneIntrsct (Shapes.Line (pos camera) point) plane
    scale (a, b) = (a*ratio, b*ratio)
    ratio = (fromIntegral $ fst screen) / (width camera)

projectEdge :: Camera -> (Int, Int) -> Edge -> Maybe ScreenLine
projectEdge camera screen (Edge a b) = do {p1 <- projectF a; p2 <- projectF b; return (p1, p2)}
  where projectF = projectPoint camera screen

{-- Under construction
projectBlurredEdge :: Camera -> (Int, Int) -> Edge -> [Point]
projectBlurredEdge camera screen (Edge a b) = undefined
--}
