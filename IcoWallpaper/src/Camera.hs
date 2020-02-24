{-# LANGUAGE StrictData #-}

module Camera (
    Scene,
    Camera(..),
    BlurParams(..),
    renderNormal,
    renderBlurred,
) where

import qualified Graphics.Rendering.OpenGL as GL
import Control.Applicative (liftA2)
import Data.Maybe (catMaybes)
import System.Random

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
-- The three Floats correspond to f, m and e in the inconvergent's article
data BlurParams = Blur Float Float Float

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

renderBlurred :: StdGen -> BlurParams -> Int -> Float -> Renderer
renderBlurred rand params samples alpha screen cam scene = GL.renderPrimitive GL.Points $
    GL.color (GL.Color4 1.0 1.0 1.0 alpha :: GL.Color4 Float) >>
    (sequence_ $ map vertexify $ concatMap (catMaybes . maybeBlur) scene)
  where
    vertexify :: Point -> IO ()
    vertexify (a, b) = GL.vertex $ GL.Vertex2 (a/(fromIntegral $ fst screen)) (b/(fromIntegral $ snd screen))
    maybeBlur :: Shape -> [Maybe Point]
    maybeBlur = concatMap (projectBlurredEdge rand params samples cam screen)

{-- Under construction
renderBlurredDistort :: Renderer
renderBlurredDistort = undefined
--}

-- Renderer helper functions
-- TODO 1: Make it so that projectPoint returns Nothing when the target point is on the wrong side of the plane i.e. camera cannot see it.
-- TODO 2: Come up with a way to handle the case when only the other end of an edge is Nothing in projectEdge function.
-- TODO 3: Maybe add orthographic projections later.
-- TODO 4: Remove scaling here and in render functions, because OpenGL assumes 1 x 1 screen.
projectPoint :: Camera -> (Int, Int) -> Vector3 -> Maybe Point
projectPoint camera screen point = fmap scale $ (planeBasis plane) >>= (\basis -> planeCoordinates plane basis intersection)
  where
    center = (pos camera) `vAdd` ((dist camera) `sProd` (dir camera))
    plane = Shapes.Plane center (dir camera)
    intersection = linePlaneIntrsct (Shapes.Line (pos camera) point) plane
    scale (a, b) = (a*ratio, b*ratio)
    ratio = (fromIntegral $ fst screen) / (width camera)

projectDisplaced :: BlurParams -> Camera -> (Int, Int) -> (Vector3, StdGen) -> Maybe Point
projectDisplaced (Blur f m e) camera screen (point, rand)= projectPoint camera screen displaced
  where
    displaced = rndSphere rand r `vAdd` point
    d = vDist (pos camera) point
    r = (*) m $ (abs(f-d))**e

-- TODO: Come up with a smarted way of doing liftA2 (,) (f a) (f b)
projectEdge :: Camera -> (Int, Int) -> Edge -> Maybe ScreenLine
projectEdge camera screen (Edge a b) = liftA2 (,) (projectF a) (projectF b)
  where projectF = projectPoint camera screen

projectBlurredEdge :: StdGen -> BlurParams -> Int -> Camera -> (Int, Int) -> Edge -> [Maybe Point]
projectBlurredEdge rand params samples camera screen edge =
  map (projectDisplaced params camera screen) (rndVectorsOnEdge rand samples edge)

-- Random generators for blurred rendering.
rndVectorsOnEdge :: StdGen -> Int -> Edge -> [(Vector3, StdGen)]
rndVectorsOnEdge rand samples edge = zip vectors (generators r2)
  where
    vectors = map (lerp edge) (take samples $ randoms r1 :: [Float])
    (r1, r2) = split rand

rndSphere :: StdGen -> Float -> Vector3
rndSphere rand r = sProd r $ fromSpherical theta phi
  where
    (theta, newRand) = randomR (0, pi) (rand)
    (phi, _) = randomR (0, 2*pi) (newRand)

generators :: StdGen -> [StdGen]
generators g = g1:(generators g2)
  where (g1, g2) = split g
