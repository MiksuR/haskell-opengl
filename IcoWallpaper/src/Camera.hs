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
import Control.Monad (guard)
import Data.Either.Extra (eitherToMaybe)
import Data.Maybe (catMaybes, mapMaybe)
import System.Random

import LinearEqs
import Shapes

-- Type definitions
type ScreenLine = (Point, Point)
type Scene = [Shape]
data Camera = Camera {
    pos :: Vector3,
    dir :: Vector3,
    dist :: Float}
type Renderer = (Int, Int) -> Camera -> Scene -> IO()
-- The three Floats correspond to f, m and e in the inconvergent's article
data BlurParams = Blur Float Float Float

-- Renderers
renderNormal :: Renderer
renderNormal screen cam scene = GL.renderPrimitive GL.Lines $
    GL.color (GL.Color3 1.0 1.0 1.0 :: GL.Color3 Float) >>
    mapM_ lineify (concatMap lineList scene)
  where
    lineList :: Shape -> [ScreenLine]
    lineList = mapMaybe (projectEdge cam)
    vertexify :: Point -> IO ()
    vertexify (a, b) = GL.vertex $ GL.Vertex2 a (b*ratio)
    ratio = fromIntegral (fst screen)/fromIntegral (snd screen)
    lineify :: ScreenLine -> IO ()
    lineify (a, b) = vertexify a >> vertexify b

renderBlurred :: StdGen -> BlurParams -> Int -> Float -> Renderer
renderBlurred rand params samples alpha screen cam scene = GL.renderPrimitive GL.Points $
    GL.color (GL.Color4 1.0 1.0 1.0 alpha :: GL.Color4 Float) >>
    mapM_ vertexify (concatMap (catMaybes . maybeBlur) scene)
  where
    vertexify :: Point -> IO ()
    vertexify (a, b) = GL.vertex $ GL.Vertex2 a (b*ratio)
    ratio = fromIntegral (fst screen)/fromIntegral (snd screen)
    maybeBlur :: Shape -> [Maybe Point]
    maybeBlur = concatMap (projectBlurredEdge rand params samples cam)

{-- Under construction
renderBlurredDistort :: Renderer
renderBlurredDistort = undefined
--}

-- Renderer helper functions
projectPoint :: Camera -> Vector3 -> Maybe Point
projectPoint camera point = guard checkSide >> eitherToMaybe (planeBasis plane) >>= (\basis -> planeCoordinates plane basis intersection)
  where
    center = pos camera `vAdd` (dist camera `sProd` dir camera) -- Calculates the center point of the plane where the scene will be projected on to
    plane = Shapes.Plane center (dir camera)
    intersection = linePlaneIntrsct (Shapes.Line (pos camera) point) plane
    checkSide = dir camera `vDot` (point `vSubs` pos camera) >= 0 -- Checks wether the point is in front or behind of the camera

projectDisplaced :: BlurParams -> Camera -> (Vector3, StdGen) -> Maybe Point
projectDisplaced (Blur f m e) camera (point, rand) = projectPoint camera displaced
  where
    displaced = rndSphere rand r `vAdd` point
    d = vDist (pos camera) point
    r = (*) m $ abs(f-d)**e

-- TODO: Come up with a way to handle the case when only the other end of an edge is Nothing in projectEdge function.
-- Basically you'd have to take the intersection of the plane and the line between a and b,
-- and use this new point as the other end of the edge to calculate the projection.
projectEdge :: Camera -> Edge -> Maybe ScreenLine
projectEdge camera (Edge a b) = let projectF = projectPoint camera in liftA2 (,) (projectF a) (projectF b)

projectBlurredEdge :: StdGen -> BlurParams -> Int -> Camera -> Edge -> [Maybe Point]
projectBlurredEdge rand params samples camera edge =
  map (projectDisplaced params camera) (rndVectorsOnEdge rand samples edge)

-- Random generators for blurred rendering.
rndVectorsOnEdge :: StdGen -> Int -> Edge -> [(Vector3, StdGen)]
rndVectorsOnEdge rand samples edge = zip vectors (generators r2)
  where
    vectors = map (lerp edge) (take samples $ randoms r1 :: [Float])
    (r1, r2) = split rand

rndSphere :: StdGen -> Float -> Vector3
rndSphere rand r = sProd r $ fromSpherical (acos $ 2*v - 1) (2*pi*u)
  where
    (u, newRand) = randomR (0, 1) rand
    (v, _) = randomR (0, 1) newRand

generators :: StdGen -> [StdGen]
generators g = g1 : generators g2
  where (g1, g2) = split g
