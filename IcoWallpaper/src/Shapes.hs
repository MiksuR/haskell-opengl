{-# LANGUAGE StrictData #-}

module Shapes (
    Edge(..),
    Line(..),
    Plane(..),
    Shape,
    rotateShape,
    linePlaneIntrsct,
    planeBasis,
    planeCoordinates,
    lerp,
    icosahedron,
    uprightIco,
    phi,
) where

import LinearEqs

-- Primitive type definitions
data Edge = Edge Vector3 Vector3 deriving Show
instance Eq Edge where
    Edge a b == Edge c d
        = (a == c && b == d) || (a == d && b == c)
type Shape = [Edge]

-- Shape transformations
rotateShape :: Float -> Float -> Float -> Shape -> Shape
rotateShape x y z = map rotateEdges
    where
        rotateEdges (Edge a b) = Edge (rotatePoint a) (rotatePoint b)
        rotatePoint = transform3 $ rotationZ z `mProd` rotationY y `mProd` rotationX x

-- Calculations with lines and planes
data Line = Line Vector3 Vector3
data Plane = Plane { p :: Vector3, n :: Vector3} -- point and normal

linePlaneIntrsct :: Line -> Plane -> Vector3
linePlaneIntrsct (Line l0 l1) plane = d `sProd` (l1 `vSubs` l0) `vAdd` l0
  where d = ((p plane `vSubs` l0) `vDot` n plane)/(l1 `vSubs` l0 `vDot` n plane)

type Basis = (Vector3, Vector3)

planeBasis :: Plane -> Either String Basis
planeBasis (Plane p n) = (,) <$> planeX <*> planeY
  where
    planeX = vNormalize $ n `vCross` Vector3 0 0 1
    planeY = (vCross <$> planeX <*> Right n) >>= vNormalize

-- Given a plane, a basis on the plane and a point on the plane, return a 2D coordinates of the point on the plane.
planeCoordinates :: Plane -> Basis -> Vector3 -> Maybe Point
planeCoordinates (Plane (Vector3 px py pz) _) (Vector3 xx xy xz, Vector3 yx yy yz) (Vector3 a b c) = solve2Eqs [eq0, eq1, eq2]
  where
    eq0 = Eq2 xx yx (a-px)
    eq1 = Eq2 xy yy (b-py)
    eq2 = Eq2 xz yz (c-pz)

lerp :: Edge -> Float -> Vector3
lerp (Edge a b) t = vAdd a $ sProd t $ b `vSubs` a

-- Icosahedron points
phi = (1 + sqrt 5)/2
ip1 = Vector3 1 0 phi
ip2 = Vector3 (-1) 0 phi
ip3 = Vector3 0 phi 1
ip4 = Vector3 0 (-phi) 1
ip5 = Vector3 phi 1 0
ip6 = Vector3 (-phi) 1 0
ip7 = Vector3 phi (-1) 0
ip8 = Vector3 (-phi) (-1) 0
ip9 = Vector3 0 phi (-1)
ip10 = Vector3 0 (-phi) (-1)
ip11 = Vector3 1 0 (-phi)
ip12 = Vector3 (-1) 0 (-phi)

icosahedron :: Shape
icosahedron = [Edge ip1 ip2,
                Edge ip1 ip3, Edge ip1 ip4,
                Edge ip2 ip3, Edge ip2 ip4,
                Edge ip1 ip5, Edge ip1 ip7,
                Edge ip2 ip6, Edge ip2 ip8,
                Edge ip3 ip5, Edge ip3 ip6,
                Edge ip4 ip7, Edge ip4 ip8,
                Edge ip3 ip9, Edge ip4 ip10,
                Edge ip5 ip7, Edge ip6 ip8,
                Edge ip5 ip9, Edge ip6 ip9,
                Edge ip7 ip10, Edge ip8 ip10,
                Edge ip5 ip11, Edge ip7 ip11,
                Edge ip6 ip12, Edge ip8 ip12,
                Edge ip9 ip11, Edge ip9 ip12,
                Edge ip10 ip11, Edge ip10 ip12,
                Edge ip11 ip12]

uprightIco :: Shape
uprightIco = rotateShape 0 (atan $ 1/phi) 0 icosahedron
