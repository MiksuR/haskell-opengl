{-# LANGUAGE StrictData #-}

module LinearEqs (
    Point (..),
    Path (..),
    Vector3(..),
    Eq2(..),
    vAdd,
    vSubs,
    sProd,
    vDot,
    vCross,
    vNormalize,
    mProd,
    solve2Eqs,
    transform3,
    rotationX,
    rotationY,
    rotationZ
) where


-- Primitive type definitions
type Point = (Float, Float)
type Path = [Point]
data Vector3 = Vector3 Float Float Float deriving (Eq, Show)
data Mat2 = Mat2 {x11 :: Float, x12 :: Float, x21 :: Float, x22 :: Float}
data Mat3 = Mat3 Float Float Float Float Float Float Float Float Float
data Eq2 = Eq2 {eq0 :: Float, eq1 :: Float, val :: Float} deriving Show -- eq0*x + eq1*y = val, where x and y are the variables
type Eqs = [Eq2] -- System of equations of two variables

-- Vector arithmetic

vAdd :: Vector3 -> Vector3 -> Vector3
vAdd (Vector3 x0 y0 z0) (Vector3 x1 y1 z1) = Vector3 (x0+x1) (y0+y1) (z0+z1)
vSubs :: Vector3 -> Vector3 -> Vector3
vSubs (Vector3 x0 y0 z0) (Vector3 x1 y1 z1) = Vector3 (x0-x1) (y0-y1) (z0-z1)
sProd :: Float -> Vector3 -> Vector3
sProd a (Vector3 x y z) = Vector3 (a*x) (a*y) (a*z)
vDot :: Vector3 -> Vector3 -> Float
vDot (Vector3 x0 y0 z0) (Vector3 x1 y1 z1) = x0*x1 + y0*y1+ z0*z1
vCross :: Vector3 -> Vector3 -> Vector3
vCross (Vector3 x0 y0 z0) (Vector3 x1 y1 z1) = Vector3 (y0*z1-y1*z0) (x0*z1-x1*z0) (x0*y1-x1*y0)
vNormalize :: Vector3 -> Vector3
vNormalize (Vector3 x y z) = Vector3 (x/mag) (y/mag) (z/mag)
    where mag = sqrt $ x*x + y*y + z*z
mProd :: Mat3 -> Mat3 -> Mat3
mProd (Mat3 x11 x12 x13 x21 x22 x23 x31 x32 x33) (Mat3 y11 y12 y13 y21 y22 y23 y31 y32 y33) = 
    Mat3 (x1 `vDot` y1) (x1 `vDot` y2) (x1 `vDot` y3) (x2 `vDot` y1) (x2 `vDot` y2) (x2 `vDot` y3) (x3 `vDot` y1) (x3 `vDot` y2) (x3 `vDot` y3)
    where
        x1 = Vector3 x11 x12 x13
        x2 = Vector3 x21 x22 x23
        x3 = Vector3 x31 x32 x33
        y1 = Vector3 y11 y21 y31
        y2 = Vector3 y12 y22 y32
        y3 = Vector3 y13 y23 y33

-- Matrices and linear transforms

determinant :: Mat2 -> Float
determinant (Mat2 a b c d) = a*d-b*c

inverse2x2 :: Mat2 -> Mat2
inverse2x2 (Mat2 a b c d) = Mat2 (d/det) (-b/det) (-c/det) (a/det)
    where det = determinant $ Mat2 d (-b) (-c) a

transform2 :: Mat2 -> Point -> Point
transform2 (Mat2 a b c d) (e, f) = (a*e+b*f, c*e+d*f)

transform3 :: Mat3 -> Vector3 -> Vector3
transform3 (Mat3 x11 x12 x13 x21 x22 x23 x31 x32 x33) (Vector3 a b c) = Vector3 x y z
    where
        x = a*x11 + b*x12 + c*x13
        y = a*x21 + b*x22 + c*x23
        z = a*x31 + b*x32 + c*x33

rotationX :: Float -> Mat3
rotationX angle = Mat3 1 0 0 0 (cos angle) (-sin angle) 0 (sin angle) (cos angle)
rotationY :: Float -> Mat3
rotationY angle = Mat3 (cos angle) 0 (sin angle) 0 1 0 (-sin angle) 0 (cos angle)
rotationZ :: Float -> Mat3
rotationZ angle = Mat3 (cos angle) (-sin angle) 0 (sin angle) (cos angle) 0 0 0 1

-- Systems of equations

solve2Eqs :: Eqs -> Point -- Gives only the first solution it finds.
solve2Eqs = solve2Eq . getEqPair

solve2Eq :: (Eq2, Eq2) -> Point -- Solve a pair of equations with two variables
solve2Eq ((Eq2 eq00 eq01 val0), (Eq2 eq10 eq11 val1)) = inverse2x2 (Mat2 eq00 eq01 eq10 eq11) `transform2` (val0, val1)

getEqPair :: Eqs -> (Eq2, Eq2) -- Get a pair of equations with zero determinant from a list of equations
getEqPair eqs = head $ dropWhile zeroDeterminant $ pairs eqs
    where
        pairs (x:xs) = (map ((,) x) xs) ++ (pairs xs)
        zeroDeterminant ((Eq2 eq00 eq01 _), (Eq2 eq10 eq11 _)) = determinant (Mat2 eq00 eq01 eq10 eq11) == 0
