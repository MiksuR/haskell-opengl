module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Environment (getScreenSize)
import LinearEqs
import Shapes
import Camera

main :: IO ()
main = do
        screenSize <- getScreenSize
        animate FullScreen black ((color white) . (frameAt screenSize))

frameAt :: (Int, Int) -> Float -> Picture
frameAt size time = renderScene camera size (scene time)
    where renderScene = renderNormal -- Change this to one of {renderNormal, renderBlurred, renderBlurredDistort}

scene :: Float -> Scene
scene time = [rotateShape 0 0 time uprightIco]

camera :: Camera
camera = Camera (Vector3 5 0 0) (Vector3 (-1) 0 0) 0.5 0.25
