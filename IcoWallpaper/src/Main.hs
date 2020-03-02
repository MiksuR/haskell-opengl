module Main where

import Graphics.Rendering.OpenGL as GL hiding (Vector3)
import Graphics.Rendering.OpenGL.Capture
import Graphics.UI.GLFW as GLFW

import Control.Exception (finally)
import Control.Monad.Trans.Except
import Data.ByteString (writeFile)
import GHC.Float (double2Float)
import System.Exit (exitSuccess)
import System.Random (newStdGen)
import Text.Printf

import LinearEqs
import Shapes
import Camera

main :: IO ()
main = do
  True <- GLFW.init
  GLFW.defaultWindowHints
  Just monitor <- GLFW.getPrimaryMonitor
  (winX, winY) <- do
    Just mode <- GLFW.getVideoMode monitor
    return (GLFW.videoModeWidth mode, GLFW.videoModeHeight mode)
  Just win <- GLFW.createWindow winX winY "IcoWallpaper" (Just monitor) Nothing
  GLFW.makeContextCurrent (Just win)
  GLFW.setWindowCloseCallback win (Just shutdown)
  GL.blend $= Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.shadeModel $= Flat
  GL.clearColor $= Color4 0 0 0 0
  -- finally (drawScene win (winX, winY) 0) (shutdown win)
  mapM_ (drawScene win (winX, winY)) [0..120]
  shutdown win

scene :: Int -> Scene
scene frame = [rotateIco uprightIco]
  where
    rotateIco = rotateShape (-(atan $ 1/phi)) 0 0 . rotateShape 0 0 (fromIntegral frame * (pi/150))

safeCamera :: ExceptT String IO Camera
safeCamera = except $ (\dir -> Camera (Vector3 3 (-1.8) (1.6)) dir 0.95) <$> vNormalize (Vector3 (-1) 0 (-0.2))

unsafeCamera :: Camera
unsafeCamera = Camera (Vector3 3 (0) (1.6)) (norm $ Vector3 (-1) (-0.5) (-0.2)) 0.95
  where
    norm (Vector3 a b c) = Vector3 (a/sqrt (a*a + b*b + c*c)) (b/sqrt (a*a + b*b + c*c)) (c/sqrt (a*a + b*b + c*c))

drawScene :: GLFW.Window -> (Int, Int) -> Int -> IO ()
drawScene w size frame = do
  GLFW.pollEvents
  Just time <- GLFW.getTime
  GL.clear [ColorBuffer]
  GL.loadIdentity

  exceptCam <- runExceptT safeCamera
  rand <- newStdGen
  case exceptCam of
    Left err -> putStrLn err
    Right cam -> renderBlurred rand (Blur 3.2 0.006 4.8) 24000 0.01 size cam (scene frame)

  let fileName = printf "capture/frame%04d.ppm" frame
  capturePPM >>= Data.ByteString.writeFile fileName

  GL.get GL.errors >>= mapM_ print
  GL.flush
  GLFW.swapBuffers w
  -- case exceptCam of
  --   Left _ -> return ()
  --   Right _ -> drawScene w size (frame+1)

shutdown :: GLFW.WindowCloseCallback
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  _ <- exitSuccess
  return ()

