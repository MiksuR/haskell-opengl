module Main where

import Graphics.Rendering.OpenGL as GL hiding (Vector3)
import Graphics.UI.GLFW as GLFW

import Control.Exception (finally)
import Control.Monad.Trans.Except
import GHC.Float (double2Float)
import System.Exit (exitSuccess)
import System.Random (newStdGen)

import LinearEqs
import Shapes
import Camera

main :: IO ()
main = do
  True <- GLFW.init
  GLFW.defaultWindowHints
  (winX, winY) <- do
    Just monitor <- GLFW.getPrimaryMonitor
    Just mode <- GLFW.getVideoMode monitor
    return (GLFW.videoModeWidth mode, GLFW.videoModeHeight mode)
  print (winX, winY)
  Just win <- GLFW.createWindow winX winY "IcoWallpaper" Nothing Nothing
  -- Just win <- GLFW.createWindow 1 1 "IcoWallpaper" GLFW.getPrimaryMonitor Nothing
  GLFW.makeContextCurrent (Just win)
  GLFW.setWindowCloseCallback win (Just shutdown)
  GL.blend $= Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.shadeModel $= Flat
  GL.clearColor $= Color4 0 0 0 0
  finally (drawScene win (winX, winY)) (shutdown win)

scene :: Float -> Scene
scene time = [rotateIco uprightIco]
  where
    rotateIco = rotateShape (-(atan $ 1/phi)) 0 0 . rotateShape 0 0 (time/2)

safeCamera :: ExceptT String IO Camera
safeCamera = except $ (\dir -> Camera (Vector3 (1) 0 0) dir 0.95) <$> vNormalize (Vector3 (-1) 0 0)
-- safeCamera = except $ (\dir -> Camera (Vector3 3 (-1.8) (1.6)) dir 0.95) <$> vNormalize (Vector3 (-1) 0 (-0.2))

unsafeCamera :: Camera
unsafeCamera = Camera (Vector3 3 (0) (1.6)) (norm $ Vector3 (-1) (-0.5) (-0.2)) 0.95
  where
    norm (Vector3 a b c) = Vector3 (a/sqrt (a*a + b*b + c*c)) (b/sqrt (a*a + b*b + c*c)) (c/sqrt (a*a + b*b + c*c))

drawScene :: GLFW.Window -> (Int, Int) -> IO ()
drawScene w size= do
  GLFW.pollEvents
  Just time <- GLFW.getTime
  GL.clear [ColorBuffer]
  GL.loadIdentity

  exceptCam <- runExceptT safeCamera
  -- rand <- newStdGen
  case exceptCam of
    Left err -> putStrLn err
    -- Right cam -> renderBlurred rand (Blur 3.5 0.2 1.3) 1000 0.15 size cam (scene . double2Float $ time)
    Right cam -> renderNormal size cam (scene . double2Float $ time)

  GL.get GL.errors >>= mapM_ print
  GL.flush
  GLFW.swapBuffers w
  case exceptCam of
    Left _ -> return ()
    Right _ -> drawScene w size

shutdown :: GLFW.WindowCloseCallback
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  _ <- exitSuccess
  return ()

