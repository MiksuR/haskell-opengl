module Main where

import Graphics.Rendering.OpenGL as GL hiding (Vector3)
import Graphics.UI.GLFW as GLFW

import Control.Exception (finally)
import GHC.Float (double2Float)
import System.Exit (exitWith, ExitCode(..))
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
  putStrLn . show $ (winX, winY)
  Just win <- GLFW.createWindow winX winY "IcoWallpaper" Nothing Nothing
  -- Just win <- GLFW.createWindow 1 1 "IcoWallpaper" GLFW.getPrimaryMonitor Nothing
  GLFW.makeContextCurrent (Just win)
  GLFW.setWindowCloseCallback win (Just shutdown)
  GL.blend $= Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.shadeModel $= Flat
  GL.clearColor $= Color4 0 0 0 0
  finally (drawScene win (winX, winY) camera scene) (shutdown win)

scene :: Float -> Scene
scene time = [rotateIco uprightIco]
  where
    rotateIco = (rotateShape (-(atan $ 1/phi)) 0 0) . (rotateShape 0 0 (time/2))

camera :: Camera
camera = Camera (Vector3 3 (-1.8) (1.6)) (Vector3 (-1) 0 (-0.2)) 0.5 0.45

drawScene :: GLFW.Window -> (Int, Int) -> Camera -> (Float -> Scene) -> IO ()
drawScene w size cam sc = do
  GLFW.pollEvents
  Just time <- GLFW.getTime
  GL.clear $ [ColorBuffer]
  GL.loadIdentity

  --renderNormal size cam (sc . double2Float $ time)
  --{--
  rand <- newStdGen --      f   m   e
  renderBlurred rand (Blur 3.0 0.1 1.8) 1000 0.2 size cam (sc . double2Float $ time)
  ----}

  GL.get GL.errors >>= mapM_ print
  GL.flush
  GLFW.swapBuffers w
  drawScene w size cam sc

shutdown :: GLFW.WindowCloseCallback
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return ()

