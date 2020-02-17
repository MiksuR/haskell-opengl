module Main where

import Graphics.Rendering.OpenGL as GL hiding (Vector3)
import Graphics.UI.GLFW as GLFW

import Control.Exception (finally)
import GHC.Float (double2Float)
import System.Exit (exitWith, ExitCode(..))

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
  GL.clearColor $= Color4 0 0 0 0
  finally (drawScene win (winX, winY) camera scene) (shutdown win)

scene :: Float -> Scene
scene time = [rotateShape 0 0 time uprightIco]

camera :: Camera
camera = Camera (Vector3 5 0 0) (Vector3 (-1) 0 0) 0.5 0.45

drawScene :: GLFW.Window -> (Int, Int) -> Camera -> (Float -> Scene) -> IO ()
drawScene w size cam sc = do
  GLFW.pollEvents
  Just time <- GLFW.getTime
  GL.clear $ [ColorBuffer]
  GL.loadIdentity

  --renderNormal size cam (sc . double2Float $ time)
  renderBlurred (Blur 5 1.2 1) 10 1 size cam (sc . double2Float $ time)

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

