module Main where

import Graphics.Rendering.OpenGL as GL hiding (Vector3)
import Graphics.UI.GLFW as GLFW
import System.Exit (exitWith, ExitCode(..))
import Control.Monad (forever)
import LinearEqs
import Shapes
import Camera

main :: IO ()
main = do
  True <- GLFW.init
  GLFW.defaultWindowHints
  --let (winX, winY) = (640, 480)
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
  -- (winX, winY) <- GLFW.getWindowSize win
  forever $ do
    GLFW.pollEvents
    Just time <- GLFW.getTime
    drawScene (winX, winY) camera $ scene 0 -- time
    GLFW.swapBuffers win
-- animate FullScreen black ((color white) . (frameAt screenSize))
-- animate (InWindow "Ico" (200, 200) (10, 10)) black ((color white) . (frameAt screenSize))

drawScene :: (Int, Int) -> Camera -> Scene -> IO ()
drawScene size camera scene = do
  GL.clear $ [ColorBuffer]
  GL.loadIdentity

  renderNormal size camera scene

shutdown :: GLFW.WindowCloseCallback
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return ()

scene :: Float -> Scene
scene time = [rotateShape 0 0 time uprightIco]

camera :: Camera
camera = Camera (Vector3 5 0 0) (Vector3 (-1) 0 0) 0.5 0.25
