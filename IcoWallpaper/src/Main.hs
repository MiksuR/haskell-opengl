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
  let (winX, winY) = (640, 480)
  Just win <- GLFW.createWindow winX winY "IcoWallpaper" Nothing Nothing
  -- Just win <- GLFW.createWindow 1 1 "IcoWallpaper" GLFW.getPrimaryMonitor Nothing
  GLFW.makeContextCurrent (Just win)
  GLFW.setWindowCloseCallback win (Just shutdown)
  GL.clearColor 0 0 0 0
  -- (winX, winY) <- GLFW.getWindowSize win
  forever $ do
    GLFW.pollEvents
    drawScene win
    GLFW.swapBuffers win
        -- animate FullScreen black ((color white) . (frameAt screenSize))
        -- animate (InWindow "Ico" (200, 200) (10, 10)) black ((color white) . (frameAt screenSize))

drawScene :: GLFW.Window -> IO ()
drawScene _ = do
  GL.clear $ [ColorBuffer]
  GL.loadIdentity

  GL.beginTransformFeedback GL.Lines
  GL.vertex 0 1
  GL.vertex 1 0
  GL.endTransformFeedback
  GL.flush

shutdown :: GLFW.WindowCloseCallback
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return ()

  {--
frameAt :: (Int, Int) -> Float -> Picture
frameAt size time = renderScene camera size (scene time)
    where renderScene = renderNormal -- Change this to one of {renderNormal, renderBlurred, renderBlurredDistort}
--}

scene :: Float -> Scene
scene time = [rotateShape 0 0 time uprightIco]

camera :: Camera
camera = Camera (Vector3 5 0 0) (Vector3 (-1) 0 0) 0.5 0.25
