module GraphicsContextGL where

import Graphics.UI.GLUT
import Data.IORef

import Types
import GLCallbacks
import GLKeyboardMouseCallback


gcglInitialize :: IO ()
gcglInitialize = do
  initialWindowSize $= Size 1024 768
  initialize "arcgrid-viewer" []

  initialDisplayMode $= [DoubleBuffered, WithDepthBuffer] -- XXX
  createWindow "ArcGrid Viewer"
  fullScreen

  return ()


-- | Setup the callbacks with the gc and loop-off
gcglLoop :: IORef GraphicsContext -> IO ()
gcglLoop gcr = do
  displayCallback $= gcglDisplayCallback gcr
  reshapeCallback $= Just gcglReshapeCallback
  keyboardMouseCallback $= Just (gcglKeyboardMouseCallback gcr)
--  idleCallback $= Just (gcglIdleCallback gcr)

  -- GLUT main loop
  mainLoop
