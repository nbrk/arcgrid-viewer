module GraphicsContextGL where

import Graphics.UI.GLUT
import Data.IORef

import Types
import GLCallbacks


gcglInitialize :: IO ()
gcglInitialize = do
  getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered] -- XXX
  createWindow "Hello World"

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
