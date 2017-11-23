module GLCallbacks where

import Graphics.UI.GLUT
import Control.Lens
import Control.Monad
import Data.IORef

import Types
import GLHelpers
import ColorScheme(intensivity)


gcglReshapeCallback :: ReshapeCallback
gcglReshapeCallback size = do
  let (Size w h) = size
  viewport $= (Position 0 0, size)
  postRedisplay Nothing



gcglDisplayCallback :: IORef GraphicsContext -> DisplayCallback
gcglDisplayCallback gcr = do
  gc <- get gcr
  let prs = zip (gc ^. gcPoints) (gc ^. gcRGBAs)

  clearColor $= Color4 1 1 1 1
  clear [ ColorBuffer ]
  loadIdentity

  -- user-set pan
  translate $ Vector3 (gc ^. gcPosition._1) (gc ^. gcPosition._2) 0

  preservingMatrix $ do
    rotate (gc ^. gcAngleX) (Vector3 1 0 0)
    rotate (gc ^. gcAngleY) (Vector3 0 1 0)
    rotate (gc ^. gcAngleZ) (Vector3 0 0 1)
    scale (gc ^. gcZoomFactorXY) (gc ^. gcZoomFactorXY) (gc ^. gcZoomFactorZ)
    forM_ prs $ \((x, y, z), r) ->
      preservingMatrix $ do
        color4f r

        translate $ Vector3 x y 0
        renderCuboid' 1 $ intensivity (gc ^. gcPointsZAmplification) (truncate z)
--        renderPrimitive Points $ vertex3f (x,y,z)

  swapBuffers
--  flush


gcglKeyboardMouseCallback :: IORef GraphicsContext -> KeyboardMouseCallback
gcglKeyboardMouseCallback gcr key Down _mods _pos =
  do
    case key of
      (Char 'X') -> gcr $~! (gcAngleX +~ 15)
      (Char 'x') -> gcr $~! (gcAngleX -~ 15)
      (Char 'Y') -> gcr $~! (gcAngleY +~ 15)
      (Char 'y') -> gcr $~! (gcAngleY -~ 15)
      (Char 'Z') -> gcr $~! (gcAngleZ +~ 15)
      (Char 'z') -> gcr $~! (gcAngleZ -~ 15)
      (Char '+') -> gcr $~! (gcZoomFactorZ *~ 2)
      (Char '-') -> gcr $~! (gcZoomFactorZ //~ 2)
      (SpecialKey KeyLeft ) -> gcr $~! (gcPosition %~ \(x,y) -> (x-0.1, y))
      (SpecialKey KeyRight) -> gcr $~! (gcPosition %~ \(x,y) -> (x+0.1, y))
      (SpecialKey KeyUp   ) -> gcr $~! (gcPosition %~ \(x,y) -> (x, y+0.1))
      (SpecialKey KeyDown ) -> gcr $~! (gcPosition %~ \(x,y) -> (x, y-0.1))
      (SpecialKey KeyPageUp) -> gcr $~! (gcZoomFactorXY *~ 2) -- 0.5
      (SpecialKey KeyPageDown) -> gcr $~! (gcZoomFactorXY //~ 2)
      otherwise -> return ()
    postRedisplay Nothing

gcglKeyboardMouseCallback _ _ _ _ _ = return ()


-- gcglIdleCallback :: IORef GraphicsContext -> IdleCallback
-- gcglIdleCallback gcr = do
-- --  gcr $~! (gcAngle %~ (+1))
--   gc <- get gcr
--   gcr $~! (gcAngle +~ (gc ^. gcRotationSpeed))
--   postRedisplay Nothing

