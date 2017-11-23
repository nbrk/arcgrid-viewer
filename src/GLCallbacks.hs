module GLCallbacks where

import Graphics.UI.GLUT
import Control.Lens
import Control.Monad
import Data.IORef

import Types
import GLKeyboardMouseCallback
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
  clear [ ColorBuffer, DepthBuffer ]
  loadIdentity

--  ortho (-100) 100 (-100) 100 (-100) 100 
  perspective (-90) (4/3) 0 10000

  lookAt
    (toVertex3d (gc ^. gcEyesPosition))
    (toVertex3d (gc ^. gcEyesSeeDirection))
    (toVector3d (gc ^. gcEyesTopDirection))

  -- -- the translation
  translate $
    Vector3 (gc ^. gcTranslation._1)  (gc ^. gcTranslation._2) (gc ^. gcTranslation._3)
  -- -- the rotation
  rotate (gc ^. gcRotation._1) (Vector3 1 0 0)
  rotate (gc ^. gcRotation._2) (Vector3 0 1 0)
  rotate (gc ^. gcRotation._3) (Vector3 0 0 1)

  -- -- the scaling
  scale (gc ^. gcScaling._1) (gc ^. gcScaling._2) (gc ^. gcScaling._3)

  -- draw axis
  renderPrimitive Lines $ do
    color $ Color4 0 0 0 (1 :: Float)
    vertex3f (0,0,0)
    vertex3f (10000,0,0)
    vertex3f (0,0,0)
    vertex3f (0,10000,0)
    vertex3f (0,0,0)
    vertex3f (0,0,10000)


  forM_ prs $ \((x, y, z), r) ->
    preservingMatrix $ do
      color4f r
      translate $ Vector3 x 0 z

      let ampl = gc ^. gcPointsYAmplification
      let (minval, maxval) = gc ^. gcPointsYAmplificationRange
      let intens = intensivity (minval, maxval) (truncate (y + ampl))
      renderCuboid' 1 (intens * ampl)
--      renderObject Solid (Cube 1)

  swapBuffers -- performs flush internally



-- gcglIdleCallback :: IORef GraphicsContext -> IdleCallback
-- gcglIdleCallback gcr = do
-- --  gcr $~! (gcAngle %~ (+1))
--   gc <- get gcr
--   gcr $~! (gcAngle +~ (gc ^. gcRotationSpeed))
--   postRedisplay Nothing


