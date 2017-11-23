module GraphicsContext where

import GraphicsContextGL
import Control.Lens
import Data.IORef

import Types
import ColorScheme


gcInitialize :: Options -> Data -> IO (IORef GraphicsContext)
gcInitialize opts d = do
  let scm = opts ^. optColorScheme
  let vals = d ^. dataValues
  let valrng = d ^. dataRange
  let nd = d ^. dataIgnoredValue

  gcglInitialize

  let initialgc = GraphicsContext
           { _gcPoints = gcPointsFromData  d
           , _gcPointsYAmplificationRange = valrng
           , _gcPointsYAmplification = 1
           , _gcRGBAs = map (rgbaFromValue nd valrng scm) vals

           , _gcTranslation = (0,0,0)
           , _gcRotation = (0,0,0)
           , _gcScaling = (1,1,1)

           , _gcEyesPosition = (0, 100, 0) -- 100 above maxval
           , _gcEyesSeeDirection = (0, 0, 0)
           , _gcEyesTopDirection = (0, 0, 1)
           }
  newIORef initialgc


gcLoop :: IORef GraphicsContext -> IO ()
gcLoop = gcglLoop


-- | Construct points in 3-D space. Right Hand Rule, so elev is Y
gcPointsFromData :: Data -> [(Float, Float, Float)]
gcPointsFromData d =
  let
    (nc, nr) = d ^. dataDimensions
    xzs = [(x, negate z) | z <- [0..(nr-1)], x <- [0..(nc-1)] ]
  in
    zipWith
    (\(x, z) val -> (fromIntegral x, fromIntegral val, fromIntegral z))
    xzs
    (d ^. dataValues)
