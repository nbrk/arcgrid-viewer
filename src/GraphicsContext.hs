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
           , _gcPointsZAmplification = valrng
           , _gcRGBAs = map (rgbaFromValue nd valrng scm) vals
           , _gcAngleX = 0
           , _gcAngleY = 0
           , _gcAngleZ = 0
           , _gcPosition = (0, 0)
           , _gcZoomFactorXY = 0.01
           , _gcZoomFactorZ = 0.01
           }
  newIORef initialgc


gcLoop :: IORef GraphicsContext -> IO ()
gcLoop = gcglLoop


-- | Construct points in 3-D space
gcPointsFromData :: Data -> [(Float, Float, Float)]
gcPointsFromData d =
  let
    (nc, nr) = d ^. dataDimensions
    xys = [(x, negate y) | y <- [0..(nr-1)], x <- [0..(nc-1)] ]
  in
    zipWith
    (\(x, y) val -> (fromIntegral x, fromIntegral y, fromIntegral val))
    xys
    (d ^. dataValues)
