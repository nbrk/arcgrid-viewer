{-# LANGUAGE TemplateHaskell #-}
module Types where

import Control.Lens
import Data.IORef

-- | Colour modes (gradients)
data ColorScheme = RedScheme | BWScheme | ThermalScheme

-- | Supported data sources
data DataSourceType = DataSourceArcGrid

-- | User-given options
data Options = Options
               { _optColorScheme  :: ColorScheme
               , _optDataFile :: FilePath
               }

-- | The data matrix that is rendered by the viewer
data Data = Data
            { _dataValues :: [Int]
            , _dataIgnoredValue :: Maybe Int
            , _dataDimensions :: (Int, Int)
            , _dataRange :: (Int, Int)
            , _dataSourceType :: DataSourceType
            }


-- | The current graphics context
data GraphicsContext = GraphicsContext
  { _gcPoints :: [(Float, Float, Float)]
  , _gcPointsYAmplificationRange :: (Int, Int)
  , _gcPointsYAmplification :: Float
  , _gcRGBAs :: [(Float, Float, Float, Float)]

  , _gcTranslation :: (Float, Float, Float)
  , _gcRotation :: (Float, Float, Float)
  , _gcScaling :: (Float, Float, Float)
  , _gcEyesPosition :: (Float, Float, Float)
  , _gcEyesSeeDirection :: (Float, Float, Float)
  , _gcEyesTopDirection :: (Float, Float, Float)
  }

-- | Abstract viewer data type
data Viewer = Viewer
              { _viewOptions :: Options
              , _viewData :: Data
              , _viewGCRef :: IORef GraphicsContext
              }


makeLenses ''Options
makeLenses ''Data
makeLenses ''GraphicsContext
makeLenses ''Viewer
