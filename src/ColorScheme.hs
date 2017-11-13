module ColorScheme where

import Graphics.Gloss
import Control.Monad.Trans.Reader

import Types

colorFromValue :: Int -> ReaderT ViewerCtx IO Color
colorFromValue v = do
  (vmin, vmax) <- asks vctxMinMaxVal
  c <- asks vctxColorScheme
  -- X between A and B; we want Y to fall between C and D
  -- Y = (X-A)/(B-A) * (D-C) + C
  let d = fromIntegral $ abs $ vmax - vmin
  let dv = fromIntegral $ abs $ v - vmin
  let int = dv / d * (1.0 - 0) + 0
  case c of
    RedScheme -> return $ makeColor int 0 0 1
    BWScheme -> return $ greyN int
    FancyScheme -> fancyColor int


fancyColor :: Float -> ReaderT ViewerCtx IO Color
fancyColor int = do
  let tz = thermalZone int
  let c = case tz of
        1 -> makeColor 0 0 int 1
        2 -> makeColor 0 int 1 1
        3 -> makeColor 0 1 (1 - int) 1
        4 -> makeColor int 1 0 1
        _ -> makeColor 1 (1 - int) 0 1
  -- let c = case tz of
  --       1 -> makeColor 0 int 1 1
  --       2 -> makeColor 0 1 (1 - int) 1
  --       3 -> makeColor int 1 0 1
  --       _ -> makeColor 1 (1 - int) 0 1

  return c
  where
    thermalZone i = truncate $ i / 0.2 + 1 -- 1 / 0.2, so 5 thermal zones
--    thermalZone i = truncate $ i / 0.25 + 1
