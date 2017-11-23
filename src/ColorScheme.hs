module ColorScheme where

import Types

type RGBA = (Float, Float, Float, Float)


constNodataColor :: RGBA
constNodataColor = (1, 0, 1, 1) -- magenta


rgbaFromValue :: Maybe Int -> (Int, Int) -> ColorScheme -> Int -> RGBA
rgbaFromValue nodata minmaxval scm val =
  if Just val == nodata
  then constNodataColor
  else
    let intens = intensivity minmaxval val
    in
      case scm of
        RedScheme -> (intens, 0, 0, 1)
        BWScheme -> (intens, intens, intens, 1) -- XXX
        ThermalScheme -> rgbaThermal intens


rgbaThermal :: Float -> RGBA
rgbaThermal intens =
  let tz = thermalZone intens
  in
    case tz of
      1 -> (0, 0, intens, 1)
      2 -> (0, intens, 1, 1)
      3 -> (0, 1, (1 - intens), 1)
      4 -> (intens, 1, 0, 1)
      _ -> (1, (1 - intens), 0, 1)


thermalZone :: Float -> Int
thermalZone i = truncate $ i / 0.2 + 1 -- 1 / 0.2, so 5 thermal zones


intensivity :: (Int, Int) -> Int -> Float
intensivity (minval, maxval) val =
  -- X between A and B; we want Y to fall between C and D
  -- Y = (X-A)/(B-A) * (D-C) + C
  let d = fromIntegral $ abs $ maxval - minval
      dv = fromIntegral $ abs $ val - minval
  in
    dv / d * (1.0 - 0) + 0

