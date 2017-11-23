module GLKeyboardMouseCallback where

import Graphics.UI.GLUT
import Control.Lens
import Data.IORef

import Types


-- | Endomorphisms
gcglKeyboardMouseCallback :: IORef GraphicsContext -> KeyboardMouseCallback
gcglKeyboardMouseCallback gcr key Down _mods _pos =
  do
    let tr = 1
    let rot = 15
    let scal = 0.5
    let ampl = 0.25
    let epos = 5
    let edir = 15

    case key of
      -- translation
      (Char 'q') -> gcr $~! (gcTranslation %~ \(x,y,z) -> (x+tr,y,z))
      (Char 'Q') -> gcr $~! (gcTranslation %~ \(x,y,z) -> (x-tr,y,z))
      (Char 'w') -> gcr $~! (gcTranslation %~ \(x,y,z) -> (x,y+tr,z))
      (Char 'W') -> gcr $~! (gcTranslation %~ \(x,y,z) -> (x,y-tr,z))
      (Char 'e') -> gcr $~! (gcTranslation %~ \(x,y,z) -> (x,y,z+tr))
      (Char 'E') -> gcr $~! (gcTranslation %~ \(x,y,z) -> (x,y,z-tr))

      -- rotation
      (Char 'a') -> gcr $~! (gcRotation %~ \(x,y,z) -> (x+rot,y,z))
      (Char 'A') -> gcr $~! (gcRotation %~ \(x,y,z) -> (x-rot,y,z))
      (Char 's') -> gcr $~! (gcRotation %~ \(x,y,z) -> (x,y+rot,z))
      (Char 'S') -> gcr $~! (gcRotation %~ \(x,y,z) -> (x,y-rot,z))
      (Char 'd') -> gcr $~! (gcRotation %~ \(x,y,z) -> (x,y,z+rot))
      (Char 'D') -> gcr $~! (gcRotation %~ \(x,y,z) -> (x,y,z-rot))

      -- scaling
      (Char 'z') -> gcr $~! (gcScaling %~ \(x,y,z) -> (x+scal,y,z))
      (Char 'Z') -> gcr $~! (gcScaling %~ \(x,y,z) -> (x-scal,y,z))
      (Char 'x') -> gcr $~! (gcScaling %~ \(x,y,z) -> (x,y+scal,z))
      (Char 'X') -> gcr $~! (gcScaling %~ \(x,y,z) -> (x,y-scal,z))
      (Char 'c') -> gcr $~! (gcScaling %~ \(x,y,z) -> (x,y,z+scal))
      (Char 'C') -> gcr $~! (gcScaling %~ \(x,y,z) -> (x,y,z-scal))
      (Char 'v') -> gcr $~! (gcScaling %~ \(x,y,z) -> (x+scal,y+scal,z+scal))
      (Char 'V') -> gcr $~! (gcScaling %~ \(x,y,z) -> (x-scal,y-scal,z-scal))

      -- value amplification
      (Char 't') -> gcr $~! (gcPointsYAmplification %~ \a -> a - ampl)
      (Char 'T') -> gcr $~! (gcPointsYAmplification %~ \a -> a + ampl)

      -- eyes position (also sync some eyes direction)
      (Char '-') -> gcr $~! (gcEyesPosition %~ \(x,y,z) -> (x,y+epos,z))
                    >> gcr $~! (gcEyesSeeDirection %~ \(x,y,z) -> (x,y+epos,z))
      (Char '+') -> gcr $~! (gcEyesPosition %~ \(x,y,z) -> (x,y-epos,z))
                    >> gcr $~! (gcEyesSeeDirection %~ \(x,y,z) -> (x,y-epos,z))
      (SpecialKey KeyLeft ) -> gcr $~! (gcEyesPosition %~ \(x,y,z) -> (x-epos,y,z))
                               >> gcr $~! (gcEyesSeeDirection %~ \(x,y,z) -> (x-epos,y,z))
      (SpecialKey KeyRight) -> gcr $~! (gcEyesPosition %~ \(x,y,z) -> (x+epos,y,z))
                               >> gcr $~! (gcEyesSeeDirection %~ \(x,y,z) -> (x+epos,y,z))
      (SpecialKey KeyUp   ) -> gcr $~! (gcEyesPosition %~ \(x,y,z) -> (x,y,z-epos))
                               >> gcr $~! (gcEyesSeeDirection %~ \(x,y,z) -> (x,y,z-epos))
      (SpecialKey KeyDown ) -> gcr $~! (gcEyesPosition %~ \(x,y,z) -> (x,y,z+epos))
                               >> gcr $~! (gcEyesSeeDirection %~ \(x,y,z) -> (x,y,z+epos))

      -- eyes direction, z-axis (tilt)
      (SpecialKey KeyPageUp   ) -> gcr $~! (gcEyesSeeDirection %~ \(x,y,z) -> (x,y,z-edir))
                                   >> gcr $~! (gcEyesTopDirection %~ \(x,y,z) -> (x,y,z+edir))
      (SpecialKey KeyPageDown ) -> gcr $~! (gcEyesSeeDirection %~ \(x,y,z) -> (x,y,z+edir))
                                   >> gcr $~! (gcEyesTopDirection %~ \(x,y,z) -> (x,y,z-edir))

      -- eyes direction, x-axis
      (Char ',') -> gcr $~! (gcEyesSeeDirection %~ \(x,y,z) -> (x-edir,y,z))
                                   >> gcr $~! (gcEyesTopDirection %~ \(x,y,z) -> (x+edir,y,z))
      (Char '.') -> gcr $~! (gcEyesSeeDirection %~ \(x,y,z) -> (x+edir,y,z))
                                   >> gcr $~! (gcEyesTopDirection %~ \(x,y,z) -> (x-edir,y,z))
      otherwise -> return ()

    -- request window redisplay on each key press
    postRedisplay Nothing

gcglKeyboardMouseCallback _ _ _ _ _ = return ()
