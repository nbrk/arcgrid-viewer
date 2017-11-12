module Main where

import System.Environment
import Graphics.Gloss
import qualified Data.List as L
import qualified Data.Vector as V
import Data.IORef
import System.IO.Unsafe

import ArcGrid

data ColorMode = Red | BW | Fancy

colorModeRef :: IORef ColorMode
colorModeRef = unsafePerformIO $ newIORef Red

bgColor :: Color
bgColor = white

squareWidth :: Float
squareWidth = 1

main :: IO ()
main = do
  args <- getArgs
  case args of
    mstr:fname:_ -> do
      let mode = case mstr of
                   "bw" -> BW
                   "fancy" -> Fancy
                   otherwise -> Red -- default
      writeIORef colorModeRef mode
      displayFile fname
    otherwise -> usage


usage :: IO ()
usage = do
  pname <- getProgName
  putStrLn $ "Usage: " ++ pname ++ " <red|bw|fancy> <file>" ++
    "\nBeware that big (i.e. bigger than 150x150 cells) files take time to process!"


displayFile :: String -> IO ()
displayFile f = do
  ag <- arcGridFromFile f
  putStrLn $ "Parser done. Forming " ++ (show $ ncols ag) ++ "x" ++ (show $ nrows ag) ++ " viewport..."
  display FullScreen bgColor (pictureFromArcGrid ag)


pictureFromArcGrid :: ArcGrid -> Picture
pictureFromArcGrid ag =
  let vat' = L.sort (vat ag)
      vmin = head vat'
      vmax = last vat'
      vatv = V.fromList $ vat ag
      width = ncols ag
      height = nrows ag
      rows = map
        (\h -> at 0 (negate (fromIntegral h)*squareWidth)
               (pictureValueVector (vmin, vmax) (getNthSubvector width h vatv))
        )
        [0..(height - 1)]
  in
    Pictures rows


getNthSubvector :: Int -> Int -> V.Vector a -> V.Vector a
getNthSubvector width n vec = V.slice (n * width) (width - 1) vec


determineValueColor :: (Int, Int) -> Int -> Color
determineValueColor (vmin, vmax) v =
  let d = fromIntegral $ vmax - vmin
      prg = fromIntegral $ v - vmin
      rat = prg / d
      zone = determineThermalZone rat
      mode = unsafePerformIO $ readIORef colorModeRef
  in
    case mode of
      Red -> makeColor rat 0 0 1
      BW -> greyN rat
      Fancy -> determineThermalZoneColor rat zone


determineThermalZone :: Float -> Int
determineThermalZone rat = determineThermalZone' $ rat * 5


determineThermalZone' :: Float -> Int
determineThermalZone' rat | 0 <= rat && rat < 1 = 1
determineThermalZone' rat | 1 <= rat && rat < 2 = 2
determineThermalZone' rat | 2 <= rat && rat < 3 = 3
determineThermalZone' rat | 3 <= rat && rat < 4 = 4
determineThermalZone' rat | 4 <= rat && rat <= 5 = 5


determineThermalZoneColor :: Float -> Int -> Color
determineThermalZoneColor rat zone =
  let i = (rat * 5) / (fromIntegral zone)
  in
    case zone of
      1 -> makeColor 0 0 i 1
      2 -> makeColor 0 i 1 1
      3 -> makeColor 0 1 (1 - i) 1
      4 -> makeColor i 1 0 1
      5 -> makeColor 1 (1 - i) 0 1

pictureValue :: Int -> Picture
pictureValue v =
  polygon (rectanglePath squareWidth squareWidth)


pictureValueVector :: (Int, Int) -> V.Vector Int -> Picture
pictureValueVector minmax vec =
  let mkpic v = color (determineValueColor minmax v) $ pictureValue v
      picsv = V.imap (\i v ->
                        at ((fromIntegral i) * squareWidth) 0 (mkpic v))
              vec
  in
    Pictures $ V.toList picsv


at = translate
