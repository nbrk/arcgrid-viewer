module Main where

import System.Environment
import Graphics.Gloss
import qualified Data.List as L
import qualified Data.Vector as V
import Data.Traversable

import ArcGrid

bgColor :: Color
bgColor = greyN 0.8

squareWidth :: Float
squareWidth = 1

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> usage
    fname:_ -> displayFile fname


usage :: IO ()
usage = do
  pname <- getProgName
  putStrLn $ "Usage: " ++ pname ++ " <file>" ++
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
  in
    makeColor rat 0 0 1


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
