module Main where

import Graphics.Gloss (white)
import System.Console.GetOpt
import System.Environment
import Data.Maybe ( fromMaybe )
import Control.Monad.Trans.Reader

import Types
import View

defaultOptions = Options
               { optColorScheme = RedScheme
               , optRenderMode = VectorMode
               , optBGColor = white
               , optSqSize = 1
               , optInput = ""
               }

parseColorScheme :: String -> ColorScheme
parseColorScheme "red" = RedScheme
parseColorScheme "bw" = BWScheme
parseColorScheme "fancy" = FancyScheme
parseColorScheme _ = error "Incorrect color scheme"

parseRenderMode :: String -> RenderMode
parseRenderMode "raster" = RasterMode
parseRenderMode "vector" = VectorMode
parseRenderMode _ = error "Incorrect rendering mode"


options :: [OptDescr (Options -> Options)]
options =
  [ Option ['c'] ["color"]
    (ReqArg (\a opts -> opts {optColorScheme = parseColorScheme a}) "MODE") "color scheme: red|bw|fancy"
  ,
    Option ['r'] ["mode"]
    (ReqArg (\a opts -> opts {optRenderMode = parseRenderMode a}) "MODE") "rendering mode: vector|raster"
  ]


programOpts :: [String] -> IO (Options, [String])
programOpts argv = do
  pn <- getProgName
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
    (_,_,errs) -> do
      putStrLn $ "Error: " ++ concat errs
      errorUsage options


errorUsage :: [OptDescr a] -> IO b
errorUsage opts = do
  pn <- getProgName
  putStrLn $ usageInfo (usageHeader pn) options
  ioError $ userError ""
  where
    usageHeader pn = "Usage: " ++ pn ++ " [OPTION...] <file>"


main :: IO ()
main = do
  argv <- getArgs

  (opts, argv') <- programOpts argv
  case argv' of
    fname:_ -> runReaderT viewArcGridFile $ opts {optInput = fname}
    otherwise -> do
      putStrLn "Error: filename required"
      putStrLn ""
      errorUsage options
