module Main where

import System.Environment (getArgs)

import Types
import Options
import Data
import GraphicsContext
import Viewer


main :: IO ()
main = do
  argv <- getArgs

  (opts, argv') <- programOpts argv
  case argv' of
    fname:_ -> do
      -- prepare the data
      d <- dataFromFile DataSourceArcGrid fname

      -- init the gc of the data
      gc <- gcInitialize opts d

      -- start the viewer
      viewerStart opts d gc

    otherwise -> do
      putStrLn "Error: filename required"
      putStrLn ""
      errorUsage options
