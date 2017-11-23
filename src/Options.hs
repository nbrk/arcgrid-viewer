module Options where

import System.Console.GetOpt
import System.Environment

import Types


defaultOptions = Options
               { _optColorScheme = RedScheme
               , _optDataFile = ""
               }


parseColorScheme :: String -> ColorScheme
parseColorScheme "red" = RedScheme
parseColorScheme "bw" = BWScheme
parseColorScheme "thermal" = ThermalScheme
parseColorScheme _ = error "Incorrect color scheme"


options :: [OptDescr (Options -> Options)]
options =
  [ Option ['c'] ["color"]
    (ReqArg (\a opts -> opts {_optColorScheme = parseColorScheme a}) "MODE") "color scheme: red|bw|thermal"
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
