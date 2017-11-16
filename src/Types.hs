module Types where

import Graphics.Gloss (Color)

data ColorScheme = RedScheme | BWScheme | FancyScheme

data Options = Options
               { optColorScheme  :: ColorScheme
               , optBGColor :: Color
               , optSqSize :: Float
               , optInput :: FilePath
               }


data ViewerCtx = ViewerCtx
                 { vctxColorScheme :: ColorScheme
                 , vctxBGColor :: Color
                 , vctxSqSize :: Float
                 , vctxValTblSize :: (Int, Int)
                 , vctxValTblData :: [((Int, Int), Int)]
                 , vctxMinMaxVal :: (Int, Int)
                 , vctxNodataVal :: Maybe Int
                 }
