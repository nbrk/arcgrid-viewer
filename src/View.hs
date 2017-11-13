module View where

import Graphics.Gloss
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import qualified Data.List as L
import ArcGrid

import Types
import ColorScheme
import Vector
import Raster

viewArcGridFile :: ReaderT Options IO ()
viewArcGridFile = do
  opts <- ask
  ag <- lift $ arcGridFromFile (optInput opts)
  lift $ putStrLn $
    "Parser done. " ++
    "Forming " ++ show (ncols ag) ++ "x" ++ show (nrows ag) ++ " viewport... "

  let rcs = [(r, c) | r <- [0..(nrows ag) - 1], c <- [0..(ncols ag) - 1]]
  let vs' = L.sort $ vat ag
  let vctx = ViewerCtx
             { vctxRenderMode = optRenderMode opts
             , vctxColorScheme = optColorScheme opts
             , vctxBGColor = optBGColor opts
             , vctxSqSize = optSqSize opts
             , vctxValTblSize = (ncols ag, nrows ag)
             , vctxValTblData = zip rcs (vat ag)
             , vctxMinMaxVal = (head vs', last vs')
             , vctxNodataVal = nodata_value ag
             }
--  lift $ putStrLn $ show $ vctxValTblData vctx

  lift $ runReaderT viewContext vctx


makePicture :: ReaderT ViewerCtx IO Picture
makePicture = do
  r <- asks vctxRenderMode
  case r of
    VectorMode -> makeVectorPicture
    RasterMode -> makeRasterPicture


viewContext :: ReaderT ViewerCtx IO ()
viewContext = do
  bg <- asks vctxBGColor
  pic <- makePicture
  lift $ display FullScreen bg pic





