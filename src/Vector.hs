module Vector where

import Graphics.Gloss
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Data.Monoid

import Types
import ColorScheme

makeVectorPicture :: ReaderT ViewerCtx IO Picture
makeVectorPicture = do
  vs <- asks vctxValTblData
  ps <- mapM pictureFromRowColValue vs
  return $ pictures ps


pictureFromRowColValue :: ((Int, Int), Int) -> ReaderT ViewerCtx IO Picture
pictureFromRowColValue ((r, c), v) = do
  sq <- asks vctxSqSize
  vcolor <- colorFromValue v
  let mon = mconcat
        [ Endo $ translate (fromIntegral c * sq) (negate (fromIntegral r * sq))
        , Endo $ color vcolor
        ]
--  lift $ putStrLn $ "((r,c,),val)=" ++ show ((r,c), v) ++ " vcolor=" ++ show vcolor ++ " "
  return $ appEndo mon $ rectangleSolid sq sq
