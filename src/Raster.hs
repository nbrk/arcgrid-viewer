module Raster where

import Graphics.Gloss
import Control.Monad.Trans.Reader
import qualified Data.ByteString as B
import Data.Word

import Types
import ColorScheme


packRGBA :: (Float, Float, Float, Float) -> [Word8]
packRGBA (r, g, b, a) = map (fromIntegral . round) [r * 255, g * 255, b * 255, a * 255]


makeRasterPicture :: ReaderT ViewerCtx IO Picture
makeRasterPicture = do
   vs <- asks vctxValTblData
   (w, h) <- asks vctxValTblSize
   colors <- mapM (\(_, v) -> colorFromValue v) vs
   let rgbas = concatMap (packRGBA . rgbaOfColor) colors
   let bs = B.pack rgbas

   return $ bitmapOfByteString w h (BitmapFormat TopToBottom PxRGBA) bs False
