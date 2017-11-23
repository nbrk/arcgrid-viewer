module DataArcGrid where

import Data.List
import Data.Maybe
import ArcGrid
import Text.Printf

import Types

dataFromArcGridFile :: FilePath -> IO Data
dataFromArcGridFile f = do
  ag <- arcGridFromFile f

  let rng = rangeOfVAT (nodata_value ag) (vat ag)

  printf "Parsed %dx%d ArcGrid VAT (cell size %f). Value range is %s, ignoring %s\n"
    (ncols ag) (nrows ag) (cellsize ag) (show rng) (show (nodata_value ag))

  return $ Data
    { _dataValues = vat ag
    , _dataIgnoredValue = nodata_value ag
    , _dataDimensions = (ncols ag, nrows ag)
    , _dataRange = rng
    , _dataSourceType = DataSourceArcGrid
    }


rangeOfVAT :: Maybe Int -> [Int] -> (Int, Int)
rangeOfVAT nd vat =
  let vat' = if isNothing nd
             then vat
             else filter (\v -> Just v /= nd) vat
      vat'' = sort vat'
  in
    (head vat'', last vat'')
