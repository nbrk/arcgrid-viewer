module Data where

import Types
import DataArcGrid


-- | Read matrix data from the file of the given type
dataFromFile :: DataSourceType -> FilePath -> IO Data
dataFromFile DataSourceArcGrid = dataFromArcGridFile
dataFromFile _ = error "Unsupported DataSourceType"
