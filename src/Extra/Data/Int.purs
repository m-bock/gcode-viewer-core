module Extra.Data.Int
  ( module Export
  , tsExports
  ) where

import Data.Int
import Data.Int as Export
import DTS as DTS
import Data.Either (Either)
import GCodeViewer.TsBridge (Tok(..))
import TsBridge as TSB

moduleName :: String
moduleName = "Data.Int"

tsExports :: Either TSB.AppError (Array DTS.TsModuleFile)
tsExports = TSB.tsModuleFile moduleName
  [ TSB.tsValues Tok
      { toNumber
      , fromNumber
      , round
      , floor
      , ceil
      , trunc
      }
  ]