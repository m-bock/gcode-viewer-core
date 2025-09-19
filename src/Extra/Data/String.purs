module Extra.Data.String
  ( eqString
  , tsExports
  ) where

import Prelude

import DTS as DTS
import Data.Either (Either)
import GCodeViewer.TsBridge (Tok(..))
import TsBridge as TSB

moduleName :: String
moduleName = "Extra.Data.String"

eqString :: String -> String -> Boolean
eqString = eq

tsExports :: Either TSB.AppError (Array DTS.TsModuleFile)
tsExports = TSB.tsModuleFile moduleName
  [ TSB.tsValues Tok
      { eqString
      }
  ]