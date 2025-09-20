module Main where

import Prelude

import Data.Either (Either)
import Effect (Effect)
import TsBridge as TSB
import DTS as DTS
import StateMachines.App as StateMachines.App
import StateMachines.Viewer as StateMachines.Viewer
import RemoteData as RemoteData
import Extra.Data.Int as Extra.Data.Int
import Extra.Data.String as Extra.Data.String

myTsProgram :: Either TSB.AppError DTS.TsProgram
myTsProgram =
  TSB.tsProgram
    [ StateMachines.App.tsExports
    , StateMachines.Viewer.tsExports
    , RemoteData.tsExports
    , Extra.Data.Int.tsExports
    , Extra.Data.String.tsExports
    ]

main :: Effect Unit
main = TSB.mkTypeGenCli myTsProgram