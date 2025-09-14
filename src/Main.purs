module Main where

import Prelude

import Data.Either (Either)
import Effect (Effect)
import TsBridge as TSB
import DTS as DTS
import GCodeViewer.StateMachines.App as GCodeViewer.StateMachines.App
import GCodeViewer.StateMachines.Viewer as GCodeViewer.StateMachines.Viewer
import GCodeViewer.RemoteData as GCodeViewer.RemoteData
import Extra.Data.Int as Extra.Data.Int

myTsProgram :: Either TSB.AppError DTS.TsProgram
myTsProgram =
  TSB.tsProgram
    [ GCodeViewer.StateMachines.App.tsExports
    , GCodeViewer.StateMachines.Viewer.tsExports
    , GCodeViewer.RemoteData.tsExports
    , Extra.Data.Int.tsExports
    ]

main :: Effect Unit
main = TSB.mkTypeGenCli myTsProgram