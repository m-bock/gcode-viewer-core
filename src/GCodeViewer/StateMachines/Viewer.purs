module GCodeViewer.StateMachines.Viewer
  ( Dispatchers
  , Msg
  , PubState
  , tsApi
  , tsExports
  , useViewer
  ) where

import GCodeViewer.Prelude

import Control.Monad.Error.Class (catchError)
import DTS as DTS
import Data.Argonaut.Core (Json)
import Data.Codec (encode)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Lens (set)
import Data.Newtype (class Newtype)
import Data.String as Str
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import GCodeViewer.Api as Api
import GCodeViewer.Error (Err, mkErr, printErr)
import GCodeViewer.Error as Err
import Stadium.Core (DispatcherApi, TsApi, mkTsApi)
import Stadium.React (useStateMachine)
import GCodeViewer.RemoteData (RemoteData(..), codecRemoteData)
import GCodeViewer.TsBridge (class TsBridge, Tok(..))
import TsBridge as TSB

type PubState =
  { gcodeLines :: RemoteData (Array String)
  , startLayer :: Int
  , endLayer :: Int
  }

initPubState :: PubState
initPubState =
  { gcodeLines: NotAsked
  , startLayer: 0
  , endLayer: 0
  }

data Msg
  = MsgSetStartLayer Int
  | MsgSetEndLayer Int
  | MsgSetGcodeLines (RemoteData (Array String))

updatePubState :: Msg -> PubState -> Except String PubState
updatePubState msg pubState = case msg of
  MsgSetStartLayer startLayer -> pubState
    # set (prop @"startLayer") startLayer
    # pure

  MsgSetEndLayer endLayer -> pubState
    # set (prop @"endLayer") endLayer
    # pure

  MsgSetGcodeLines value -> pubState
    # set (prop @"gcodeLines") value
    # pure

encodeMsg :: Msg -> { tag :: String, args :: Json }
encodeMsg = case _ of
  MsgSetStartLayer r ->
    { tag: "MsgSetStartLayer"
    , args: CA.encode CA.int r
    }
  MsgSetEndLayer r ->
    { tag: "MsgSetEndLayer"
    , args: CA.encode CA.int r
    }
  MsgSetGcodeLines r ->
    { tag: "MsgSetGcodeLines"
    , args: CA.encode (codecRemoteData (CA.array CA.string)) r
    }

type Dispatchers =
  { emitSetStartLayer :: EffectFn1 Int Unit
  , emitSetEndLayer :: EffectFn1 Int Unit
  , runLoadGcodeLines :: EffectFn1 { url :: String } Unit
  }

dispatchers :: DispatcherApi Msg PubState {} -> Dispatchers
dispatchers { emitMsg, emitMsgCtx, readPubState } =
  { emitSetStartLayer: emit MsgSetStartLayer
  , emitSetEndLayer: emit MsgSetEndLayer
  , runLoadGcodeLines: run loadGcodeLines
  }
  where
  loadGcodeLines :: { url :: String } -> ExceptT Err Aff Unit
  loadGcodeLines { url } =
    let
      emitMsg' = liftEffect <<< emitMsgCtx "loadGcodeLines"
    in
      ( do
          st <- liftEffect $ readPubState

          when (st.gcodeLines == Loading) $ do
            throwError (mkErr Err.Err6 "Gcode lines are already loading")

          emitMsg' $ MsgSetGcodeLines Loading
          ret <- Api.getGCodeFile url

          let lines = Str.split (Str.Pattern "\n") ret
          emitMsg' $ MsgSetGcodeLines (Loaded lines)
      )
        `catchError`
          ( \e -> do
              log $ Err.printErr e

              case e.code of
                Err.Err6 -> pure unit
                _ -> emitMsg' $ MsgSetGcodeLines (Error (printErr e))
          )

  emit :: forall a. (a -> Msg) -> EffectFn1 a Unit
  emit f = mkEffectFn1 \arg -> emitMsg (f arg)

  run :: forall a. (a -> ExceptT Err Aff Unit) -> EffectFn1 a Unit
  run f = mkEffectFn1 \arg -> launchAff_ do
    result <- runExceptT $ f arg
    case result of
      Left err -> log (printErr err)
      Right _ -> pure unit

tsApi :: TsApi Msg PubState {} Dispatchers
tsApi = mkTsApi
  { updatePubState: \msg s -> runExcept (updatePubState msg s)
  , dispatchers
  , initPubState
  , initPrivState: {}
  , printError: identity
  , encodeJsonPubState: encode codecPubState
  , encodeMsg
  }

codecPubState :: JsonCodec PubState
codecPubState = CAR.object "PubState"
  { gcodeLines: codecRemoteData (CA.array CA.string)
  , startLayer: CA.int
  , endLayer: CA.int
  }

newtype PubStateAlias = PubStateAlias PubState

derive instance Newtype (PubStateAlias) _

instance TsBridge (PubStateAlias) where
  tsBridge = TSB.tsBridgeNewtype0 Tok { moduleName, typeName: "PubState" }

newtype DispatchersAlias = DispatchersAlias Dispatchers

derive instance Newtype (DispatchersAlias) _

instance TsBridge (DispatchersAlias) where
  tsBridge = TSB.tsBridgeNewtype0 Tok { moduleName, typeName: "Dispatchers" }

instance TsBridge Msg where
  tsBridge = TSB.tsBridgeOpaqueType { moduleName, typeName: "Msg", typeArgs: [] }

-----

moduleName :: String
moduleName = "GCodeViewer.StateMachines.Viewer"

useViewer :: Effect { state :: PubState, dispatch :: Dispatchers }
useViewer = useStateMachine tsApi

tsExports :: Either TSB.AppError (Array DTS.TsModuleFile)
tsExports = TSB.tsModuleFile moduleName
  [ TSB.tsValues Tok
      { useViewer
      }
  ]