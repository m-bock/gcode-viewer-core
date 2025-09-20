module GCodeViewer.StateMachines.Viewer
  ( Dispatchers
  , ModuleName
  , Msg(..)
  , PubState
  , tsApi
  , tsExports
  , useStateMachineViewer
  , mkMsg
  ) where

import GCodeViewer.Prelude

import Control.Monad.Error.Class (catchError)
import Control.Monad.Rec.Class (forever)
import DTS as DTS
import Data.Argonaut.Core (Json)
import Data.Codec (encode)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Lens (set)
import Data.Lens.Iso.Newtype (unto)
import Data.Newtype (class Newtype)
import Data.String as Str
import Effect.Aff (killFiber, runAff)
import Effect.Exception (error)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import GCodeViewer.Api as Api
import GCodeViewer.Error (Err, mkErr, printErr)
import GCodeViewer.Error as Err
import GCodeViewer.RemoteData (RemoteData(..), codecRemoteData)
import GCodeViewer.TsBridge (class TsBridge, Tok(..))
import Heterogeneous.Mapping (class Mapping, hmap)
import Named (Named(..), carNamedObject)
import Record as Record
import Stadium.Core (DispatcherApi, TsApi, mkTsApi)
import Stadium.React (useStateMachine)
import Stadium.TL (mkConstructors, mkMatcher)
import TsBridge as TSB

type ModuleName = "GCodeViewer.StateMachines.Viewer"

type PubState = Named ModuleName "PubState"
  { gcodeLines :: RemoteData (Array String)
  , minLayer :: Int
  , maxLayer :: Int
  , startLayer :: Int
  , endLayer :: Int
  }

initPubState :: PubState
initPubState = Named
  { gcodeLines: NotAsked
  , startLayer: 0
  , endLayer: 100
  , minLayer: 0
  , maxLayer: 100
  }

data Msg
  = MsgSetStartLayer Int
  | MsgSetEndLayer Int
  | MsgSetGcodeLines (RemoteData (Array String))
  | MsgSetMinLayer Int
  | MsgSetMaxLayer Int

derive instance Generic Msg _

updatePubState :: Msg -> PubState -> Except String PubState
updatePubState msg pubState = case msg of
  MsgSetStartLayer startLayer -> pubState
    # set (unto Named <<< prop @"startLayer") startLayer
    # pure

  MsgSetEndLayer endLayer -> pubState
    # set (unto Named <<< prop @"endLayer") endLayer
    # pure

  MsgSetGcodeLines value -> pubState
    # set (unto Named <<< prop @"gcodeLines") value
    # pure

  MsgSetMinLayer minLayer -> pubState
    # set (unto Named <<< prop @"minLayer") minLayer
    # pure

  MsgSetMaxLayer maxLayer -> pubState
    # set (unto Named <<< prop @"maxLayer") maxLayer
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
  MsgSetMinLayer r ->
    { tag: "MsgSetMinLayer"
    , args: CA.encode CA.int r
    }
  MsgSetMaxLayer r ->
    { tag: "MsgSetMaxLayer"
    , args: CA.encode CA.int r
    }

type Dispatchers r =
  { runLoadGcodeLines :: EffectFn1 { url :: String } { cancel :: Effect Unit }
  , msg :: EffectFn1 Msg Unit
  | r
  }

data F msg = F { emitMsg :: msg -> Effect Unit }

instance Mapping (F msg) (arg -> msg) (EffectFn1 arg Unit) where
  mapping (F { emitMsg }) mkMsg = mkEffectFn1 \arg -> emitMsg (mkMsg arg)

dispatchers :: DispatcherApi Msg PubState {} -> Dispatchers _
dispatchers { emitMsg, emitMsgCtx, readPubState } =
  ( Record.merge
      { msg: mkEffectFn1 emitMsg
      , runLoadGcodeLines: run loadGcodeLines
      }
      ctors
  )
  where
  ctors = hmap (F { emitMsg }) mkMsg

  loadGcodeLines :: { url :: String } -> ExceptT Err Aff Unit
  loadGcodeLines { url } = do
    let emitMsg' = liftEffect <<< emitMsgCtx "loadGcodeLines"
    do
      Named st <- liftEffect $ readPubState
      case st.gcodeLines of
        Loading -> pure unit
        _ ->
          ( do
              emitMsg' (MsgSetGcodeLines Loading)
              ret <- Api.getGCodeFile url
              let lines = Str.split (Str.Pattern "\n") ret
              emitMsg' (MsgSetGcodeLines (Loaded lines))
          ) `catchError`
            ( \e -> do
                emitMsg' (MsgSetGcodeLines (Error $ printErr e))
            )
      pure unit

  run :: forall a. (a -> ExceptT Err Aff Unit) -> EffectFn1 a { cancel :: Effect Unit }
  run f = mkEffectFn1 \arg -> do
    fiber <- runAff (const $ pure unit) do
      result <- runExceptT $ f arg
      case result of
        Left err -> log (printErr err)
        Right _ -> pure unit

    pure { cancel: launchAff_ $ killFiber (error "cancel") fiber }

mkMsg :: _
mkMsg = mkConstructors @Msg

tsApi :: TsApi Msg PubState {} (Dispatchers _)
tsApi = mkTsApi
  { updatePubState: \msg s -> runExcept (updatePubState msg s)
  , dispatchers
  , initPubState: initPubState
  , initPrivState: {}
  , printError: identity
  , encodeJsonPubState: encode codecPubState
  , encodeMsg
  }

codecPubState :: JsonCodec PubState
codecPubState = carNamedObject
  { gcodeLines: codecRemoteData (CA.array CA.string)
  , startLayer: CA.int
  , endLayer: CA.int
  , minLayer: CA.int
  , maxLayer: CA.int
  }

instance TsBridge Msg where
  tsBridge = TSB.tsBridgeOpaqueType { moduleName, typeName: "Msg", typeArgs: [] }

-----

moduleName :: String
moduleName = "GCodeViewer.StateMachines.Viewer"

useStateMachineViewer :: Effect { state :: PubState, dispatch :: Dispatchers _ }
useStateMachineViewer = useStateMachine tsApi

tsExports :: Either TSB.AppError (Array DTS.TsModuleFile)
tsExports = TSB.tsModuleFile moduleName
  [ TSB.tsValues Tok
      { useStateMachineViewer
      , mkMsg
      }
  ]