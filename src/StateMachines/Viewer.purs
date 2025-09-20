module StateMachines.Viewer
  ( Dispatchers
  , ModuleName
  , Msg(..)
  , PubState
  , tsApi
  , tsExports
  , useStateMachineViewer
  ) where

import Internal.Prelude
import Prelude

import Api as Api
import Control.Monad.Error.Class (catchError)
import Control.Monad.Rec.Class (forever)
import DTS as DTS
import Data.Argonaut.Core (Json)
import Data.Codec (encode)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Lens (set)
import Data.Lens.Iso.Newtype (unto)
import Data.String as Str
import Data.Symbol (reflectSymbol)
import Effect.Aff (killFiber, runAff)
import Effect.Exception (error)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Error (Err, printErr)
import Internal.TsBridge (class TsBridge, Tok(..))
import Named (Named(..), carNamedObject)
import Record as Record
import RemoteData (RemoteData(..), codecRemoteData)
import Stadium.Core (DispatcherApi, TsApi, mkTsApi)
import Stadium.React (useStateMachine)
import Stadium.TL (mkConstructors, mkCtorEmitter)
import TsBridge as TSB
import Type.Proxy (Proxy(..))

type ModuleName = "StateMachines.Viewer"

type PubState = Named ModuleName "PubState"
  { gcodeLines :: Array String
  , gcodeFile :: RemoteData String
  , minLayer :: Int
  , maxLayer :: Int
  , startLayer :: Int
  , endLayer :: Int
  }

initPubState :: PubState
initPubState = Named
  { gcodeLines: []
  , gcodeFile: NotAsked
  , startLayer: 0
  , endLayer: 100
  , minLayer: 0
  , maxLayer: 100
  }

data Msg
  = MsgSetStartLayer Int
  | MsgSetEndLayer Int
  | MsgSetGcodeFile (RemoteData String)
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

  MsgSetGcodeFile value -> pubState
    # set (unto Named <<< prop @"gcodeFile") value
    #
      ( case value of
          Loaded ret ->
            let
              lines = Str.split (Str.Pattern "\n") ret
            in
              set (unto Named <<< prop @"gcodeLines") lines
          _ -> identity
      )
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
  MsgSetGcodeFile r ->
    { tag: "MsgSetGcodeFile"
    , args: CA.encode (codecRemoteData CA.string) r
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
  { runLoadGcodeLines ::
      EffectFn1 { url :: String, interval :: Number } { cancel :: Effect Unit }
  | r
  }

dispatchers :: DispatcherApi Msg PubState {} -> Dispatchers _
dispatchers { emitMsg, emitMsgCtx, readPubState } =
  ( Record.merge
      { runLoadGcodeLines: run loadGcodeLines
      }
      ctors
  )
  where
  ctors = mkCtorEmitter { emitMsg } mkMsg

  loadGcodeLines :: { url :: String, interval :: Number } -> ExceptT Err Aff Unit
  loadGcodeLines { url, interval } =
    forever
      ( do
          liftEffect $ emitMsg (MsgSetGcodeFile Loading)
          ret <- Api.getGCodeFile url
          liftEffect $ emitMsg (MsgSetGcodeFile (Loaded ret))
          liftAff $ delay (Milliseconds interval)
      ) `catchError`
      ( \e -> do
          liftEffect $ emitMsg (MsgSetGcodeFile (Error $ printErr e))
      )

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
  { gcodeLines: CA.array CA.string
  , gcodeFile: codecRemoteData CA.string
  , startLayer: CA.int
  , endLayer: CA.int
  , minLayer: CA.int
  , maxLayer: CA.int
  }

instance TsBridge Msg where
  tsBridge = TSB.tsBridgeOpaqueType { moduleName, typeName: "Msg", typeArgs: [] }

-----

moduleName :: String
moduleName = reflectSymbol (Proxy :: Proxy ModuleName)

useStateMachineViewer :: Effect { state :: PubState, dispatch :: Dispatchers _ }
useStateMachineViewer = useStateMachine tsApi

tsExports :: Either TSB.AppError (Array DTS.TsModuleFile)
tsExports = TSB.tsModuleFile moduleName
  [ TSB.tsValues Tok
      { useStateMachineViewer
      }
  ]