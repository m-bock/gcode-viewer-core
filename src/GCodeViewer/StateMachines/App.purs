module GCodeViewer.StateMachines.App
  ( PubState
  , Msg(..)
  , Dispatchers
  , tsApi
  , tsExports
  , useStateMachineApp
  ) where

import GCodeViewer.Prelude

import Control.Monad.Error.Class (catchError, try)
import DTS as DTS
import Data.Argonaut.Core (Json)
import Data.Codec (encode)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Lens (set)
import Data.Newtype (class Newtype)
import Effect.Class.Console (logShow)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import GCodeViewer.Api (IndexFile, codecIndexFile)
import GCodeViewer.Api as Api
import GCodeViewer.Error (Err, printErr)
import GCodeViewer.RemoteData (RemoteData(..), codecRemoteData)
import GCodeViewer.TsBridge (class TsBridge, Tok(..))
import Routing.Duplex (RouteDuplex, RouteDuplex', (:=))
import Routing.Duplex as RD
import Routing.Duplex as RoutingDuplex
import Stadium.Core (DispatcherApi, TsApi, mkTsApi)
import Stadium.React (useStateMachine)
import Stadium.TL (mkConstructors)
import TsBridge as TSB
import Type.Prelude (Proxy(..))

type PubState =
  { index :: RemoteData IndexFile
  }

initPubState :: PubState
initPubState =
  { index: NotAsked
  }

data Msg = MsgSetIndex (RemoteData IndexFile)

derive instance Generic Msg _

updatePubState :: Msg -> PubState -> Except String PubState
updatePubState msg pubState = case msg of
  MsgSetIndex r -> pubState # set (prop @"index") r # pure

encodeMsg :: Msg -> { tag :: String, args :: Json }
encodeMsg = case _ of
  MsgSetIndex r ->
    { tag: "MsgSetIndex"
    , args: CA.encode (codecRemoteData codecIndexFile) r
    }

type Dispatchers =
  { msg :: EffectFn1 Msg Unit
  , runFetchIndex :: EffectFn1 { url :: String } Unit
  }

dispatchers :: DispatcherApi Msg PubState {} -> Dispatchers
dispatchers { emitMsg, emitMsgCtx, readPubState } =
  { msg: mkEffectFn1 emitMsg
  , runFetchIndex: run fetchIndex
  }
  where
  fetchIndex :: { url :: String } -> ExceptT Err Aff Unit
  fetchIndex { url } = do
    st <- liftEffect $ readPubState
    if st.index == Loading then do
      pure unit
    else
      ( do
          let r = RD.parse rdQuery "?url=ffo&debug=true"

          logShow r

          liftEffect $ emitMsg $ MsgSetIndex Loading
          index <- Api.getIndexFile { url }
          liftEffect $ emitMsg $ MsgSetIndex (Loaded index)
      ) `catchError`
        ( \e -> do
            liftEffect $ emitMsg $ MsgSetIndex (Error (printErr e))
        )

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

useStateMachineApp :: Effect { state :: PubState, dispatch :: Dispatchers }
useStateMachineApp = useStateMachine tsApi

codecPubState :: JsonCodec PubState
codecPubState = CAR.object "PubState"
  { index: codecRemoteData codecIndexFile
  }

instance TsBridge Msg where
  tsBridge = TSB.tsBridgeOpaqueType { moduleName, typeName: "Msg", typeArgs: [] }

-----

moduleName :: String
moduleName = "GCodeViewer.StateMachines.App"

tsExports :: Either TSB.AppError (Array DTS.TsModuleFile)
tsExports = TSB.tsModuleFile moduleName
  [ TSB.tsValues Tok
      { useStateMachineApp
      , mkMsg: mkConstructors @Msg
      }
  ]

--

type Query =
  { url :: String
  , debug :: Boolean
  }

rdQuery :: RouteDuplex' Query
rdQuery =
  RD.params
    { url: RD.string
    , debug: RD.boolean
    }
