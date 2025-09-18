module GCodeViewer.StateMachines.App
  ( Dispatchers
  , ModuleName
  , Msg(..)
  , PubState
  , getQueryParams
  , mkMsg
  , mkUrl
  , tsApi
  , tsExports
  , useStateMachineApp
  ) where

import GCodeViewer.Prelude

import Control.Monad.Error.Class (catchError)
import Control.Monad.Writer (Writer, runWriter)
import Control.Monad.Writer.Class (tell)
import DTS as DTS
import Data.Argonaut.Core (Json)
import Data.Codec (encode)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Lens (set)
import Data.Lens.Iso.Newtype (unto)
import Data.String as Str
import Data.Symbol (reflectSymbol)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import GCodeViewer.Api (IndexFile, codecIndexFile)
import GCodeViewer.Api as Api
import GCodeViewer.Error (Err, printErr)
import GCodeViewer.RemoteData (RemoteData(..), codecRemoteData)
import GCodeViewer.TsBridge (class TsBridge, Tok(..))
import Named (Named(..), carNamedObject)
import Routing.Duplex (class RouteDuplexParams)
import Routing.Duplex as RD
import Routing.Duplex.Parser (RouteError)
import Stadium.Core (DispatcherApi, TsApi, mkTsApi)
import Stadium.React (useStateMachine)
import Stadium.TL (mkConstructors)
import TsBridge as TSB
import Type.Prelude (Proxy(..))

type PubState = Named ModuleName "PubState"
  { index :: RemoteData { url :: String, content :: IndexFile }
  }

initPubState :: PubState
initPubState = Named
  { index: NotAsked
  }

mkUrl :: { absUrl :: String, relUrl :: String } -> String
mkUrl { absUrl, relUrl } = removeLastSegment absUrl <> "/" <> relUrl

removeLastSegment :: String -> String
removeLastSegment url =
  case Str.lastIndexOf (Str.Pattern "/") url of
    Just i -> Str.take i url
    Nothing -> ""

data Msg = MsgSetIndex (RemoteData { url :: String, content :: IndexFile })

derive instance Generic Msg _

updatePubState :: Msg -> PubState -> Except String PubState
updatePubState msg pubState = case msg of
  MsgSetIndex r -> pubState # set (unto Named <<< prop @"index") r # pure

encodeMsg :: Msg -> { tag :: String, args :: Json }
encodeMsg = case _ of
  MsgSetIndex r ->
    { tag: "MsgSetIndex"
    , args: CA.encode (codecRemoteData (CAR.object "" { url: CA.string, content: codecIndexFile })) r
    }

type Dispatchers = Named ModuleName "Dispatchers"
  { msg :: EffectFn1 Msg Unit
  , runFetchIndex :: EffectFn1 { url :: String } Unit
  }

getQueryParams :: Effect Query
getQueryParams = do
  searchParams <- getSearchParams
  let (query /\ errors) = parseQuery searchParams
  pure query

dispatchers :: DispatcherApi Msg PubState {} -> Dispatchers
dispatchers { emitMsg, emitMsgCtx, readPubState } =
  Named
    { msg: mkEffectFn1 emitMsg
    , runFetchIndex: run fetchIndex
    }
  where
  fetchIndex :: { url :: String } -> ExceptT Err Aff Unit
  fetchIndex { url } = do
    Named st <- liftEffect $ readPubState
    if st.index == Loading then do
      pure unit
    else
      ( do

          liftEffect $ emitMsg $ MsgSetIndex Loading
          index <- Api.getIndexFile { url }
          liftEffect $ emitMsg $ MsgSetIndex (Loaded { url, content: index })
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
codecPubState = carNamedObject
  { index: codecRemoteData (CAR.object "" { url: CA.string, content: codecIndexFile })
  }

instance TsBridge Msg where
  tsBridge = TSB.tsBridgeOpaqueType { moduleName, typeName: "Msg", typeArgs: [] }

-----

type ModuleName = "GCodeViewer.StateMachines.App"

moduleName :: String
moduleName = reflectSymbol (Proxy :: _ ModuleName)

mkMsg :: _
mkMsg = mkConstructors @Msg

tsExports :: Either TSB.AppError (Array DTS.TsModuleFile)
tsExports = TSB.tsModuleFile moduleName
  [ TSB.tsValues Tok
      { useStateMachineApp
      , mkMsg
      , getQueryParams
      , mkUrl
      }
  ]

--

type Query = Named ModuleName "Query"
  { url :: String
  }

parseQuery :: String -> Tuple Query (Array RouteError)
parseQuery s = runWriter do
  { url } <- parse { url: "index.json" } { url: RD.string } s

  pure $ Named { url }

  where

  parse :: forall r1 r2. RouteDuplexParams r1 r2 => Record r2 -> Record r1 -> String -> Writer (Array RouteError) (Record r2)
  parse def r str = case RD.parse (RD.params r) str of
    Left err -> do
      tell [ err ]
      pure def
    Right a -> pure a

foreign import getSearchParams :: Effect String
