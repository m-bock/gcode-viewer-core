module GCodeViewer.RemoteData where

import Prelude

import DTS as DTS
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Sum as CAS
import Data.Tuple.Nested ((/\))
import GCodeViewer.Prelude (class Generic, Either)
import GCodeViewer.TsBridge (class TsBridge, tsBridge, Tok(..))
import Stadium.TL (mkConstructors1, mkMatcher1)
import TsBridge (TypeVar)
import TsBridge as TSB
import Type.Proxy (Proxy(..))

data RemoteData a
  = NotAsked
  | Loading
  | Loaded a
  | Error String

derive instance Eq a => Eq (RemoteData a)

derive instance Generic (RemoteData a) _

codecRemoteData :: forall a. JsonCodec a -> JsonCodec (RemoteData a)
codecRemoteData codecValue = CAS.sum "RemoteData"
  { "NotAsked": unit
  , "Loading": unit
  , "Loaded": codecValue
  , "Error": CA.string
  }

---

moduleName :: String
moduleName = "GCodeViewer.RemoteData"

instance TsBridge a => TsBridge (RemoteData a) where
  tsBridge = TSB.tsBridgeOpaqueType
    { moduleName
    , typeName: "RemoteData"
    , typeArgs:
        [ "A" /\ tsBridge (Proxy :: _ a)
        ]
    }

mkRemoteData :: _
mkRemoteData = mkConstructors1 @RemoteData @(TypeVar "A")

onRemoteData :: _
onRemoteData = mkMatcher1 @RemoteData @(TypeVar "A") @(TypeVar "Z")

tsExports :: Either TSB.AppError (Array DTS.TsModuleFile)
tsExports = TSB.tsModuleFile moduleName
  [ TSB.tsValues Tok
      { mkRemoteData
      , onRemoteData
      }
  ]
