module GCodeViewer.TsBridge where

import Prelude

import DTS as DTS
import Data.Function.Uncurried (Fn2, Fn3, Fn4)
import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol)
import Effect (Effect)
import Effect.Uncurried (EffectFn1)
import Named (NamedRecord)
import TsBridge as TSB
import Type.Data.Symbol (reflectSymbol)
import Type.Prelude (Proxy(..))
import Type.Proxy (Proxy)

data Tok = Tok

instance TsBridge a => TSB.TsBridgeBy Tok a where
  tsBridgeBy _ = tsBridge

class TsBridge (a :: Type) where
  tsBridge :: Proxy a -> TSB.TsBridgeM DTS.TsType

instance TsBridge Unit where
  tsBridge = TSB.tsBridgeUnit

instance TsBridge Number where
  tsBridge = TSB.tsBridgeNumber

instance TsBridge Int where
  tsBridge = TSB.tsBridgeInt

instance TsBridge Boolean where
  tsBridge = TSB.tsBridgeBoolean

instance TsBridge String where
  tsBridge = TSB.tsBridgeString

instance TsBridge a => TsBridge (Effect a) where
  tsBridge = TSB.tsBridgeEffect Tok

instance TsBridge a => TsBridge (Maybe a) where
  tsBridge = TSB.tsBridgeMaybe Tok

instance TsBridge a => TsBridge (Array a) where
  tsBridge = TSB.tsBridgeArray Tok

instance (TsBridge a, TsBridge b) => TsBridge (a -> b) where
  tsBridge = TSB.tsBridgeFunction Tok

instance (TsBridge a, TsBridge b, TsBridge c) => TsBridge (Fn2 a b c) where
  tsBridge = TSB.tsBridgeFn2 Tok

instance (TsBridge a, TsBridge b, TsBridge c, TsBridge d) => TsBridge (Fn3 a b c d) where
  tsBridge = TSB.tsBridgeFn3 Tok

instance (TsBridge a, TsBridge b, TsBridge c, TsBridge d, TsBridge e) => TsBridge (Fn4 a b c d e) where
  tsBridge = TSB.tsBridgeFn4 Tok

instance (TSB.TsBridgeRecord Tok r) => TsBridge (Record r) where
  tsBridge = TSB.tsBridgeRecord Tok

instance (TSB.TsBridgeRecord Tok r, IsSymbol s) => TsBridge (NamedRecord s r) where
  tsBridge = TSB.tsBridgeNewtype0 Tok { moduleName: "TypeAliases", typeName: reflectSymbol (Proxy :: _ s) }

instance IsSymbol sym => TsBridge (TSB.TypeVar sym) where
  tsBridge = TSB.tsBridgeTypeVar

instance (TsBridge a, TsBridge b) => TsBridge (EffectFn1 a b) where
  tsBridge = TSB.tsBridgeEffectFn1 Tok