module Named where

import Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut.Record (class RowListCodec)
import Data.Codec.Argonaut.Record as CAR
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (dimap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.RowList (class RowToList, RowList)
import Type.Proxy (Proxy(..))

newtype Named (moduleName :: Symbol) (typeName :: Symbol) (a :: Type) = Named a

derive newtype instance Show a => Show (Named moduleName typeName a)
derive newtype instance Eq a => Eq (Named moduleName typeName a)
derive newtype instance Ord a => Ord (Named moduleName typeName a)

derive instance Newtype (Named moduleName typeName a) _

carNamedObject
  :: forall (typeName :: Symbol) (moduleName :: Symbol) (ri ∷ Row Type) (ro ∷ Row Type) (rl ∷ RowList Type)
   . RowToList ri rl
  => RowListCodec rl ri ro
  => IsSymbol typeName
  => Record ri
  → JsonCodec (Named moduleName typeName (Record ro))
carNamedObject r = dimap unwrap wrap $ CAR.object (reflectSymbol (Proxy :: _ typeName)) r

