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

newtype NamedRecord (moduleName :: Symbol) (typeName :: Symbol) (r :: Row Type) = NamedRecord (Record r)

derive newtype instance Show (Record r) => Show (NamedRecord moduleName typeName r)
derive newtype instance Eq (Record r) => Eq (NamedRecord moduleName typeName r)
derive newtype instance Ord (Record r) => Ord (NamedRecord moduleName typeName r)

derive instance Newtype (NamedRecord moduleName typeName a) _

carNamedObject
  :: forall (typeName :: Symbol) (moduleName :: Symbol) (ri ∷ Row Type) (ro ∷ Row Type) (rl ∷ RowList Type)
   . RowToList ri rl
  => RowListCodec rl ri ro
  => IsSymbol typeName
  => Record ri
  → JsonCodec (NamedRecord moduleName typeName ro)
carNamedObject r = dimap unwrap wrap $ CAR.object (reflectSymbol (Proxy :: _ typeName)) r

