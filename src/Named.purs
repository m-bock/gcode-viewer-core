module Named where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Codec (Codec)
import Data.Codec.Argonaut (JsonDecodeError, JsonCodec)
import Data.Codec.Argonaut.Record (class RowListCodec)
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (dimap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.RowList (class RowToList, RowList)
import Type.Proxy (Proxy(..))

newtype NamedRecord (s :: Symbol) (r :: Row Type) = Named (Record r)

derive newtype instance Show (Record r) => Show (NamedRecord s r)
derive newtype instance Eq (Record r) => Eq (NamedRecord s r)
derive newtype instance Ord (Record r) => Ord (NamedRecord s r)

derive instance Newtype (NamedRecord s a) _

carNamedObject
  :: forall (s :: Symbol) (ri ∷ Row Type) (ro ∷ Row Type) (rl ∷ RowList Type)
   . RowToList ri rl
  => RowListCodec rl ri ro
  => IsSymbol s
  => Record ri
  → JsonCodec (NamedRecord s ro)
carNamedObject r = dimap unwrap wrap $ CAR.object (reflectSymbol (Proxy :: _ s)) r

