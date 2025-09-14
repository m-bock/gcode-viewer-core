module GCodeViewer.Prelude
  ( module Export
  , prop
  ) where

import Control.Monad.Error.Class (class MonadError, throwError) as Export
import Control.Monad.Except (ExceptT, runExceptT, runExcept, Except) as Export
import Data.Either (Either(..)) as Export
import Data.Generic.Rep (class Generic) as Export
import Data.Lens (Lens)
import Data.Lens.Record as LensRecord
import Data.Maybe (Maybe(..)) as Export
import Data.Show.Generic (genericShow) as Export
import Data.Symbol (class IsSymbol)
import Data.Symbol (class IsSymbol) as Export
import Data.Time.Duration (Milliseconds(..)) as Export
import Effect (Effect) as Export
import Effect.Aff (Aff, launchAff_, delay) as Export
import Effect.Aff.Class (class MonadAff, liftAff) as Export
import Effect.Class (liftEffect) as Export
import Effect.Class.Console (log) as Export
import Prelude (class Applicative, class Apply, class Bind, class BooleanAlgebra, class Bounded, class Category, class CommutativeRing, class Discard, class DivisionRing, class Eq, class EuclideanRing, class Field, class Functor, class HeytingAlgebra, class Monad, class Monoid, class Ord, class Ring, class Semigroup, class Semigroupoid, class Semiring, class Show, type (~>), Ordering(..), Unit, Void, absurd, add, ap, append, apply, between, bind, bottom, clamp, compare, comparing, compose, conj, const, degree, discard, disj, div, eq, flap, flip, gcd, identity, ifM, join, lcm, liftA1, liftM1, map, max, mempty, min, mod, mul, negate, not, notEq, one, otherwise, pure, recip, show, sub, top, unit, unless, unlessM, void, when, whenM, zero, (#), ($), ($>), (&&), (*), (*>), (+), (-), (/), (/=), (<), (<#>), (<$), (<$>), (<*), (<*>), (<<<), (<=), (<=<), (<>), (<@>), (=<<), (==), (>), (>=), (>=>), (>>=), (>>>), (||)) as Export
import Prim.Row as Row
import Type.Proxy (Proxy(..))
import Type.Proxy (Proxy) as Export

prop :: forall @l r1 r2 r a b. IsSymbol l => Row.Cons l a r r1 => Row.Cons l b r r2 => Lens (Record r1) (Record r2) a b
prop = LensRecord.prop (Proxy :: _ l)