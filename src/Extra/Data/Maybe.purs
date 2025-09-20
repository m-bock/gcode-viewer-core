module Extra.Data.Maybe where

import Prelude

import DTS as DTS
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Internal.TsBridge (Tok(..))
import Stadium.TL (mkConstructors1, mkMatcher1)
import TsBridge (TypeVar)
import TsBridge as TSB

moduleName :: String
moduleName = "Extra.Data.Maybe"

mkMaybe :: _
mkMaybe = mkConstructors1 @Maybe @(TypeVar "A")

onMaybe :: _
onMaybe = mkMatcher1 @Maybe @(TypeVar "A") @(TypeVar "Z")

eqMaybe :: forall @a. (a -> a -> Boolean) -> Maybe a -> Maybe a -> Boolean
eqMaybe eq = case _ of
  Just a -> case _ of
    Just b -> eq a b
    Nothing -> false
  Nothing -> case _ of
    Just _ -> false
    Nothing -> true

tsExports :: Either TSB.AppError (Array DTS.TsModuleFile)
tsExports = TSB.tsModuleFile moduleName
  [ TSB.tsValues Tok
      { mkMaybe
      , onMaybe
      , eqMaybe: eqMaybe @(TypeVar "A")
      }
  ]