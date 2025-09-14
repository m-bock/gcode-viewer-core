module GCodeViewer.CodecExtra where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError(..))
import Data.Codec.Argonaut as CA
import Data.Either (Either)

encTag :: String -> Json
encTag tag = CA.encode CA.string tag

encTagWithArgs :: forall c. String -> JsonCodec c -> c -> Json
encTagWithArgs tag codecC val = encodeJson [ CA.encode CA.string tag, CA.encode codecC val ]

decTag :: forall a. String -> a -> Json -> Either JsonDecodeError a
decTag tag val j = do
  str <- CA.decode CA.string j
  if str == tag then pure val else throwError (TypeMismatch "no match")

decTagWithArgs :: forall c a. String -> (c -> a) -> JsonCodec c -> Json -> Either JsonDecodeError a
decTagWithArgs expectedTag mkVal codecC j = do
  parts <- CA.decode CA.jarray j

  { tagJson, argsJson } <- case parts of
    [ tagJson, argsJson ] -> pure { tagJson, argsJson }
    _ -> throwError (TypeMismatch "no match")

  tag <- CA.decode CA.string tagJson
  args <- CA.decode codecC argsJson

  if tag == expectedTag then pure (mkVal args) else throwError (TypeMismatch "no match")
