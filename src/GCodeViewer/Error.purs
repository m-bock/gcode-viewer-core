module GCodeViewer.Error where

import GCodeViewer.Prelude

type Err = { message :: String, code :: ErrCode }

mkErr :: ErrCode -> String -> Err
mkErr code message = { message, code }

printErr :: Err -> String
printErr { message, code } = "Error: " <> message <> " (" <> show code <> ")"

data ErrCode
  = Err1
  | Err2
  | Err3
  | Err4
  | Err5
  | Err6
  | XErr7
  | XErr8
  | XErr9
  | XErr10
  | ErrX

derive instance Eq ErrCode

derive instance Ord ErrCode

derive instance Generic ErrCode _

instance Show ErrCode where
  show = genericShow

handleAffEither :: forall m err a. MonadError Err m => MonadAff m => (err -> Err) -> Aff (Either err a) -> m a
handleAffEither errToString act = do
  ret <- liftAff act
  case ret of
    Left err -> throwError $ errToString err
    Right a -> pure a

handleEither :: forall m err a. MonadError Err m => (err -> Err) -> Either err a -> m a
handleEither errToString = case _ of
  Left err -> throwError $ errToString err
  Right a -> pure a
