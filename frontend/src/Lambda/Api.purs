module Lambda.Api
  ( Error(..)
  , fetch
  , store
  ) where

import Lambda.Prelude

import Effect.Console as Console
import Lambda.Env as Env
import Lambda.Language.Pretty (class Pretty, text, pretty)
import Lambda.Language.Program (Program)
import Lambda.Language.Snapshot as Snapshot
import Lambda.Language.Snapshot.Code (Code(..))

-- | TODO: actually post to api
store :: Program -> Aff (Either Error Code)
store program = liftEffect do
  Console.log $ "Env.api: " <> Env.api
  case Snapshot.new program of
    Left err -> pure $ Left $ SnapshotError err
    Right snapshot -> do
      Console.log $ "POST " <> Env.api <> "/snapshots"
      Console.log $ "  => " <> stringify (encodeJson snapshot)
      pure $ Right $ Code "SNAPSH0T"

-- | TODO: actually fetch from api
fetch
  :: Code
  -> Aff (Either Error Program)
fetch code = liftEffect do
  Console.log $ "Env.api: " <> Env.api
  Console.log $ "GET " <> Env.api <> "/snapshots/" <> unwrap code
  pure $ Right { defs: [], expr: Nothing }

-- | Http or Snapshot errors
data Error
  = HttpError -- temporary
  | SnapshotError Snapshot.Error

derive instance eqError :: Eq Error
derive instance genericError :: Generic Error _

instance showError :: Show Error where
  show x = genericShow x

instance Pretty Error where
  pretty rep = case _ of
    HttpError -> text "http error"
    SnapshotError err -> pretty rep err
