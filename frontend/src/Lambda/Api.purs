module Lambda.Api
  ( Error(..)
  , fetch
  , store
  ) where

import Lambda.Prelude

import Data.Variant (match)
import Lambda.Env as Env
import Lambda.Language.Program (Program)
import Lambda.Language.Snapshot as Snapshot
import Lambda.Language.Snapshot.Code (Code)
import Simple.Ajax as Ajax
import Simple.Ajax (AjaxError)

-- | Store `Program` and generate `Code`
store :: Program -> Aff (Either Error Code)
store program = case Snapshot.new program of
  Left err -> pure $ Left $ BadSnapshot err
  Right snapshot -> convert <$> Ajax.post url (Just snapshot)
 where
  url = Env.api <> "/snapshots"
  convert = bimap
    (fromAjaxError Nothing)
    (_.code :: { code :: Code } -> Code)

-- | Fetch `Program` by `Code`
fetch :: Code -> Aff (Either Error Program)
fetch code = convert <$> Ajax.get url
 where
  url = Env.api <> "/snapshots/" <> unwrap code
  convert = either
    (Left <<< fromAjaxError (Just code))
    (lmap BadSnapshot <<< Snapshot.load)

-- | Logic and simplified Ajax errors
data Error
  = BadSnapshot Snapshot.Error
  | ParseError String
  | Missing Code
  | HttpError (Maybe String)

-- | Convert `Ajax.Error` to application-specific `Error`
fromAjaxError :: Maybe Code -> AjaxError -> Error
fromAjaxError code = match
  { parseError: ParseError <<< printJsonDecodeError
  , badRequest: HttpError <<< Just
  , unAuthorized: const ignore
  , forbidden: const ignore
  , notFound: const $ maybe ignore Missing code
  , methodNotAllowed: const ignore
  , serverError: const ignore
  , affjaxError: const ignore
  }
 where
  ignore = HttpError Nothing
