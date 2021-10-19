{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Backend.Micro
  ( run
  , root
  , segment
  , param
  , (//)
  , get
  , post
  , notFound
  , badRequest
  , badMethod
  , internalError
  , Path
  , Handler
  ) where

import Backend.Prelude hiding (handle)

import Backend.Wai (Error(..), errorResponse, jsonResponse)
import qualified Data.Text as T
import Network.Wai
import qualified RIO.ByteString as BS

-- | Root path /
root :: Path '[]
root = End

-- | Capture variable in path
param :: FromHttpApiData v => Path '[v]
param = Param End

-- | Literal path text. Prefer using the 'IsString' instance instead.
segment :: Text -> Path '[]
segment = foldr Segment End . pieces
  where pieces = filter (not . T.null) . T.split (== '/')

-- | Join paths
(//) :: Path xs -> Path ys -> Path (Append xs ys)
lhs // rhs = case lhs of
  End -> rhs
  Segment s ps -> Segment s $ ps // rhs
  Param ps -> Param $ ps // rhs
infixl 1 //

-- | Handle GET request
--
-- Handler receives capture variables as typed, parsed arguments.
--
get
  :: forall m f vs r
   . (MonadIO m, Apply m f vs, Result m f vs ~ m r, ToJSON r)
  => Path vs
  -> f
  -> Handler m ()
get path f = handle methodGet path $ const . apply @m f

-- | Handle POST request
--
-- Handler receives request body and capture variables as typed, parsed
-- arguments.
--
post
  :: forall m f vs body r
   . ( FromJSON body
     , MonadIO m
     , Apply m f (body ': vs)
     , Result m f (body ': vs) ~ m r
     , ToJSON r
     )
  => Path vs
  -> f
  -> Handler m ()
post path f = handle methodPost path $ \args req -> do
  body :: body <- liftIO $ jsonBody req
  apply @m f $ body :- args

-- | throw 404 Not Found
notFound :: MonadIO m => m a
notFound = throwIO $ Error [] status404 Nothing

-- | throw 400 Bad Request
badRequest :: MonadIO m => Text -> m a
badRequest = throwIO . Error [] status400 . Just

-- | Throw 405 Method Not Allowed
badMethod :: MonadIO m => [Method] -> m a
badMethod allowed = throwIO $ Error headers status405 Nothing
  where headers = [("Allow", BS.intercalate ", " allowed)]

-- | Throw 500 Internal Server Error
internalError :: MonadIO m => Text -> m a
internalError = throwIO . Error [] status500 . Just

-- | Build handlers for specific methods
handle
  :: forall m vs r
   . (MonadIO m, ToJSON r)
  => Method
  -> Path vs
  -> (Args vs -> Request -> m r)
  -> Handler m ()
handle method path f = modify (|> match)
 where
  match m ts = case parse path ts of
    Just (Right args)
      | m == method -> Match $ fmap jsonResponse . f args
      | otherwise -> WrongMethod [method]
    Just (Left err) -> BadParse err
    Nothing -> NoMatch

-- | Convert 'Handler' to Wai 'Application'
run :: MonadUnliftIO m => Handler m () -> m Application
run routes = do
  io <- askRunInIO
  pure $ \req respond -> do
    let match = findMatch (requestMethod req) (pathInfo req) routes
    result <- try $ case match of
      NoMatch -> notFound
      WrongMethod allowed -> badMethod allowed
      BadParse err -> badRequest err
      Match act -> io $ act req
    respond $ case result of
      Left e@SomeException{}
        | Just err <- fromException e -> errorResponse err
        | otherwise -> errorResponse $ Error [] status500 $ pure $ tshow e
      Right response -> response

-- | Parse JSON from request body
jsonBody :: forall a m . (MonadIO m, FromJSON a) => Request -> m a
jsonBody req = do
  let contentType = lookup "content-type" $ requestHeaders req
  unless (contentType == Just "application/json")
    $ badRequest "content type not JSON"
  bytes <- liftIO $ lazyRequestBody req
  either (badRequest . pack) pure $ eitherDecode bytes

-- | Attempt to find a match given 'Handler'
findMatch :: Method -> [Text] -> Handler m () -> Match m
findMatch method ts = foldMap step . flip execState mempty . runHandler
  where step matcher = matcher method ts

-- | Path indexed by capture variable types
data Path (vs :: [Type]) where
  End ::Path '[]
  Segment ::Text -> Path vs -> Path vs
  Param ::FromHttpApiData v => Path vs -> Path (v ': vs)

instance vs ~ '[] => IsString (Path vs) where
  fromString = segment . fromString

-- | Type-level list append
type family Append xs ys where
  Append '[] ys = ys
  Append (x ': xs) ys = x ': Append xs ys

-- | Heterogenous list of function arguments
data Args vs where
  Nil ::Args '[]
  (:-) ::v -> Args vs -> Args (v ': vs)
infixr 5 :-

-- | Parse path segments into 'Args' if they match 'Path'
--
-- Returns 'Nothing' if segments don't match. Returns 'Left' if
-- segments did match but some variable failed to parse.
--
parse :: Path vs -> [Text] -> Maybe (Either Text (Args vs))
parse End [] = Just $ Right Nil
parse _ [] = Nothing
parse path (t : ts) = case path of
  End -> Nothing
  Segment s ps
    | s == t -> parse ps ts
    | otherwise -> Nothing
  Param ps -> case parseUrlPiece t of
    Left err -> Just $ Left err
    Right arg -> fmap (arg :-) <$> parse ps ts

-- | Class for applying functions to 'Args' in some monad
class Apply m f vs where
  type Result m f vs
  apply :: f -> Args vs -> Result m f vs

instance (m ~ n) => Apply m (n r) '[] where
  type Result m (n r) '[] = n r
  apply f Nil = f

instance (Apply m r vs, v ~ a) => Apply m (a -> r) (v ': vs) where
  type Result m (a -> r) (v ': vs) = Result m r vs
  apply f (v :- vs) = apply @m (f v :: r) vs

-- | Result of running 'findMatch'
data Match m
  = Match (Request -> m Response)
  -- ^ Matched path - run the embedded handler
  | WrongMethod [Method]
  -- ^ Path matched but can only handle these methods
  | BadParse Text
  -- ^ Path matched but a param failed to parse
  | NoMatch
  -- ^ No match at all

instance Semigroup (Match m) where
  -- Bad parses take highest priority
  BadParse lhs <> _ = BadParse lhs
  _ <> BadParse rhs = BadParse rhs

  -- Followed by successful matches
  Match lhs <> _ = Match lhs
  _ <> Match rhs = Match rhs

  -- Followed by wrong method (which collects allowed methods)
  WrongMethod lhs <> WrongMethod rhs = WrongMethod $ lhs <> rhs

  -- No match is the identity
  NoMatch <> rhs = rhs
  lhs <> NoMatch = lhs

instance Monoid (Match m) where
  mempty = NoMatch

-- | Generate a 'Match' based on method and path pieces
type Matcher m = Method -> [Text] -> Match m

-- | State-based writer for collecting 'Matcher's
newtype Handler m a = Handler { runHandler :: State (Seq (Matcher m)) a }
  deriving newtype (Functor, Applicative, Monad, MonadState (Seq (Matcher m)))
