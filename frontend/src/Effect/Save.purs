module Effect.Save
  ( save
  , Filename(..)
  ) where

import Lambda.Prelude

newtype Filename = Filename String

derive instance newtypeFilename :: Newtype Filename _
derive newtype instance eqFilename :: Eq Filename
derive newtype instance showFilename :: Show Filename

save :: { text :: String, to :: Filename } -> Effect (Either String Unit)
save { text, to: Filename filename } = saveImpl Left Right text filename

-- | Save text to file
foreign import saveImpl
  :: (String -> Either String Unit)
  -> (Unit -> Either String Unit)
  -> String
  -> String
  -> Effect (Either String Unit)
