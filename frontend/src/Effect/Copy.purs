module Effect.Copy
  ( copy
  , hasClipboard
  , Error(..)
  ) where

import Lambda.Prelude

import Effect.Aff.Compat (fromEffectFnAff, EffectFnAff)

copy :: String -> Aff (Either Error Unit)
copy text
  | hasClipboard = clipboardWrite text
  | otherwise = pure $ Left NoCapability

clipboardWrite :: String -> Aff (Either Error Unit)
clipboardWrite = fromEffectFnAff <<< clipboardWriteImpl (Left <<< CopyFailed) Right

-- | Browser has navigator.clipboard?
foreign import hasClipboard :: Boolean

-- | Async navigator.clipboard.writeText
foreign import clipboardWriteImpl
  :: (String -> Either Error Unit)
  -> (Unit -> Either Error Unit)
  -> String
  -> EffectFnAff (Either Error Unit)

data Error
  = CopyFailed String
  | NoCapability

derive instance genericError :: Generic Error _
derive instance eqError :: Eq Error

instance Show Error where
  show = case _ of
    CopyFailed err -> "navigator.clipboard.write[Text] failed: " <> err
    NoCapability -> "no clipboard capability detected"
