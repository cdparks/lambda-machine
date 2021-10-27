module Effect.Copy
  ( copy
  ) where

import Lambda.Prelude

import Effect.Aff.Compat (fromEffectFnAff, EffectFnAff)

copy :: String -> Aff (Either String Unit)
copy = fromEffectFnAff <<< copyImpl Left Right

-- | Async navigator.clipboard.writeText
foreign import copyImpl
  :: (String -> Either String Unit)
  -> (Unit -> Either String Unit)
  -> String
  -> EffectFnAff (Either String Unit)
