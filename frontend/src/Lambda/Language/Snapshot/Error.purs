module Lambda.Language.Snapshot.Error
  ( Error(..)
  ) where

import Lambda.Prelude

import Data.Array as Array
import Lambda.Language.Name (Name)
import Lambda.Language.Pretty (class Pretty, text, pretty)
import Lambda.Language.Snapshot.Tag (Tag)
import Data.Grammar (pluralizeWith)

-- | Errors that can occur while creating or loading a `Snapshot`
data Error
  -- | Payload would use more than 29 bits
  = PayloadOutOfRange Tag Int
  -- | Unrecognized 3-bit tag (only 0b000 now)
  | UnrecognizedTag Tag
  -- | Unprocessed values on the stack after running out of instructions
  | ExtraStackValues Int
  -- | Not enough values on the stack
  | StackUnderflow { op :: String, wanted :: Int, saw :: Int }
  -- | Bad index into the name store
  | IndexOutOfRange Int (Array Name)

derive instance eqError :: Eq Error
derive instance genericError :: Generic Error _

instance showError :: Show Error where
  show x = genericShow x

instance Pretty Error where
  pretty rep = case _ of
    PayloadOutOfRange tag p -> fold
      [ text "payload "
      , text $ show p
      , text " for tag "
      , pretty rep tag
      , text " out of range for 29 bits"
      ]
    UnrecognizedTag tag -> fold
      [ text "unrecognized tag "
      , pretty rep tag
      ]
    ExtraStackValues n -> fold
      [ text "malformed snapshot; "
      , text $ show n
      , text " extra expressions left on stack"
      ]
    StackUnderflow { op, wanted, saw } -> fold
      [ text "stack underflow for "
      , text op
      , text "; wanted "
      , text $ show wanted
      , text $ pluralizeWith "s" wanted "item"
      , text ", saw "
      , text $ show saw
      ]
    IndexOutOfRange i names -> fold
      [ text "index "
      , text $ show i
      , text " out of range for symbol table ["
      , Array.intercalate (text ", ") (pretty rep <$> names)
      , text "]"
      ]
