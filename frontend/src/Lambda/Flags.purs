module Lambda.Flags
  ( Flags
  , none
  , parse
  ) where

import Lambda.Prelude

import Data.Foldable (elem)
import Data.String.CodeUnits (toCharArray)

type Flags =
  { sharing :: Boolean
  , loading :: Boolean
  }

none :: Flags
none =
  { sharing: false
  , loading: false
  }

parse :: String -> Maybe Flags
parse = Just <<< set <<< toCharArray
 where
  set cs =
    { sharing: 's' `elem` cs
    , loading: 'l' `elem` cs
    }
