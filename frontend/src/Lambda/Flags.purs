module Lambda.Flags
  ( Flags
  , none
  , parse
  , param
  ) where

import Lambda.Prelude

import Data.Array as Array
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

param :: Flags -> String
param { sharing, loading} = fold $ Array.catMaybes
  [ "&f=" <$ guard (sharing || loading)
  , "s" <$ guard sharing
  , "l" <$ guard loading
  ]
