module Components.ParseError
  ( component
  ) where

import Lambda.Prelude

import Lambda.Language.Parse (ParseError, formatParseError)
import React.Basic (JSX, fragment)
import React.Basic.DOM as R

type Props =
  { input :: String
  , error :: ParseError
  }

component :: Props -> JSX
component { input, error } = fragment
  [ R.p_ [R.text message]
  , R.p
    { className: "preformatted"
    , children:
      [ R.text $ source <> "\n" <> caret
      ]
    }
  ]
 where
  {message, source, caret} = formatParseError input error
