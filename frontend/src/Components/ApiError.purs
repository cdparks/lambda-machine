module Components.ApiError
  ( component
  ) where

import Components.Markup

import Lambda.Api (Error(..))
import Lambda.Prelude (Maybe(..), (<>), show, ($), (<<<))
import Lambda.Language.Pretty (pretty, Rep(..), toString)

component :: { error :: Error } -> JSX
component = markup <<< explain <<< _.error

explain :: Error -> Array Markup
explain = case _ of
  BadSnapshot error ->
    [ para
      [ text "Something went wrong while processing a snapshot:"
      ]
    , code [toString $ pretty Raw error]
    , para $ bug Definitely
    ]
  ParseError error ->
    [ para
      [ text "A response from the API failed to parse:"
      ]
    , code [error]
    , para $ bug Probably
    ]
  Missing code ->
    [ para $
      [ text "The snapshot identified by "
      , text $ show code
      , text " doesn't appear to exist anymore. "
      ] <> bug Maybe
    ]
  HttpError (Just error) ->
    [ para
      [ text "Something went wrong while communicating with the API:"
      ]
    , code [error]
    , para $ bug Probably
    ]
  HttpError Nothing ->
    [ para $
      [ text "Something went wrong while communicating with the API. "
      ] <> bug Probably
    ]

bug :: Likelihood -> Array Leaf
bug = case _ of
  Definitely ->
    [ text "This is definitely a bug. If you have time to "
    , fileAnIssue
    , text ", I'd really appreciate it!"
    ]
  Probably ->
    [ text "This is probably a bug. If you have time to "
    , fileAnIssue
    , text ", I'd really appreciate it!"
    ]
  Maybe ->
    [ text "If you think this looks like a bug, feel free to "
    , fileAnIssue
    , text ". Thanks!"
    ]
 where
  fileAnIssue = link
    { this: "file an issue"
    , to: "github.com/cdparks/lambda-machine/issues"
    }

data Likelihood
  = Definitely
  | Probably
  | Maybe
