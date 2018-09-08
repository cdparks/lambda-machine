module Components.Help
  ( component
  ) where

import Prelude

import React.Basic as React
import React.Basic.DOM as R

component :: React.Component {}
component =
  React.stateless {displayName: "Help", render}
 where
  render _ = React.fragment $ toJSX <$> tutorial

data Markup
  = Para (Array Leaf)
  | Code (Array String)

data Leaf
  = Text String
  | Bold String
  | MailTo String

toJSX :: Markup -> React.JSX
toJSX = case _ of
  Para ls ->
    R.p
      { children: formatLeaf <$> ls
      }
  Code cs ->
    R.p
      { className: "preformatted"
      , children: formatCode <$> cs
      }
 where
  formatLeaf :: Leaf -> React.JSX
  formatLeaf = case _ of
    Text t ->
      R.text t
    Bold t ->
      R.b_ [R.text t]
    MailTo t ->
      R.a
        { href: "mailto:christopher.daniel.parks@gmail.com"
        , children: [R.text t]
        }

  formatCode :: String -> React.JSX
  formatCode code = R.text $ "  " <> code <> "\n"

tutorial :: Array Markup
tutorial =
  [ Para
    [ Text "Lambda Machine accepts "
    , Bold "expressions"
    , Text " and "
    , Bold "definitions"
    , Text ". An "
    , Bold "expression"
    , Text " consists of variables, applications, and lambdas. Lambdas can be written with a backslash "
    , Bold "\\"
    , Text " or the Greek letter "
    , Bold "λ"
    , Text ", but we'll use "
    , Bold "\\"
    , Text " since it's easier to type. Here's an expression that represents a function that accepts one argument and returns it immediately."
    ]
  , Code
    [ "\\x. x"
    ]
  , Para
    [ Text "We call this function "
    , Bold "identity"
    , Text ". Here's a function that returns its first argument after throwing away its second argument."
    ]
  , Code
    [ "\\x. \\y. x"
    ]
  , Para
    [ Text "We usually call this function "
    , Bold "const"
    , Text ". Note that the function above is really a function that returns another function."
    , Text " For convenience we can write it as a single lambda with two arguments, but it means the same thing."
    ]
  , Code
    [ "\\x y. x"
    ]
  , Para
    [ Text "Going forward, we'll use the multi-argument form instead of manually nesting lambdas. We can apply "
    , Bold "const"
    , Text " to "
    , Bold "identity"
    , Text " twice using "
    , Bold "juxtaposition"
    , Text ", which is a fancy word for \"putting things next to one another\"."
    ]
  , Code
    [ "(\\x y. x) (\\x. x) (\\x. x)"
    ]
  , Para
    [ Text "Even if we rename the arguments to our "
    , Bold "identity"
    , Text " functions, they still do the same thing."
    ]
  , Code
    [ "(\\x y. x) (\\a. a) (\\b. b)"
    ]
  , Para
    [ Text "Specifically, this expression would reduce as follows"
    ]
  , Code
    [ "(\\x y. x) (\\a. a) (\\b. b)"
    , "(\\y. \\a. a) (\\b. b)"
    , "\\a. a"
    ]
  , Para
    [ Text "To avoid repeating ourselves, we can enter a "
    , Bold "definition"
    , Text ". A "
    , Bold "definition"
    , Text " consists of a name, zero or more arguments, an equals sign, and an "
    , Bold "expression"
    , Text ". Let's make definitions for "
    , Bold "identity"
    , Text " and "
    , Bold "const"
    , Text "."
    ]
  , Code
    [ "identity = \\x. x"
    , "const = \\x y. x"
    ]
  , Para
    [ Text "We can also write these like so."
    ]
  , Code
    [ "identity x = x"
    , "const x y = x"
    ]
  , Para
    [ Text "Once we've defined them, we can refer to them by name."
    ]
  , Code
    [ "const identity identity"
    ]
  , Para
    [ Text "We reduce names by replacing them with their definition."
    ]
  , Code
    [ "const identity identity"
    , "(\\x. \\y. x) identity identity"
    , "(\\y. identity) identity"
    , "identity"
    , "\\x. x"
    ]
  , Para
    [ Text "Have fun and "
    , MailTo "let me know if you find this useful"
    , Text "!"
    ]
  ]
