module Components.Help
  ( component
  ) where

import Lambda.Prelude

import React.Basic (JSX, fragment)
import React.Basic.DOM as R

component :: {} -> JSX
component _ = fragment $ toJSX <$> tutorial

data Markup
  = Header String
  | Para (Array Leaf)
  | Code (Array String)
  | Comments (Array Comment)

data Leaf
  = Text String
  | Bold String
  | Mono String
  | Link String String

data Comment
  = Comment String (Array Leaf)

-- | Convert `Markup` to styled `JSX`
toJSX :: Markup -> JSX
toJSX = case _ of
  Header t ->
    R.h3_ [R.text t]
  Para ls ->
    R.p
      { children: formatLeaf <$> ls
      }
  Code cs ->
    R.p
      { className: "preformatted"
      , children: formatCode <$> cs
      }
  Comments as ->
    R.table_ [R.tbody_ $ formatComment <$> as]
 where
  formatLeaf :: Leaf -> JSX
  formatLeaf = case _ of
    Text t ->
      R.text t
    Bold t ->
      R.b_ [R.text t]
    Link url t ->
      R.a
        { href: "https://" <> url
        , children: [R.text t]
        }
    Mono t ->
      R.span
        { className: "monospace-font"
        , children: [R.text t]
        }

  formatCode :: String -> JSX
  formatCode code = R.text $ "  " <> code <> "\n"

  formatComment :: Comment -> JSX
  formatComment (Comment t cs) =
    R.tr_
      [ R.td_
        [ R.span
          { className: "preformatted"
          , children: [formatCode t]
          }
        ]
      , R.td
        { className: "comment"
        , children: formatLeaf <$> cs
        }
      ]

tutorial :: Array Markup
tutorial =
  [ Header "Hello!"
  , Para
    [ Text "Welcome to Lambda Machine, a tool for stepping through expressions in the untyped lambda calculus."
    ]
  , Header "Expressions"
  , Para
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
    , Text ". We'll use "
    , Bold "\\"
    , Text " since it's easier to type. Here's an expression that represents a function that accepts one argument and returns it immediately."
    ]
  , Code
    [ "\\x. x"
    ]
  , Para
    [ Text "We call this function "
    , Bold "identity"
    , Text "."
    ]
  , Para
    [ Text "Here's a function that returns its first argument after throwing away its second argument."
    ]
  , Code
    [ "\\x. \\y. x"
    ]
  , Para
    [ Text "We usually call this function "
    , Bold "const"
    , Text "."
    ]
  , Para
    [ Text "Note that the expression above is a function that returns another function."
    , Text " For convenience we can write it as a single lambda with two arguments."
    ]
  , Code
    [ "\\x y. x"
    ]
  , Para
    [ Text "We'll use the multi-argument form instead of manually nesting lambdas from now on."
    ]
  , Para
    [ Text "We can apply "
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
    [ Text "Specifically, this expression reduces as follows."
    ]
  , Comments
    [ Comment
      "(\\x y. x) (\\a. a) (\\b. b)"
      [ Text "Replace "
      , Mono "x"
      , Text " with "
      , Mono "(\\a. a)"
      , Text " in "
      , Mono "(\\x y. x)"
      ]
    , Comment
      "(\\y. \\a. a) (\\b. b)"
      [ Text "Replace "
      , Mono "y"
      , Text " with "
      , Mono "(\\b. b)"
      , Text " in "
      , Mono "(\\y. \\a. a)"
      ]
    , Comment "\\a. a" []
    ]
  , Header "Definitions"
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
    [ Text "Now we can refer to them by name."
    ]
  , Code
    [ "const identity identity"
    ]
  , Para
    [ Text "A name is reduced by replacing it with the expression on the right-hand-side of its definition."
    ]
  , Comments
    [ Comment
      "const identity identity"
      [ Text "Replace "
      , Mono "const"
      , Text " with "
      , Mono "\\x. \\y. x"
      ]
    , Comment
      "(\\x. \\y. x) identity identity"
      [ Text "Replace "
      , Mono "x"
      , Text " with "
      , Mono "identity"
      , Text " in "
      , Mono "(\\x. \\y. x)"
      ]
    , Comment
      "(\\y. identity) identity"
      [ Text "Replace "
      , Mono "y"
      , Text " with "
      , Mono "identity"
      , Text " in "
      , Mono "(\\y. identity)"
      ]
    , Comment
      "identity"
      [ Text "Replace "
      , Mono "identity"
      , Text " with "
      , Mono "\\x. x"
      ]
    , Comment "\\x. x" []
    ]
  , Header "Syntactic Sugar"
  , Para
    [ Text "Lambda Machine can parse natural numbers. A natural number "
    , Bold "n"
    , Text " is parsed as a function that applies "
    , Bold "s"
    , Text " to "
    , Bold "z n"
    , Text " times."
    ]
  , Code
    [ "0 -> \\s. \\z. z"
    , "1 -> \\s. \\z. s z"
    , "2 -> \\s. \\z. s (s z)"
    , "3 -> \\s. \\z. s (s (s z))"
    , "4 -> \\s. \\z. s (s (s (s z)))"
    ]
  , Para
    [ Text "You can read more about this encoding "
    , Link "en.wikipedia.org/wiki/Church_encoding" "here"
    , Text "."
    ]
  , Para
    [ Text "Lambda Machine can also parse lists."
    , Text " A list is parsed as a right fold over the elements using "
    , Bold "cons"
    , Text " and "
    , Bold "nil"
    , Text "."
    ]
  , Code
    [ "[a] -> \\cons. \\nil. cons a nil"
    , "[a, b] -> \\cons. \\nil. cons a (cons b nil)"
    , "[a, b, c] -> \\cons. \\nil. cons a (cons b (cons c nil))"
    ]
  , Para
    [ Text "This works with natural numbers as well."
    ]
  , Code
    [ "[1] -> \\cons. \\nil. cons (\\s. \\z. s z) nil"
    , "[1, 2] -> \\cons. \\nil. cons (\\s. \\z. s z) (cons (\\s. \\z. s (s z)) nil)"
    ]
  , Header "Bye!"
  , Para
    [ Text "Have fun and "
    , Link "github.com/cdparks/lambda-machine" "let me know if you find this useful"
    , Text "!"
    ]
  ]
