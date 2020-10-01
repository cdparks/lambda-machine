{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "lambda-machine"
, dependencies =
  [ "arrays"
  , "console"
  , "control"
  , "effect"
  , "foldable-traversable"
  , "generics-rep"
  , "unordered-collections"
  , "maybe"
  , "ordered-collections"
  , "parsing"
  , "prelude"
  , "psci-support"
  , "react-basic"
  , "react-basic-hooks"
  , "react-basic-dom"
  , "strings"
  , "transformers"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
