{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "lambda-machine"
, dependencies =
  [ "aff"
  , "arrays"
  , "bifunctors"
  , "control"
  , "debug"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
  , "integers"
  , "lists"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "parsing"
  , "partial"
  , "prelude"
  , "psci-support"
  , "quickcheck"
  , "react-basic"
  , "react-basic-dom"
  , "react-basic-hooks"
  , "safe-coerce"
  , "simple-json"
  , "spec"
  , "spec-discovery"
  , "spec-quickcheck"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "unfoldable"
  , "unordered-collections"
  , "web-dom"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
