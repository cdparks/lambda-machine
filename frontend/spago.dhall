{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "lambda-machine"
, dependencies =
  [ "aff"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "bifunctors"
  , "control"
  , "debug"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "identity"
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
  , "simple-ajax"
  , "spec"
  , "spec-discovery"
  , "spec-quickcheck"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "unfoldable"
  , "unordered-collections"
  , "variant"
  , "web-dom"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
