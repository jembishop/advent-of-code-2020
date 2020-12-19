{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "array-views"
  , "bifunctors"
  , "bigints"
  , "console"
  , "debug"
  , "effect"
  , "errors"
  , "foldable-traversable"
  , "generics-rep"
  , "lists"
  , "longs"
  , "monad-loops"
  , "node-fs-aff"
  , "ordered-collections"
  , "parsing"
  , "parsing-repetition"
  , "psci-support"
  , "strings"
  , "stringutils"
  , "tuples"
  , "unordered-collections"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
