{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "datetime-iso"
, dependencies =
    [ "console"
    , "datetime"
    , "effect"
    , "foreign-generic"
    , "newtype"
    , "parsing"
    , "psci-support"
    , "spec"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
