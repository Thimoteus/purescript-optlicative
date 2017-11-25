# purescript-optlicative

An applicative-style CLI option parsing lib with accumulative errors and type-based
command parsing.

## Usage by example

Let's say we have a CLI program called `p`, and we want it to accept a
flag that determines whether its output will be colored or not.

We want users to express this intention by writing `p --color`.

Somewhere in our code we have a `Config` type that represents options
passed in:

```purescript
type Config r = {color :: Boolean | r}
```

To parse this, we use the `flag` combinator:

```purescript
parseConfig :: Optlicative (Config ())
parseConfig = {color: _} <$> flag "color" Nothing
```

If we want to extend `Config` to include an `--output` option that takes a filename argument, that's easy too:

```purescript
type Config' r = Config (output :: String | r)

parseConfig' :: Optlicative (Config' ())
parseConfig' = {color: _, output: _}
  <$> flag "color" Nothing
  <*> string "output" Nothing
```

Suddenly we think of several more boolean flags we want to support:

```purescript
type Config'' r = Config' (humanReadable :: Boolean, metricUnits :: Boolean | r)

parseConfig'' :: Optlicative (Config'' ())
parseConfig'' = {color: _, output: _, humanReadable: _, metricUnits: _}
  <$> flag "color" Nothing
  <*> string "output" Nothing
  <*> flag "human-readable" Nothing
  <*> flag "metric-units" Nothing
```

But now if we want users to use every one of these flags, we require them to write something like `p --color --human-readable --metric-units --output "./output.txt"`. That's way too long!

If we want to allow single-hyphen, single-character options we just change a few `Nothing`'s:

```purescript
parseConfig2 :: Optlicative (Config'' ())
parseConfig2 = {color: _, humanReadable: _, metricUnits: _, output: _}
  <$> flag "color" (Just 'c')
  <*> flag "human-readable" (Just 'H')
  <*> flag "metric-units" (Just 'm')
  <*> string "output" Nothing
```

Now our users can write `p -cHm --output "./output.txt"`. Much better!

### Error messages

By default, if the `--output` option is missing the following error will be
generated: `"Missing option: Option 'output' is required."`

Our flags won't produce any error messages, since if a user doesn't supply a flag we assume they want it to be `false`.

But we can also change the error message, for example by changing our `--output` parser to `string "output" (Just "I need to know where to place my output!")`

Error messages are accumulated via the semigroup-based `V` applicative functor,
meaning that if the user gives input that causes multiple errors, each one can be
shown.

### Optional values

What if we want to provide a default output directory, and don't want to require
the user to always supply it? We can use `optional`, `withDefault` or `withDefaultM`:

```purescript
parseConfig4 :: Optlicative (Config'' ())
parseConfig4 = {color: _, _, humanReadable: _, metricUnits: _, output: _}
  <$> flag "color" (Just 'c')
  <*> flag "human-readable" (Just 'H')
  <*> flag "metric-units" (Just 'm')
  <*> withDefault "./output.txt" (string "output" Nothing)
```

Note that none of these three combinators will fail.

### Custom data-types

If we have a way of reading values from a `String` (specifically a function `f` of
type `String -> F a`) then we can use `optF f` to read such a value. Any errors
in the `F` monad get turned into `OptErrors` in the `Optlicative` functor.

Example:

```purescript
readTupleString :: String -> F (Tuple Int Int)

optTuple :: Optlicative (Tuple Int Int)
optTuple = optF readTupleString "point" (Just "Points must be in the form '(x,y)'")
```

Then the option `--point (3,5)` won't error if and only if
`readTupleString "(3,5)"` does not error.

### Options that accept multiple arguments

Again, assuming we have a function `read :: String -> F a` for some type `a`,
we can use `manyF read :: Int -> String -> Maybe ErrorMsg -> Optlicative (List a)`.

In this case, `Int` represents the number of arguments expected (none of which
may start with a hyphen character).

### Running the parser

```purescript
optlicate :: Constraints => Record optrow -> Preferences a -> Eff (process :: PROCESS | e) {cmd :: Maybe String, value :: Value a}
```

`Preferences` is a record:

```purescript
{ errorOnUnrecognizedOpts :: Boolean
, usage :: Maybe String
, globalOpts :: Optlicative a
}
```

A `defaultPreferences :: Preferences Void` is available.

The `errorOnUnrecognizedOpts` field indicates whether an error should be generated
if a user passes in an option that isn't recognized by the parser.

The `usage` field will print a given message in case of any error.

`globalOpts` is for options which don't match a given command; for more on commands
see the next section.

The `value` field has type `Value a`, which is a type synonym for
`V (List OptError) a`. This means you'll need to use `unV` from
`Data.Validation.Semigroup`, handling any possible errors, in order to have access
to the value of type `a`.

## Dealing with Commands

Let's take a closer look at the "Constraints" part of the `optlicate` type signature.
The actual signature starts like this:

```purescript
optlicate :: forall optrow a e. Commando optrow => Record optrow -> Preferences a -> ...
```

The important part is the `Commando` typeclass constraint. It applies only to
a certain class of rows -- similar to homogenous rows, but a bit more generalized
than what usually comes to mind. Let's look at an example:

```
type MyConfig =
  ( command :: Opt Config
    ( more :: Opt Config ()
    )
  , second :: Opt Config ()
  )
```

Note that this type has not only breadth but also depth. The `Opt` type is a
datatype around `Optlicative` but with extra type information in the second argument:
this is what allows us to nest commands, treating every possible command (and
associated options) as a tree-like structure (a record), where each node (field)
represents a pair of a command entered, and the options for that command.

For example, if the user had run `p command --help`, the parser would then recognize this, and match the `Optlicative Config` associated with the `command` command and
run it against the `--help` flag. 

Any command, if it exists, will be placed into the `cmd` field of the result --
if the program is used like `p command more`, then `cmd = Just "more"`.

Let's look at the first argument to `optlicate`. In our example case, we'd need a
value of type `Record MyConfig`. If we can construct a value for just one field,
we can construct them all. And those values are built using `Opt`, as suggested
by the definition of `MyConfig`:

```purescript
data Opt (a :: Type) (row :: # Type) = Opt (Optlicative a) (Record row)
```

`Opt`s are pairs of `Optlicative`s and a record, which allows us to continue
chaining new `Optlicative`s. With this in mind, we can construct what we want:

```purescript
myConfig :: Record MyConfig
myConfig =
  { command: Opt commandOptlicative
    { more: Opt moreOptlicative {}
    }
  , second: Opt secondOptlicative {}
  }
```

We can also use `endOpt` to get rid of those empty records if we wish:
`more: endOpt moreOptlicative`.

## More examples

See the `test/` folder.

## Unsupported/future features

* "Unsupported command" errors: when a command is given but does not match anything
* passthrough options (as in `program --program-opt -- --passthrough-opt`)
* use of single characters for options instead of just flags
* other things I haven't thought of

## Installation

* Using bower:

```
> bower install https://github.com/Thimoteus/purescript-optlicative.git
```

* Using psc-package:

1. Add it to a custom package set, for example:
```
  "optlicative": {
    "dependencies": [
      "record",
      "typelevel-prelude",
      "symbols",
      "validation",
      "node-process",
      "eff",
      "console",
      "prelude"
    ],
    "repo": "https://github.com/thimoteus/purescript-optlicative.git",
    "version": "v0.4.1"
  }
```

Alternatively: use https://github.com/thimoteus/package-sets.git

2. `> psc-package install optlicative`