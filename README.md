# purescript-optlicative

An applicative-style CLI option parsing lib with accumulative errors.

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

Note that none of these will fail.

### Custom data-types

If we have a way of reading values from a `String` (specifically a function `f` of
type `Foreign -> F a`) then we can use `optF f` to read such a value. Any errors
in the `F` monad get turned into `OptErrors` in the `Optlicative` functor.

Example:

```purescript
readTuple :: Foreign -> F (Tuple Int Int)

optTuple :: Optlicative (Tuple Int Int)
optTuple = optF readTuple "point" (Just "Points must be in the form '(x,y)'")
```

Then the option `--point (3,5)` will not error if and only if
`readTuple (toForeign "(3,5)")` does not error.

### Running the parser

```
parse :: forall a e r. Preferences a e r -> Optlicative a -> Eff (process :: PROCESS | e) r
```

`Preferences a e r` is a record:

```purescript
{ errorOnUnrecognizedOpts :: Boolean
, onError :: List OptError -> Eff (process :: PROCESS | e) r
, onSuccess :: a -> Eff (process :: PROCESS | e) r
, helpMsg :: Maybe String
}
```

A `defaultPreferences` is available, though you *should* change the `onSuccess`
field yourself, as the default is to discard any value.

The `errorOnUnrecognizedOpts` field indicates whether an error should be generated
if a user passes in an option that isn't recognized by the parser. Note that,
since flags cannot generate an error, a flag that isn't recognized won't generate
an error either.

The `helpMsg` field will print a given message in case of any error.

Also see the `test/` folder.

## Unsupported/future features

* options with more than one argument (workaround: separate multiple arguments by a space, wrap the group in a pair of double quotes)
* passthrough options (as in `program --program-opt -- --passthrough-opt`)
* commands (example: `pulp build --help`, `build` is a command while `--help` is an option)
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
      "validation",
      "node-process",
      "eff",
      "console",
      "eff",
      "prelude"
    ],
    "repo": "https://github.com/thimoteus/purescript-optlicative.git",
    "version": "v0.1.0"
  }
```

Alternatively: use https://github.com/thimoteus/package-sets.git

2. `> psc-package install optlicative`