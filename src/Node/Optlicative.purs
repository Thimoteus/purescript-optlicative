module Node.Optlicative
  ( throw
  , flag
  , string
  , int
  , float
  , optional
  , withDefault
  , withDefaultM
  , optF
  , optForeign
  , many
  , manyF
  , optlicate
  , defaultPreferences
  , renderErrors
  , logErrors
  , module Exports
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array (intercalate)
import Data.Either (Either(..))
import Data.Int (fromNumber)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Validation.Semigroup (invalid, isValid, toEither)
import Effect (Effect)
import Effect.Console (error)
import Foreign (F, Foreign, unsafeToForeign)
import Global (isNaN, readFloat)
import Node.Commando (class Commando)
import Node.Commando (class Commando, commando, Opt(..), endOpt) as Exports
import Node.Optlicative.Internal (ddash, ex, except, find, hasHyphen, multipleErrorsToOptErrors, parse, removeAtFor, removeAtForWhile, removeHyphen, startsDash)
import Node.Optlicative.Types (ErrorMsg, OptError(..), Optlicative(..), Preferences, Value, renderOptError)
import Node.Optlicative.Types (OptError(..), ErrorMsg, Optlicative(..), Value, Preferences) as Exports
import Node.Process (argv)

-- | A combinator that always fails.
throw :: forall a. OptError -> Optlicative a
throw e = Optlicative (except e)

-- | Check whether a boolean value appears as an option. This combinator cannot
-- | fail, as absence of its option is interpreted as `false`. The first argument
-- | is the expected name, the second is an optional character for single-hyphen
-- | style: For example, `boolean "optimize" (Just 'O')` will parse both
-- | `--optimize` and `-O`.
flag :: String -> Maybe Char -> Optlicative Boolean
flag name mc = Optlicative \ state -> case find ddash name state of
  Just i ->
    let
      {removed, rest} = removeAtFor i 0 state
    in
      {state: rest, val: pure true}
  _ -> case mc of
    Just c ->
      if hasHyphen c state
        then {state: removeHyphen c state, val: pure true}
        else {state, val: pure false}
    _ -> {state, val: pure false}

-- | Check whether a string appears as an option. The first argument is the
-- | expected name, the second is a custom error message if the option does not
-- | appear. A default error message is provided if this argument is `Nothing`.
string :: String -> Maybe ErrorMsg -> Optlicative String
string name msg = Optlicative \ state -> case find ddash name state of
  Just i ->
    let
      {removed, rest} = removeAtForWhile i 1 (not <<< startsDash) state
    in
      case List.head removed.unparsed of
        Just h -> {state: rest, val: pure h}
        _ -> ex name (show 1) MissingArg msg rest
  _ -> ex name mempty MissingOpt msg state

-- | Check whether an integer appears as an option. Arguments are the same as for
-- | `string`.
int :: String -> Maybe ErrorMsg -> Optlicative Int
int name msg = Optlicative \ state -> case find ddash name state of
  Just i ->
    let
      {removed, rest} = removeAtForWhile i 1 (not <<< startsDash) state
    in
      case List.head removed.unparsed of
        Just h -> case fromNumber (readFloat h) of
          Just n -> {state: rest, val: pure n}
          _ -> ex name "int" TypeError msg rest
        _ -> ex name (show 1) MissingArg msg rest
  _ -> ex name mempty MissingOpt msg state

-- | Check whether a float appears as an option. Arguments are the same as for
-- | `string`. Note that numbers without decimal points will still parse as floats.
float :: String -> Maybe ErrorMsg -> Optlicative Number
float name msg = Optlicative \ state -> case find ddash name state of
  Just i ->
    let
      {removed, rest} = removeAtForWhile i 1 (not <<< startsDash) state
    in
      case List.head removed.unparsed of
        Just h ->
          if isNaN (readFloat h)
            then ex name "float" TypeError msg rest
            else {state: rest, val: pure (readFloat h)}
        _ -> ex name (show 1) MissingArg msg rest
  _ -> ex name mempty MissingOpt msg state

-- | Instead of failing, turns an optlicative parser into one that always succeeds
-- | but may do so with `Nothing` if no such option is found.
-- | This is useful for `--help` flags in particular: Without this combinator,
-- | it's easy to make an `Optlicative` that gives an unhelpful `MissingOpt`
-- | error message when all the user did was try to find help text for a command.
optional :: forall a. Optlicative a -> Optlicative (Maybe a)
optional (Optlicative o) = Optlicative \ s ->
  let {state, val} = o s
  in  {state, val: if isValid val then Just <$> val else pure Nothing}

-- | Apply an `Optlicative` parser zero or more times, collecting the
-- | results in a `List`.
many :: forall a. Optlicative a -> Optlicative (List a)
many parser = Optlicative \optstate -> go parser optstate Nil
  where
    go (Optlicative o) s acc =
      let
        { state, val } = o s
      in 
        case toEither val of
           Left _  -> { state, val: pure (List.reverse acc) }
           Right v -> go parser state (v:acc)

-- | Instead of failing, turns an optlicative parser into one that always succeeds
-- | but may do so with the given default argument if no such option is found.
withDefault :: forall a. a -> Optlicative a -> Optlicative a
withDefault def (Optlicative o) = Optlicative \ s ->
  let {state, val} = o s
  in  {state, val: if isValid val then val else pure def}

-- | Like `withDefault` but the default value is `mempty`.
withDefaultM :: forall m. Monoid m => Optlicative m -> Optlicative m
withDefaultM = withDefault mempty

-- | Check whether something appears as an option, and coerce it to `Foreign` if
-- | it does. This can be used to parse a JSON argument, for example.
optForeign :: String -> Maybe ErrorMsg -> Optlicative Foreign
optForeign name msg = Optlicative \ state -> case find ddash name state of
  Just i ->
    let
      {removed, rest} = removeAtForWhile i 1 (not <<< startsDash) state
    in
      case List.head removed.unparsed of
        Just h -> {state: rest, val: pure (unsafeToForeign h)}
        _ -> ex name (show 1) MissingArg msg rest
  _ -> ex name mempty MissingOpt msg state

-- | Given a deserializing function, returns the value if no errors were encountered
-- | during deserialization. If there were errors, they are turned into `OptError`s.
optF :: forall a. (String -> F a) -> String -> Maybe ErrorMsg -> Optlicative a
optF read name msg = Optlicative \ state -> case find ddash name state of
  Just i ->
    let
      {removed, rest} = removeAtForWhile i 1 (not <<< startsDash) state
    in
      case List.head removed.unparsed of
        Just h -> case runExcept (read h) of
          Right v -> {state: rest, val: pure v}
          Left errs -> {state: rest, val: invalid (multipleErrorsToOptErrors errs)}
        _ -> ex name (show 1) MissingArg msg rest
  _ -> ex name mempty MissingOpt msg state

-- | Given a deserializing function and the number of args to parse, none of which
-- | may start with a '-' character, returns a list of parsed values.
manyF :: forall a. (String -> F a) -> Int -> String -> Maybe ErrorMsg -> Optlicative (List a)
manyF read len name msg = Optlicative \ state -> case find ddash name state of
  Just i ->
    let
      {removed, rest} = removeAtForWhile i len (not <<< startsDash) state
    in
      case runExcept (traverse read removed.unparsed) of
        Right vs -> {state: rest, val: pure vs}
        Left errs -> {state: rest, val: invalid (multipleErrorsToOptErrors errs)}
  _ -> ex name mempty MissingOpt msg state

-- | A convenience function for nicely printing error messages.
renderErrors :: List OptError -> String
renderErrors = intercalate "\n" <<< map renderOptError

logErrors :: List OptError -> Effect Unit
logErrors = error <<< renderErrors

-- | A `Preferences` that errors on unrecognized options, has no usage text,
-- | and uses an always-failing parser for global options.
defaultPreferences :: Preferences Void
defaultPreferences =
  { errorOnUnrecognizedOpts: true
  , usage: Nothing
  , globalOpts: throw (Custom "Error: defaultPreferences used.")
  }

-- | Use this to run an `Optlicative`.
optlicate
  :: forall optrow a
   . Commando optrow a
  => Record optrow
  -> Preferences a
  -> Effect {cmd :: Maybe String, value :: Value a}
optlicate rec prefs = parse rec prefs <$> argv
