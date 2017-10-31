module Node.Optlicative
  ( throw
  -- , help, (<?>)
  , boolean
  , flag
  , string
  , int
  , float
  , optional
  , withDefault
  , withDefaultM
  , optF
  , optForeign
  , parse
  , renderErrors
  , defaultPreferences
  , module Types
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, error)
import Control.Monad.Except (runExcept)
import Data.Array (intercalate)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foreign (F, Foreign, toForeign)
import Data.Int (fromNumber)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (unwrap)
import Data.Validation.Semigroup (invalid, isValid, unV)
import Global (isNaN, readFloat)
import Node.Optlicative.Internal (defaultError, except, findDash, findFlag, findHyphen, initialize, multipleErrorsToOptErrors, removeDash, removeFlag, removeHyphen, throwSingleError, unrecognizedOpts)
import Node.Optlicative.Types (ErrorMsg, OptError(..), Optlicative(..), Preferences, renderOptError)
import Node.Optlicative.Types (OptError(..), ErrorMsg, Optlicative(..), Value, Preferences) as Types
import Node.Process (PROCESS, argv)

-- | A combinator that always fails.
throw :: forall a. OptError -> Optlicative a
throw e = Optlicative (except e)

-- -- | A combinator for giving a custom error. For example:
-- -- | parsePerson = `{name: _, age: _} <$> string "name" Nothing <*> int "age" Nothing
-- -- | <?> "Usage: program --name <string> --age <int>"`
-- help :: forall a. Optlicative a -> ErrorMsg -> Optlicative a
-- help (Optlicative o) msg = Optlicative \ s ->
--   let {state, val} = o s
--       check = not (isValid val)
--       err = Custom msg
--   in  { state
--       , val: if check then invalid (List.singleton err) <*> val else val
--       }

-- infixl 4 help as <?>

-- | Check whether a boolean value appears as an option. This combinator cannot
-- | fail, as its absence is interpreted as `false`. The first argument is the
-- | expected name, the second is an optional character for single-hyphen style:
-- | For example, `boolean "optimize" (Just 'O')` will parse both `--optimize` and
-- | `-O`.
flag :: String -> Maybe Char -> Optlicative Boolean
flag name mc = Optlicative \ state ->
  if findFlag name state
    then {state: removeFlag name state, val: pure true}
    else case mc of
      Just c -> if findHyphen c state
        then {state: removeHyphen c state, val: pure true}
        else {state, val: pure false}
      _ -> {state, val: pure false}

-- | A version of `boolean` where you only want to use the single-hyphen style.
boolean :: Char -> Optlicative Boolean
boolean = flag mempty <<< Just

-- | Check whether a string appears as an option. The first argument is the
-- | expected name, the second is a custom error message if the option does not
-- | appear. A default error message is provided if this argument is `Nothing`.
string :: String -> Maybe ErrorMsg -> Optlicative String
string name msg = Optlicative \ state -> case findDash name state of
  Just val -> {state: removeDash name state, val: pure val}
  _ -> except (maybe (defaultError MissingOpt name mempty) MissingOpt msg) state

-- | Check whether an integer appears as an option. Arguments are the same as for
-- | `string`.
int :: String -> Maybe ErrorMsg -> Optlicative Int
int name msg = Optlicative \ state -> case findDash name state of
  Just n -> case fromNumber (readFloat n) of
    Just i -> {state: removeDash name state, val: pure i}
    _ -> except
      (maybe (defaultError TypeError name "int") TypeError msg)
      (removeDash name state)
  _ -> except
    (maybe (defaultError MissingOpt name mempty) MissingOpt msg)
    state

-- | Check whether a float appears as an option. Arguments are the same as for
-- | `string`. Note that numbers without decimal points will still parse as floats.
float :: String -> Maybe ErrorMsg -> Optlicative Number
float name msg = Optlicative \ state -> case findDash name state of
  Just n -> if isNaN (readFloat n)
    then except
      (maybe (defaultError TypeError name "float") TypeError msg)
      (removeDash name state)
    else {state: removeDash name state, val: pure (readFloat n)}
  _ -> except (maybe (defaultError MissingOpt name mempty) MissingOpt msg) state

-- | Instead of failing, turns an optlicative parser into one that always succeeds
-- | but may do so with `Nothing` if no such option is found.
optional :: forall a. Optlicative a -> Optlicative (Maybe a)
optional (Optlicative o) = Optlicative \ s ->
  let {state, val} = o s
  in  {state, val: if isValid val then Just <$> val else pure Nothing}

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
optForeign name msg = Optlicative \ state -> case findDash name state of
  Just f -> {state: removeDash name state, val: pure (toForeign f)}
  _ -> except (maybe (defaultError MissingOpt name mempty) MissingOpt msg) state

-- | Given a deserializing function, returns the value if no errors were encountered
-- | during deserialization. If there were errors, they are turned into `OptError`s.
optF :: forall a. (Foreign -> F a) -> String -> Maybe ErrorMsg -> Optlicative a
optF read name msg = Optlicative \ state -> case findDash name state of
  Just f -> case runExcept (read (toForeign f)) of
    Right v -> { state: removeDash name state, val: pure v }
    Left errs -> {state, val: invalid (multipleErrorsToOptErrors errs)}
  Nothing -> except (maybe (defaultError MissingOpt name mempty) MissingOpt msg) state

-- | A convenience function for nicely printing error messages. Used in 
-- | `defaultPreferences` to print any errors to the console.
renderErrors :: List OptError -> String
renderErrors = intercalate "\n" <<< map renderOptError

-- | A `Preferences` that prints errors to the console, errors on unrecognized
-- | (unparsed) options, and discards any results. You should change `onSuccess`
-- | to something more interesting:
-- | `myPrefs = defaultPreferences {onSuccess = runConfig}`
defaultPreferences :: forall e a. Preferences a (console :: CONSOLE | e) Unit
defaultPreferences =
  { errorOnUnrecognizedOpts: true
  , onError: error <<< renderErrors
  , onSuccess: const (pure unit)
  , helpMsg: Nothing
  }

-- | Use this to run an `Optlicative`. 
parse
  :: forall a e r
   . Preferences a e r
  -> Optlicative a
  -> Eff (process :: PROCESS | e) r
parse prefs o = do
  args <- Array.drop 2 <$> argv
  let
    {state, val} = unwrap o $ initialize $ Array.toUnfoldable args
    h = state.hyphen
    d = state.dash
    f = state.flags
    check =
      prefs.errorOnUnrecognizedOpts &&
      not (List.null ((unit <$ h) <> (unit <$ d) <> (unit <$ f)))
    finalVal = case prefs.helpMsg, check, isValid val of
      Just msg, true, true ->
        unrecognizedOpts state <*>
        throwSingleError (Custom msg)
      Just msg, true, _ ->
        unrecognizedOpts state <*>
        throwSingleError (Custom msg) <*>
        val
      Just msg, false, true -> val
      Just msg, false, _ -> throwSingleError (Custom msg) <*> val
      _, true, _ -> unrecognizedOpts state <*> val
      _, _, _ -> val
  unV prefs.onError prefs.onSuccess finalVal