module Node.Optlicative
  ( throw
  , usage, (<?>)
  , boolean
  , flag
  , string
  , int
  , float
  , parse
  , renderErrors
  , module Types
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Array (intercalate)
import Data.Array as Array
import Data.Int (fromNumber)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(Just), maybe)
import Data.Newtype (unwrap)
import Data.Validation.Semigroup (invalid, isValid)
import Global (isNaN, readFloat)
import Node.Optlicative.Internal (defaultError, except, findDash, findFlag, findHyphen, initialize, removeDash, removeFlag, removeHyphen)
import Node.Optlicative.Types (OptError(..), ErrorMsg, Optlicative(..), Value)
import Node.Optlicative.Types (OptError(..), ErrorMsg, Optlicative(..), Value) as Types
import Node.Process (PROCESS, argv)

-- | A combinator that always fails.
throw :: forall a. OptError -> Optlicative a
throw e = Optlicative (except e)

-- | A combinator for giving a custom error. For example:
-- | parsePerson = `{name: _, age: _} <$> string "name" Nothing <*> int "age" Nothing
-- | <?> "Usage: program --name <string> --age <int>"
usage :: forall a. Optlicative a -> ErrorMsg -> Optlicative a
usage (Optlicative o) msg = Optlicative \ s -> -- throw <<< Custom
  let {state, val} = o s
      check = not (isValid val)
      err = Custom msg
  in  { state
      , val: if check then invalid (List.singleton err) <*> val else val
      }

infixl 4 usage as <?>

-- | Check whether a boolean value appears as an option. This combinator cannot
-- | fail, as its absence is interpreted as `false`. The first argument is the
-- | expected name, the second is an optional character for single-hyphen style:
-- | For example, `boolean "optimize" (Just 'O')` will parse both `--optimize` and
-- | `-O`.
boolean :: String -> Maybe Char -> Optlicative Boolean
boolean name mc = Optlicative \ state ->
  if findFlag name state
    then {state: removeFlag name state, val: pure true}
    else case mc of
      Just c -> if findHyphen c state
        then {state: removeHyphen c state, val: pure true}
        else {state, val: pure false}
      _ -> {state, val: pure false}

-- | A version of `boolean` where you only want to use the single-hyphen style.
flag :: Char -> Optlicative Boolean
flag = boolean "" <<< Just

-- | Check whether a string appears as an option. The first argument is the
-- | expected name, the second is a custom error message if the option does not
-- | appear. A default error message is provided if this argument is `Nothing`.
string :: String -> Maybe ErrorMsg -> Optlicative String
string name msg = Optlicative \ state -> case findDash name state of
  Just val -> {state: removeDash name state, val: pure val}
  _ -> except (maybe (defaultError MissingOpt name "") MissingOpt msg) state

-- | Check whether an integer appears as an option. Arguments are the same as for
-- | `string`.
int :: String -> Maybe ErrorMsg -> Optlicative Int
int name msg = Optlicative \ state -> case findDash name state of
  Just n -> case fromNumber (readFloat n) of
    Just i -> {state: removeDash name state, val: pure i}
    _ -> except
      (maybe (defaultError TypeError name "int") TypeError msg)
      state
  _ -> except
    (maybe (defaultError MissingOpt name "") MissingOpt msg)
    state

-- | Check whether a float appears as an option. Arguments are the same as for
-- | `string`. Note that numbers without decimal points will still parse as floats.
float :: String -> Maybe ErrorMsg -> Optlicative Number
float name msg = Optlicative \ state -> case findDash name state of
  Just n -> if isNaN (readFloat n)
    then except (maybe (defaultError TypeError name "float") TypeError msg) state
    else {state: removeDash name state, val: pure (readFloat n)}
  _ -> except (maybe (defaultError MissingOpt name "") MissingOpt msg) state

-- | A convenience function for nicely printing error messages. For example:
-- | `unV (log <<< renderErrors) doSomething =<< parse myOptlicativeParser`.
renderErrors :: List OptError -> String
renderErrors = intercalate "\n" <<< map show

-- | Use this to run an `Optlicative`. The resulting `Value a` is a synonym for
-- | `V (List Error) a`, so you will need to use `unV` to handle any possible
-- | errors.
parse
  :: forall a e
   . Optlicative a
  -> Eff (process :: PROCESS | e) (Value a)
parse o = do
  args <- Array.drop 2 <$> argv
  pure <<< _.val $ unwrap o $ initialize $ Array.toUnfoldable args