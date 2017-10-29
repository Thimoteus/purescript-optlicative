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
import Node.Optlicative.Types (Error(..), ErrorMsg, Optlicative(..), Value)
import Node.Optlicative.Types (Error(..), ErrorMsg, Optlicative(..), Value) as Types
import Node.Process (PROCESS, argv)

throw :: forall a. Error -> Optlicative a
throw e = Optlicative (except e)

usage :: forall a. Optlicative a -> ErrorMsg -> Optlicative a
usage (Optlicative o) msg = Optlicative \ s -> -- throw <<< Custom
  let {state, val} = o s
      check = not (isValid val)
      err = Custom msg
  in  { state
      , val: if check then invalid (List.singleton err) <*> val else val
      }

infixl 4 usage as <?>

boolean :: String -> Maybe Char -> Optlicative Boolean
boolean name mc = Optlicative \ state ->
  if findFlag name state
    then {state: removeFlag name state, val: pure true}
    else case mc of
      Just c -> if findHyphen c state
        then {state: removeHyphen c state, val: pure true}
        else {state, val: pure false}
      _ -> {state, val: pure false}

flag :: String -> Maybe Char -> Optlicative Boolean
flag = boolean

string :: String -> Maybe ErrorMsg -> Optlicative String
string name msg = Optlicative \ state -> case findDash name state of
  Just val -> {state: removeDash name state, val: pure val}
  _ -> except (maybe (defaultError MissingOpt name "") MissingOpt msg) state

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

float :: String -> Maybe ErrorMsg -> Optlicative Number
float name msg = Optlicative \ state -> case findDash name state of
  Just n -> if isNaN (readFloat n)
    then except (maybe (defaultError TypeError name "float") TypeError msg) state
    else {state: removeDash name state, val: pure (readFloat n)}
  _ -> except (maybe (defaultError MissingOpt name "") MissingOpt msg) state

renderErrors :: List Error -> String
renderErrors = intercalate "\n" <<< map show

parse
  :: forall a e
   . Optlicative a
  -> Eff (process :: PROCESS | e) (Value a)
parse o = do
  args <- Array.drop 2 <$> argv
  pure <<< _.val $ unwrap o $ initialize $ Array.toUnfoldable args