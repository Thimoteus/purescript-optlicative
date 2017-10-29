module Node.Optlicative where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Array as Array
import Data.Int (fromNumber)
import Data.Maybe (Maybe(Just))
import Data.Newtype (class Newtype, unwrap)
import Global (isNaN, readFloat)
import Node.Optlicative.Internal (Error(..), OptState, Value, Result, except, findDash, findFlag, findHyphen, initialize, removeDash, removeFlag, removeHyphen)
import Node.Process (PROCESS, argv)

newtype Optlicative a = Optlicative (OptState -> Result a)

derive instance newtypeOptlicative :: Newtype (Optlicative a) _

derive instance functorOptlicative :: Functor Optlicative

instance applyOptlicative :: Apply Optlicative where
  apply (Optlicative f) (Optlicative a) = Optlicative \ s ->
    let r1 = f s
        a' = a r1.state
        val = r1.val <*> a'.val
        state = a'.state
    in  {state, val}

instance applicativeOptlicative :: Applicative Optlicative where
  pure a = Optlicative \ state -> {state, val: pure a}

throw :: forall a. Error -> Optlicative a
throw e = Optlicative (except e)

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

string :: String -> Optlicative String
string name = Optlicative \ state -> case findDash name state of
  Just val -> {state: removeDash name state, val: pure val}
  _ -> except Error state

int :: String -> Optlicative Int
int name = Optlicative \ state -> case findDash name state of
  Just n -> case fromNumber (readFloat n) of
    Just i -> {state: removeDash name state, val: pure i}
    _ -> except Error state
  _ -> except Error state

float :: String -> Optlicative Number
float name = Optlicative \ state -> case findDash name state of
  Just n -> if isNaN (readFloat n)
    then except Error state
    else {state: removeDash name state, val: pure (readFloat n)}
  _ -> except Error state

parse
  :: forall a e
   . Optlicative a
  -> Eff (process :: PROCESS | e) (Value a)
parse o = do
  args <- Array.drop 2 <$> argv
  pure <<< _.val $ unwrap o $ initialize $ Array.toUnfoldable args