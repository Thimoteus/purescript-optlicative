module Node.Optlicative.Types where

import Prelude

import Data.List (List)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)
import Data.Validation.Semigroup (V)

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

data Error
  = TypeError ErrorMsg
  | MissingOpt ErrorMsg
  | Custom ErrorMsg

instance showError :: Show Error where
  show (TypeError msg) = "Type error: " <> msg
  show (MissingOpt msg) = "Missing option: " <> msg
  show (Custom msg) = msg

type Value a = V (List Error) a

type OptState =
  { hyphen :: List Char -- i.e. program -abc
  , dash :: List (Tuple String String) -- i.e. program --method POST
  , flags :: List String } -- i.e. program --silent

type Result a = {state :: OptState, val :: Value a}

type ErrorMsg = String