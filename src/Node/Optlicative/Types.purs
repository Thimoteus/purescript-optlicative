module Node.Optlicative.Types where

import Prelude

import Data.List (List)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Validation.Semigroup (V)

newtype Optlicative a = Optlicative (OptState -> Result a)

derive instance newtypeOptlicative :: Newtype (Optlicative a) _

derive instance functorOptlicative :: Functor Optlicative

instance applyOptlicative :: Apply Optlicative where
  apply (Optlicative f) (Optlicative a) = Optlicative \ s ->
    let
      r1 = f s
      a' = a r1.state
      val = r1.val <*> a'.val
      state = a'.state
    in
      {state, val}

instance applicativeOptlicative :: Applicative Optlicative where
  pure a = Optlicative \ state -> {state, val: pure a}

data OptError
  = TypeError ErrorMsg
  | MissingOpt ErrorMsg
  | MissingArg ErrorMsg
  | UnrecognizedOpt String
  | UnrecognizedCommand String
  | Custom ErrorMsg

renderOptError :: OptError -> String
renderOptError = case _ of
  TypeError msg -> "Type error: " <> msg
  MissingOpt msg -> "Missing option: " <> msg
  MissingArg msg -> "Missing argument: " <> msg
  UnrecognizedOpt msg -> "Unrecognized option: " <> msg
  UnrecognizedCommand msg -> "Unrecognized command: " <> msg
  Custom msg -> msg

instance showError :: Show OptError where
  show (TypeError msg) = "(TypeError " <> show msg <> ")"
  show (MissingOpt msg) = "(MissingOpt " <> show msg <> ")"
  show (MissingArg msg) = "(MissingArg " <> show msg <> ")"
  show (UnrecognizedOpt msg) = "(UnrecognizedOpt " <> show msg <> ")"
  show (UnrecognizedCommand msg) = "(UnrecognizedCommand " <> show msg <> ")"
  show (Custom msg) = "(Custom " <> show msg <> ")"

type Value a = V (List OptError) a

type OptState = {unparsed :: List String}

type Result a = {state :: OptState, val :: Value a}

type ErrorMsg = String

type Preferences a =
  { errorOnUnrecognizedOpts :: Boolean
  , usage :: Maybe String
  , globalOpts :: Optlicative a
  }