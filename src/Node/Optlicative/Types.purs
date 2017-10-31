module Node.Optlicative.Types where

import Prelude

import Control.Monad.Eff (Eff)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)
import Data.Validation.Semigroup (V)
import Node.Process (PROCESS)

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

data OptError
  = TypeError ErrorMsg
  | MissingOpt ErrorMsg
  | UnrecognizedOpt String
  | Custom ErrorMsg

renderOptError :: OptError -> String
renderOptError = case _ of
  TypeError msg -> "Type error: " <> msg
  MissingOpt msg -> "Missing option: " <> msg
  UnrecognizedOpt msg -> "Unrecognized option: " <> msg
  Custom msg -> msg

instance showError :: Show OptError where
  show (TypeError msg) = "(TypeError " <> show msg <> ")"
  show (MissingOpt msg) = "(MissingOpt " <> show msg <> ")"
  show (UnrecognizedOpt msg) = "(UnrecognizedOpt " <> show msg <> ")"
  show (Custom msg) = "(Custom " <> show msg <> ")"

type Value a = V (List OptError) a

type OptState =
  { hyphen :: List Char -- i.e. program -abc
  , dash :: List (Tuple String String) -- i.e. program --method POST
  , flags :: List String } -- i.e. program --silent

type Result a = {state :: OptState, val :: Value a}

type ErrorMsg = String

type Preferences a e r =
  { errorOnUnrecognizedOpts :: Boolean
  , onError :: List OptError -> Eff (process :: PROCESS | e) r
  , onSuccess :: a -> Eff (process :: PROCESS | e) r
  , helpMsg :: Maybe String
  }