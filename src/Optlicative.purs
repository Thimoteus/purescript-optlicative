module Optlicative where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Array as Array
import Data.Either (Either(..), either, note)
import Data.Int (fromNumber)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Global (isNaN, readFloat)
import Node.Process (PROCESS, argv)

type Errors = List Error

data Error = Error

instance showError :: Show Error where
  show Error = "Error"

type OptState = Array String

type Result a = {state :: OptState, val :: Either Errors a}

newtype Optlicative a = Optlicative (OptState -> Result a)

derive instance newtypeOptlicative :: Newtype (Optlicative a) _

liftErr :: forall a b. Either Errors (a -> b) -> Either Errors a -> Either Errors b
liftErr (Right f) (Right a) = Right (f a)
liftErr (Left es) (Left es') = Left (es <> es')
liftErr (Left e) _ = Left e
liftErr _ (Left e) = Left e

derive instance functorOptlicative :: Functor Optlicative

instance applyOptlicative :: Apply Optlicative where
  apply (Optlicative f) (Optlicative a) = Optlicative \ s ->
    let r1 = f s
        a' = a r1.state
        val = liftErr r1.val a'.val
        state = a'.state
    in  {state, val}

instance applicativeOptlicative :: Applicative Optlicative where
  pure a = Optlicative \ state -> {state, val: Right a}

except :: forall a. Error -> OptState -> Result a
except e state = {state, val: Left (List.singleton e)}

throw :: forall a. Error -> Optlicative a
throw e = Optlicative (except e)

string :: Optlicative String
string = Optlicative \ state -> case Array.uncons state of
  Just {head, tail} -> {state: tail, val: Right head}
  _ -> except Error state

int :: Optlicative Int
int = Optlicative \ state -> maybe
  (except Error state)
  (\ n -> {state: Array.drop 1 state, val: Right n})
  (fromNumber <<< readFloat =<< Array.head state)

float :: Optlicative Number
float = Optlicative \ state -> either
  (flip except state)
  (\ n -> {state: Array.drop 1 state, val: Right n}) do
    h <- note Error $ Array.head state
    let n = readFloat h
    if isNaN n then Left Error else Right n

parse
  :: forall a e
   . Optlicative a
  -> Eff (process :: PROCESS | e) (Either Errors a)
parse o = do
  args <- Array.drop 2 <$> argv
  pure <<< _.val $ unwrap o args