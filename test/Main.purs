module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe (Maybe(..))
import Node.Optlicative (Optlicative, defaultPreferences, flag, float, int, parse, string, withDefault)
import Node.Process (PROCESS)

newtype Person = Person {name :: String, age :: Int, height :: Number, elmo :: Boolean}

instance showPers :: Show Person where show = showPerson

helpMsg :: String
helpMsg = "Usage: --name <string> --age <int> --height <float> [--elmo]"

person :: Optlicative Person
person
  = (\name age height elmo -> Person {name, age, height, elmo})
  <$> string "name" Nothing
  <*> withDefault 0 (int "age" Nothing)
  <*> float "height" Nothing
  <*> flag "elmo" Nothing

showPerson :: Person -> String
showPerson (Person {name, age, height, elmo}) =
    "{name: " <>
    name <>
    ", age: " <>
    show age <>
    ", height: " <>
    show height <>
    ", elmo: " <>
    show elmo <>
    "}"

main :: forall e. Eff (process :: PROCESS, console :: CONSOLE | e) Unit
main = parse prefs person where
  prefs = defaultPreferences
    { onSuccess = log <<< showPerson
    , helpMsg = Just helpMsg
    }
