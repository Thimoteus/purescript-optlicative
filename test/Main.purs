module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe (Maybe(..))
import Node.Optlicative (Optlicative, defaultPreferences, float, int, parse, string, withDefault)
import Node.Process (PROCESS)

type Person = {name :: String, age :: Int, height :: Number}

helpMsg :: String
helpMsg = "Usage: --name [string] --age [int] --height [float]"

person :: Optlicative Person
person
  = {name: _, age: _, height: _}
  <$> string "name" Nothing
  <*> withDefault 0 (int "age" Nothing)
  <*> float "height" Nothing

showPerson :: Person -> String
showPerson {name, age, height} =
    "{name: " <>
    name <>
    ", age: " <>
    show age <>
    ", height: " <>
    show height <> "}"

main :: forall e. Eff (process :: PROCESS, console :: CONSOLE | e) Unit
main = parse prefs person where
  prefs = defaultPreferences
    { onSuccess = log <<< showPerson
    , helpMsg = Just helpMsg
    }
