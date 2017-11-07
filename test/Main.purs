module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe (Maybe(..), maybe)
import Data.Validation.Semigroup (unV)
import Node.Commando (EndHelp, Help)
import Node.Optlicative (Optlicative, defaultPreferences, flag, float, int, parse, string, withDefault, logErrors)
import Node.Process (PROCESS)
import Type.Row (RProxy(..))

newtype Person = Person {name :: String, age :: Int, height :: Number, elmo :: Boolean}

instance showPers :: Show Person where show = showPerson

usage :: String
usage = "Usage: --name <string> --age <int> --height <float> [--elmo]"

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

type MyHelp =
  ( command :: Help "help for command"
    ( more :: EndHelp "help for second-level command" )
  , second :: EndHelp "help for second first-level command"
  )

main :: forall e. Eff (process :: PROCESS, console :: CONSOLE | e) Unit
main = do
  {cmd, value} <- parse (RProxy :: RProxy MyHelp) prefs person
  maybe (pure unit) (\ x -> log "Command is:" *> log x) cmd
  unV logErrors (log <<< showPerson) value
    where
    prefs = defaultPreferences {usage = Just usage}