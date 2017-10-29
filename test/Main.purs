module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe (Maybe(..))
import Data.Validation.Semigroup (unV)
import Node.Optlicative (Optlicative, float, int, parse, renderErrors, string, withDefault, (<?>))
import Node.Process (PROCESS)

type Person = {name :: String, age :: Int, height :: Number}

person :: Optlicative Person
person
  = {name: _, age: _, height: _}
  <$> string "name" Nothing
  <*> withDefault 0 (int "age" Nothing)
  <*> float "height" Nothing
  <?> "Usage: --name [string] --age [int] --height [float]"

showPerson :: Person -> String
showPerson {name, age, height} =
    "{name: " <>
    name <>
    ", age: " <>
    show age <>
    ", height: " <>
    show height <> "}"

main :: forall e. Eff (process :: PROCESS, console :: CONSOLE | e) Unit
main = unV (log <<< renderErrors) (log <<< showPerson) =<< parse person
