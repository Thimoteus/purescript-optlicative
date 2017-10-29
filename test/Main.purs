module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Validation.Semigroup (unV)
import Node.Optlicative (Optlicative, int, parse, string, float)
import Node.Optlicative.Internal (Value)
import Node.Process (PROCESS)

type Person = {name :: String, age :: Int, height :: Number}

person :: Optlicative Person
person = {name: _, age: _, height: _} <$> string "name" <*> int "age" <*> float "height"

showPerson :: Value Person -> String
showPerson  = unV f g where
  g {name, age, height} =
    "{name: " <>
    name <>
    ", age: " <>
    show age <>
    ", height: " <>
    show height <> "}"
  f = show

main :: forall e. Eff (process :: PROCESS, console :: CONSOLE | e) Unit
main = log <<< showPerson =<< parse person
