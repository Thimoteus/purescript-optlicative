module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Tuple (Tuple(..))
import Node.Process (PROCESS)
import Optlicative (int, parse, string)

main :: forall e. Eff (process :: PROCESS, console :: CONSOLE | e) Unit
main = logShow =<< parse (Tuple <$> string <*> int)
