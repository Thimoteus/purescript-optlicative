module Test.Types where

import Prelude
import Node.Commando (Opt)
import Data.List (List, fold)

type ConfigRec =
  ( one :: Opt Config
    ( two :: Opt Config ()
    )
  )

data Config
  = GlobalConfig GlobalConfig
  | ConfigOne ConfigOne  
  | ConfigTwo ConfigTwo

showConfig :: Config -> String
showConfig (GlobalConfig {help, version, say}) =
  "Global config parsed: \n" <>
  "help: " <> show help <> ", " <>
  "version: " <> show version <> ", " <>
  "say: " <> say
showConfig (ConfigOne {output, names, help}) =
  "ConfigOne parsed: \n" <>
  "output: " <> output <> ", " <>
  "names: " <> fold names <> ", " <>
  "help: " <> show help
showConfig (ConfigTwo {color, help}) =
  "ConfigTwo parsed: \n" <>
  "color: " <> show color <> ", " <>
  "help: " <> show help

type GlobalConfig =
  { help :: Boolean
  , version :: Boolean
  , say :: String
  }

type ConfigOne =
  { output :: String
  , names :: List String
  , help :: Boolean
  }

type ConfigTwo =
  { color :: Boolean
  , help :: Boolean
  }
