module Test.Types where

import Prelude
import Node.Commando (Opt)

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
showConfig (GlobalConfig {help, version}) =
  "Global config parsed: \n" <>
  "help: " <> show help <> ", " <>
  "version: " <> show version
showConfig (ConfigOne {output, help}) =
  "ConfigOne parsed: \n" <>
  "output: " <> output <> ", " <>
  "help: " <> show help
showConfig (ConfigTwo {color, help}) =
  "ConfigTwo parsed: \n" <>
  "color: " <> show color <> ", " <>
  "help: " <> show help

type GlobalConfig =
  { help :: Boolean
  , version :: Boolean
  }

type ConfigOne =
  { output :: String
  , help :: Boolean
  }

type ConfigTwo =
  { color :: Boolean
  , help :: Boolean
  }