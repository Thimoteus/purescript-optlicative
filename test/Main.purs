module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.List (length)
import Data.Maybe (Maybe(..), maybe)
import Data.Validation.Semigroup (validation)
import Node.Commando (Opt(Opt))
import Node.Optlicative (Optlicative, Preferences, defaultPreferences, flag, logErrors, optlicate, string, many)
import Test.Types (Config(..), ConfigRec, showConfig)

configRec :: Record ConfigRec
configRec =
  { one: Opt optOne
    { two: Opt optTwo {}
    }
  }

optOne :: Optlicative Config
optOne = (\ output names help -> ConfigOne {output, names, help})
  <$> string "output" Nothing
  <*> many (string "name" Nothing)
  <*> flag "help" (Just 'h')

optTwo :: Optlicative Config
optTwo = (\ color help -> ConfigTwo {color, help})
  <$> flag "color" (Just 'c')
  <*> flag "help" (Just 'h')

globalConfig :: Optlicative Config
globalConfig = (\ help version say -> GlobalConfig {help, version, say})
  <$> flag "help" (Just 'h')
  <*> flag "version" (Just 'v')
  <*> string "say" Nothing

myPrefs :: Preferences Config
myPrefs = defaultPreferences {globalOpts = globalConfig}

-- | Try running the following:
-- | 1. `pulp test -- one`
-- | 2. `pulp test -- one --output`
-- | 3. `pulp test -- one --output blah`
-- | 4. `pulp test -- one two`
-- | 5. `pulp test -- one two --help`
-- | 5. `pulp test -- --version`
-- | 6. `pulp test -- one --name "John" --name "Bob" --name "Billy"
-- | 7. `pulp test -- --version`
-- | 8. `pulp test -- --version --say doh`
-- | 9. `pulp test`
main :: Effect Unit
main = do
  {cmd, value} <- optlicate configRec myPrefs
  maybe
    (log "No path parsed")
    (\ x -> log "Path parsed" *> log x)
    cmd
  validation
    (\ x -> do
      log "Errors found: "
      log (show (length x) <> " errors")
      logErrors x)
    (\ x -> log "Value found: " *> log (showConfig x))
    value
