module Node.Optlicative.Internal where

import Prelude

import Data.Function (on)
import Data.List (List(Nil), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple (Tuple(..), fst, lookup)
import Data.Validation.Semigroup (V, invalid)

data Error = Error

instance showError :: Show Error where
  show Error = "Error"

type Value a = V (List Error) a

type OptState =
  { hyphen :: List Char -- i.e. program -abc
  , dash :: List (Tuple String String) -- i.e. program --method POST
  , flags :: List String } -- i.e. program --silent

type Result a = {state :: OptState, val :: Value a}

except :: forall a. Error -> OptState -> Result a
except e state = {state, val: invalid (pure e)}

removeHyphen :: Char -> OptState -> OptState
removeHyphen c os = os {hyphen = List.delete c os.hyphen}

removeDash :: String -> OptState -> OptState
removeDash name os =
  os {dash = List.deleteBy (eq `on` fst) (Tuple name "") os.dash}

removeFlag :: String -> OptState -> OptState
removeFlag name os = os {flags = List.delete name os.flags}

findHyphen :: Char -> OptState -> Boolean
findHyphen c {hyphen} = c `List.elem` hyphen

findFlag :: String -> OptState -> Boolean
findFlag name {flags} = name `List.elem` flags

findDash :: String -> OptState -> Maybe String
findDash name {dash} = lookup name dash

charList :: String -> List Char
charList = charList' Nil where
  charList' acc str = case String.uncons str of
    Just {head, tail} -> charList' (head : acc) tail  
    _ -> acc

initialize :: List String -> OptState
initialize = init {hyphen: Nil, dash: Nil, flags: Nil} where
  init acc (x : xs) = case String.take 2 x of -- case String.uncons x of
    "--" -> ddash (String.drop 2 x) acc xs
    _ -> case String.uncons x of
      Just {head: '-', tail} -> init (acc {hyphen = charList tail <> acc.hyphen}) xs
      _ -> acc
  init acc Nil = acc
  ddash x acc (y : ys) = case String.uncons y of
    Just {head: '-'} -> init (acc {flags = y : acc.flags}) ys
    Just _ -> init (acc {dash = Tuple x y : acc.dash}) ys
    _ -> init acc ys -- this is where we'd put passthrough logic
  ddash _ acc _ = acc