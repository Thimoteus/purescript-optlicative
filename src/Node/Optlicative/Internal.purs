module Node.Optlicative.Internal where

import Prelude

import Data.Foldable (or)
import Data.Foreign (MultipleErrors, renderForeignError)
import Data.List (List(Nil), (:))
import Data.List as List
import Data.List.Types (toList)
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Data.Validation.Semigroup (invalid)
import Node.Optlicative.Types (ErrorMsg, OptError(..), OptState, Result, Value)

throwSingleError :: forall a. OptError -> Value a
throwSingleError = invalid <<< List.singleton

except :: forall a. OptError -> OptState -> Result a
except e state = {state, val: throwSingleError e}

ex :: forall a. String -> String -> (String -> OptError) -> Maybe String -> OptState -> Result a
ex name exp ctor mb st = except (maybe (defaultError ctor name exp) ctor mb) st

findAllIndices :: forall a. (a -> Boolean) -> List a -> List Int
findAllIndices f xs = List.reverse zs where
  zs = go Nil 0 xs
  go acc i (y:ys) = if f y then go (i : acc) (i + 1) ys else go acc (i + 1) ys
  go acc _ Nil = acc

removeAtFor :: Int -> Int -> OptState -> {removed :: OptState, rest :: OptState}
removeAtFor beg end state@{unparsed} =
  let
    end' = beg + end + 1
    beg' = beg + 1
    removed = state {unparsed = List.slice beg' end' unparsed}
    rest = state {unparsed = List.take beg unparsed <> List.drop end' unparsed}
  in
    {removed, rest}

removeHyphen :: Char -> OptState -> OptState
removeHyphen c state =
  let
    f lst =
      lst <#> \ str ->
        if isHyphen str
          then String.replace
            (String.Pattern (String.singleton c))
            (String.Replacement "")
            str
          else str
  in
    state {unparsed = f state.unparsed}

isHyphen :: String -> Boolean
isHyphen s =
  String.take 1 s == "-" &&
  String.take 2 s /= "--" &&
  String.length s >= 2

hyphens :: List String -> List String
hyphens = List.filter isHyphen

hasHyphen :: Char -> OptState -> Boolean
hasHyphen c state = or $
  String.contains (String.Pattern $ String.singleton c) <$>
  hyphens state.unparsed

find :: forall a. (a -> String) -> a -> OptState -> Maybe Int
find f n = List.elemIndex (f n) <<< _.unparsed

ddash :: String -> String
ddash = append "--"

hyphen :: Char -> String
hyphen = append "-" <<< String.singleton

charList :: String -> List Char
charList = charList' Nil where
  charList' acc str = case String.uncons str of
    Just {head, tail} -> charList' (head : acc) tail  
    _ -> acc

defaultError :: (ErrorMsg -> OptError) -> String -> String -> OptError
defaultError f name expected = case f "" of
  TypeError _ -> TypeError $
    "Option '" <> name <> "' expects an argument of type " <> expected <> "."
  MissingOpt _ -> MissingOpt $
    "Option '" <> name <> "' is required."
  MissingArg _ -> MissingArg $
    "Option '" <> name <> "' expects " <> expected <> " arguments."
  UnrecognizedOpt _ -> UnrecognizedOpt name
  UnrecognizedCommand _ -> UnrecognizedCommand name
  Custom _ -> Custom name

multipleErrorsToOptErrors :: MultipleErrors -> List OptError
multipleErrorsToOptErrors errs =
  let strerrs = map renderForeignError errs
      strlist = toList strerrs
  in  map Custom strlist

unrecognizedOpts :: forall a. OptState -> Value a
unrecognizedOpts state = invalid $ map UnrecognizedOpt $ unrecognize Nil state.unparsed
  where
  isDdash s = String.take 2 s == "--" && String.length s >= 3
  unrecognize acc lst
    | (s : ss) <- lst
    , isDdash s = unrecognize (s : acc) ss
    | (_ : ss) <- lst = unrecognize acc ss
    | otherwise = acc

partitionArgsList :: List String -> {cmds :: List String, opts :: List String}
partitionArgsList argslist =
  let
    isCmd x = String.take 1 x /= "-"
    {init, rest} = List.span isCmd argslist
    cmds = init
    opts = rest
  in
    {cmds, opts}

isHelp :: List String -> Boolean
isHelp ("--help" : _) = true
isHelp _ = false