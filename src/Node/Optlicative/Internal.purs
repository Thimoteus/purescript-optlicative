module Node.Optlicative.Internal where

import Prelude

import Data.Array as Array
import Data.Foldable (or)
import Data.List (List(Nil), (:))
import Data.List as List
import Data.List.Types (toList)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.String as String
import Data.String.CodeUnits as CU
import Data.Validation.Semigroup (invalid, isValid)
import Foreign (MultipleErrors, renderForeignError)
import Node.Commando (class Commando, commando)
import Node.Optlicative.Types (ErrorMsg, OptError(..), OptState, Result, Value, Preferences)

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
removeAtFor beg end = removeAtForWhile beg end (const true)

removeAtForWhile
  :: Int
  -> Int
  -> (String -> Boolean)
  -> OptState
  -> {removed :: OptState, rest :: OptState}
removeAtForWhile beg end f {unparsed} =
  let
    splice = spliceWhile f beg (end + 1) unparsed
  in
    {removed: {unparsed: splice.focus}, rest: {unparsed: splice.pre <> splice.post}}

spliceWhile
  :: forall a
   . (a -> Boolean)
  -> Int -> Int
  -> List a
  -> {pre :: List a, focus :: List a, post :: List a}
spliceWhile f beg end lst =
  let
    a = takeDropWhile (const true) beg lst
    b = takeDropWhile' f (end - 1) a.dropped
  in
    { pre: a.taken
    , focus: b.taken
    , post: b.dropped
    }

takeDropWhile :: forall a. (a -> Boolean) -> Int -> List a -> {taken :: List a, dropped :: List a}
takeDropWhile = takeDrop Nil

-- Ignores first element
takeDropWhile' :: forall a. (a -> Boolean) -> Int -> List a -> {taken :: List a, dropped :: List a}
takeDropWhile' _ _ Nil = {taken: Nil, dropped: Nil}
takeDropWhile' f n (_ : xs) = takeDrop Nil f n xs

takeDrop :: forall a. List a -> (a -> Boolean) -> Int -> List a -> {taken :: List a, dropped :: List a}
takeDrop acc f n lst = case n, lst of
  0, _ -> {taken: List.reverse acc, dropped: lst}
  _, Nil -> {taken: List.reverse acc, dropped: lst}
  _, x : xs ->
    if f x
      then takeDrop (x : acc) f (n - 1) xs
      else {taken: List.reverse acc, dropped: lst}

removeHyphen :: Char -> OptState -> OptState
removeHyphen c state =
  let
    f str
      | isMultiHyphen str = String.replace
        (String.Pattern (CU.singleton c))
        (String.Replacement "")
        str
      | str == "-" <> CU.singleton c = ""
      | otherwise = str
  in
    state {unparsed = List.filter (_ /= "") (f <$> state.unparsed)}

isMultiHyphen :: String -> Boolean
isMultiHyphen s =
  String.take 1 s == "-" &&
  String.take 2 s /= "--" &&
  String.length s >= 3

isSingleHyphen :: String -> Boolean
isSingleHyphen s =
  String.take 1 s == "-" &&
  String.take 2 s /= "--" &&
  String.length s == 2

isDdash :: String -> Boolean
isDdash s = String.take 2 s == "--" && String.length s >= 3

startsDash :: String -> Boolean
startsDash s = String.take 1 s == "-"

hyphens :: List String -> List String
hyphens = List.filter (isMultiHyphen || isSingleHyphen)

hasHyphen :: Char -> OptState -> Boolean
hasHyphen c state = or $
  String.contains (String.Pattern $ CU.singleton c) <$>
  hyphens state.unparsed

find :: forall a. (a -> String) -> a -> OptState -> Maybe Int
find f n = List.elemIndex (f n) <<< _.unparsed

ddash :: String -> String
ddash = append "--"

hyphen :: Char -> String
hyphen = append "-" <<< CU.singleton

charList :: String -> List Char
charList = charList' Nil where
  charList' acc str = case CU.uncons str of
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
  in
    {cmds: init, opts: rest}

parse
  :: forall optrow a
   . Commando optrow a
  => Record optrow
  -> Preferences a
  -> Array String
  -> {cmd :: Maybe String, value :: Value a}
parse rec prefs argv =
  let
    argslist = List.fromFoldable (Array.drop 2 argv)
    args = partitionArgsList argslist
    -- Commands
    cmdores = commando rec args.cmds
    cmd = _.cmd <$> cmdores
    -- Opts
    o = maybe prefs.globalOpts _.opt cmdores
    st = unwrap o {unparsed: args.opts}
    unrecCheck = prefs.errorOnUnrecognizedOpts && not (List.null st.state.unparsed)
    value = case prefs.usage, unrecCheck, isValid st.val of
      Just msg, true, true ->
        unrecognizedOpts st.state <*>
        throwSingleError (Custom msg)
      Just msg, true, _ ->
        unrecognizedOpts st.state <*>
        throwSingleError (Custom msg) <*>
        st.val
      Just msg, false, false -> throwSingleError (Custom msg) <*> st.val
      Just _, false, _ -> st.val
      _, true, _ -> unrecognizedOpts st.state <*> st.val
      _, _, _ -> st.val
  in
    {cmd, value}
