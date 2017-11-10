module Node.Commando where

import Prelude

import Data.List (List(Nil), (:))
import Data.Maybe (Maybe(..))
import Data.Record (delete, get)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Node.Optlicative.Types (Optlicative)
import Type.Row (class RowLacks, class RowToList, Cons, Nil, RLProxy(..), kind RowList)

class RLHelp
  (rl :: RowList)
  (row :: # Type)
  (a :: Type)
  | rl -> row
  where
    rlHelp :: RLProxy rl -> Record row -> List String -> Maybe {cmd :: String, opt :: Optlicative a}

instance basisRlHelp :: RLHelp Nil () a where
  rlHelp _ _ _ = Nothing

instance ihRlHelp ::
  ( IsSymbol k -- key in row of IH case
  , RLHelp tail rowtail a -- IH
  , RLHelp list' row' a -- also IH, for the Help 2nd arg
  , RowCons k (Opt a row') rowtail row -- row = rowtail U Opt
  , RowLacks k rowtail
  , RowToList rowtail tail -- rowtail <-> tail
  , RowToList row (Cons k (Opt a row') tail) -- row <-> list
  , RowToList row' list' -- row' <-> list'
  ) => RLHelp (Cons k (Opt a row') tail) row a where

    rlHelp _ rec args@(cmd : Nil) =
      let
        sproxy = SProxy :: SProxy k  
        opt = getopt (get sproxy rec)
        rectail = (delete sproxy rec) :: Record rowtail
      in
        if cmd == reflectSymbol sproxy
          then Just {cmd, opt}
          else rlHelp (RLProxy :: RLProxy tail) rectail args

    rlHelp _ rec args@(x : xs) = -- haven't found final helptext yet
      let
        sproxy = SProxy :: SProxy k
        rldeeper = RLProxy :: RLProxy list'
        rlwider = RLProxy :: RLProxy tail
        rec' = (getrow (get sproxy rec)) :: Record row'
        rectail = (delete sproxy rec) :: Record rowtail
      in
        if x == reflectSymbol sproxy -- we're on the right path
          then rlHelp rldeeper rec' xs -- recurse deeper
          else rlHelp rlwider rectail args -- recurse wider

    rlHelp _ _ _ = Nothing -- ran out of command path list elements

class Commando (row :: # Type) a where
  commando :: Record row -> List String -> Maybe {cmd :: String, opt :: Optlicative a}

instance rowHelpInst ::
  ( RowToList row list
  , RLHelp list row a
  ) => Commando row a where
    commando rec xs = rlHelp (RLProxy :: RLProxy list) rec xs

data Opt (a :: Type) (row :: # Type) = Opt (Optlicative a) (Record row)

endOpt :: forall a. Optlicative a -> Opt a ()
endOpt o = Opt o {}

getopt :: forall a row. Opt a row -> Optlicative a
getopt (Opt opt _) = opt

getrow :: forall a row. Opt a row -> Record row
getrow (Opt _ row) = row