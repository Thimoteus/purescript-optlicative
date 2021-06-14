module Node.Commando
  ( class RLCommando, rlCommando
  , class Commando, commando
  , Opt(..)
  , endOpt
  ) where

import Prelude

import Data.List (List(Nil), (:))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Node.Optlicative.Types (Optlicative)
import Prim.Row as R
import Prim.RowList as RL
import Record (delete, get)
import Type.Proxy (Proxy(..))

class RLCommando
  (rl :: RL.RowList Type)
  (row :: Row Type)
  (a :: Type)
  | rl -> row
  where
    rlCommando :: Proxy rl -> Record row -> List String -> Maybe {cmd :: String, opt :: Optlicative a}

instance basisRlHelp :: RLCommando RL.Nil () a where
  rlCommando _ _ _ = Nothing

instance ihRlHelp ::
  ( IsSymbol k -- key in row of IH case
  , RLCommando tail rowtail a -- IH
  , RLCommando list' row' a -- also IH, for 2nd arg which is a row
  , R.Cons k (Opt a row') rowtail row -- row = rowtail U Opt
  , R.Lacks k rowtail
  , RL.RowToList rowtail tail -- rowtail <-> tail
  , RL.RowToList row (RL.Cons k (Opt a row') tail) -- row <-> list
  , RL.RowToList row' list' -- row' <-> list'
  ) => RLCommando (RL.Cons k (Opt a row') tail) row a where

    rlCommando _ rec args@(cmd : Nil) =
      let
        sproxy = SProxy :: SProxy k
        opt = getopt (get sproxy rec)
        rectail = (delete sproxy rec) :: Record rowtail
      in
        if cmd == reflectSymbol sproxy
          then Just {cmd, opt}
          else rlCommando (Proxy :: Proxy tail) rectail args

    rlCommando _ rec args@(x : xs) = -- haven't found final command yet
      let
        sproxy = SProxy :: SProxy k
        rldeeper = Proxy :: Proxy list'
        rlwider = Proxy :: Proxy tail
        rec' = (getrow (get sproxy rec)) :: Record row'
        rectail = (delete sproxy rec) :: Record rowtail
      in
        if x == reflectSymbol sproxy -- we're on the right path
          then rlCommando rldeeper rec' xs -- recurse deeper
          else rlCommando rlwider rectail args -- recurse wider

    rlCommando _ _ _ = Nothing -- ran out of command path elements

class Commando (row :: Row Type) a where
  commando :: Record row -> List String -> Maybe {cmd :: String, opt :: Optlicative a}

instance rowHelpInst ::
  ( RL.RowToList row list
  , RLCommando list row a
  ) => Commando row a where
    commando rec xs = rlCommando (Proxy :: Proxy list) rec xs

data Opt (a :: Type) (row :: Row Type) = Opt (Optlicative a) (Record row)

endOpt :: forall a. Optlicative a -> Opt a ()
endOpt o = Opt o {}

getopt :: forall a row. Opt a row -> Optlicative a
getopt (Opt opt _) = opt

getrow :: forall a row. Opt a row -> Record row
getrow (Opt _ row) = row
