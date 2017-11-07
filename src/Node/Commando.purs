module Node.Commando
  ( class Commando
  , commando
  , class RLHelp
  , rlHelp
  , Help
  , EndHelp
  ) where

import Prelude

import Data.List (List(Nil), (:))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Type.Row (class RowLacks, class RowToList, Cons, Nil, RLProxy(..), RProxy(..), kind RowList)

data Help (help :: Symbol) (row :: # Type)

type EndHelp help = Help help ()

class RLHelp
  (rl :: RowList)
  (row :: # Type)
  | rl -> row
  where
    rlHelp :: RLProxy rl -> RProxy row -> List String -> Maybe String

instance basisRlHelp :: RLHelp Nil () where
  rlHelp _ _ _ = Nothing

instance ihRlHelp ::
  ( IsSymbol k -- key in row of IH case
  , IsSymbol h -- helptext in Help
  , RLHelp tail rowtail -- IH
  , RLHelp list' row' -- also IH, for the Help 2nd arg
  , RowCons k (Help h row') rowtail row -- row = rowtail U Help
  , RowLacks k rowtail
  , RowToList rowtail tail -- rowtail <-> tail
  , RowToList row (Cons k (Help h row') tail) -- row <-> list
  , RowToList row' list' -- row' <-> list'
  ) => RLHelp (Cons k (Help h row') tail) row where
    rlHelp _ _ args@(x : Nil) =
      if x == reflectSymbol (SProxy :: SProxy k)
        then Just (reflectSymbol (SProxy :: SProxy h))
        else rlHelp (RLProxy :: RLProxy tail) (RProxy :: RProxy rowtail) args
    rlHelp _ _ args@(x : xs) = -- haven't found final helptext yet
      if x == reflectSymbol (SProxy :: SProxy k) -- we're on the right path
        then rlHelp (RLProxy :: RLProxy list') (RProxy :: RProxy row') xs -- recurse deeper
        else rlHelp (RLProxy :: RLProxy tail) (RProxy :: RProxy rowtail) args -- recurse wider
    rlHelp _ _ _ = Nothing -- ran out of command path list elements

class Commando (row :: # Type) where
  commando :: forall proxy. proxy row -> List String -> Maybe String

instance rowHelpInst ::
  ( RowToList row list
  , RLHelp list row
  ) => Commando row where
    commando _ xs = rlHelp (RLProxy :: RLProxy list) (RProxy :: RProxy row) xs