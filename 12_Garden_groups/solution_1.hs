{-# LANGUAGE TemplateHaskell #-}

import Data.Maybe
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map, (!), (!?))
import Data.Bifunctor
import Control.Monad.State.Strict
import Control.Lens (uses, (%=), makeLenses)

-- union-find with path compression \o/

type UFM  a = State (UF a)
data UF   a = UF {_parent :: Map a a, _rank :: Map a Int}
makeLenses ''UF

makeSet :: Ord a => a -> UFM a ()
makeSet x = do
  parent  %= M.insert x x
  rank    %= M.insert x 0

find :: Ord a => a -> UFM a a
find x = do
  p <- parent `uses` (! x)
  if p == x
    then return x
    else do pp <- find p
            parent %= M.insert x pp
            return pp

union :: Ord a => a -> a -> UFM a ()
union x' y' = do
  x <- find x'
  y <- find y'
  when (x /= y) $ do
    xr <- rank `uses` (! x)
    yr <- rank `uses` (! y)
    let (hi,lo) = if xr < yr then (y,x) else (x,y)
    parent %= M.insert lo hi
    when (xr == yr) $ rank %= M.adjust succ hi

sets :: Ord a => UFM a [Set a]
sets = do
  xs    <- parent `uses` M.keys
  pairs <- forM xs $ \x -> (, S.singleton x) <$> find x
  return $ M.elems (M.fromListWith S.union pairs)

unionFindSets :: Ord a => UFM a () -> [Set a]
unionFindSets op = evalState (op >> sets) (UF M.empty M.empty)

-- problem specific

type Col    = Int
type Row    = Int
type Coord  = (Col, Row)
type Cell   = (Coord, Char)
type Region = Set Coord
type Grid   = Map Coord Char
data Dir    = N | E | S | W deriving Enum

readGrid :: String -> Grid
readGrid input =
  let gls = zip [0..] <$> lines input
  in  M.fromList $ concat $ zipWith trans [0..] gls
  where trans r = map $ first (,r)

go :: Dir -> Coord -> Coord
go N (x,y) = (x,y-1)
go E (x,y) = (x+1,y)
go S (x,y) = (x,y+1)
go W (x,y) = (x-1,y)

lookupNW :: Grid -> Coord -> [Cell]
lookupNW grid coord = mapMaybe (look . ($ coord) . go) [N,W]
  where look c = (c,) <$> grid !? c

setsFromGrid :: Grid -> UFM Cell ()
setsFromGrid grid = forM_ (M.toList grid) $ \cell@(crd, chr) -> do
  makeSet cell
  mapM_ (union cell) $ filter ((== chr) . snd) $ lookupNW grid crd

setToRegion :: Set Cell -> Region
setToRegion = S.map fst

area :: Region -> Int
area = S.size

perimeter :: Region -> Int
perimeter rs = sum $ map (length . bounds) $ S.toList rs
  where bounds c  = filter (not . (`S.member` rs) . (`go` c)) dirs
        dirs      = enumFrom (toEnum 0)

price :: Region -> Int
price = (*) <$> area <*> perimeter

main :: IO ()
main = interact $ show
          . sum . map (price . setToRegion)
          . unionFindSets . setsFromGrid
          . readGrid
