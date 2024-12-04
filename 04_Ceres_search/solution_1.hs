import Data.List
import Data.Bifunctor
import Data.Map (Map, fromList, findMax, (!))

countIn :: Eq a => [a] -> [a] -> Int
countIn []  _   = 0
countIn _   []  = 0
countIn xs  ys  = countIn xs (tail ys) + if found then 1 else 0
  where found = xs `isPrefixOf` ys

type Row      = Int
type Col      = Int
type Coord2D  = (Col, Row)
type Map2D a  = Map Coord2D a

collect :: Ord a => [[a]] -> Map2D a
collect = fromList . concat . z0 (z0 . curry . first . flip (,))
  where z0 = flip zipWith [0..]

genRows, genCols, genUpDiags, genDownDiags :: Coord2D -> [[Coord2D]]
genRows       (x,y) = map ((`map` [0..x]) . flip (,)) [0..y]
genCols       (x,y) = map ((`map` [0..y]) . (,)) [0..x]
genUpDiags    (x,y) = map (\s -> [(c,r) | c <- [0..x], r <- [0..y], c+r == s]) [0 .. x+y]
genDownDiags  (x,y) = map (\d -> [(c,r) | c <- [0..x], r <- [0..y], c-r == d]) [-y .. x]

countAll :: (Eq a, Ord a) => [a] -> Map2D a -> Int
countAll xs m =
  let maxCoord        = fst (findMax m)
      coordLists      = concatMap ($ maxCoord) [genRows, genCols, genUpDiags, genDownDiags]
      fullCoordLists  = concatMap (\cl -> [cl, reverse cl]) coordLists
  in  sum (map (\cl -> xs `countIn` map (m !) cl) fullCoordLists)

main :: IO ()
main = interact $ show . countAll "XMAS" . collect . lines
