import Data.Bifunctor
import Data.Map (Map, fromList, findMax, (!))

type Row      = Int
type Col      = Int
type Coord2D  = (Col, Row)
type Map2D a  = Map Coord2D a
type Coords2D = [(Col, Row)]
type Cross    = (Coords2D, Coords2D)

collect :: Ord a => [[a]] -> Map2D a
collect = fromList . concat . z0 (z0 . curry . first . flip (,))
  where z0 = flip zipWith [0..]

crosses :: Coord2D -> [Cross]
crosses (x,y) = [ ( [(c,r),(c+1,r+1),(c+2,r+2)]
                  , [(c,r+2),(c+1,r+1),(c+2,r)]
                  )
                | c <- [0..x-2]
                , r <- [0..y-2]
                ]

hasCross :: (Eq a, Ord a) => [a] -> Map2D a -> Cross -> Bool
hasCross xs m (cs1,cs2) = all (\cs -> map (m!) cs `elem` [xs, reverse xs]) [cs1,cs2]

findCrosses :: (Eq a, Ord a) => [a] -> Map2D a -> [Cross]
findCrosses xs = filter <$> hasCross xs <*> crosses . fst . findMax

main :: IO ()
main = interact $ show . length . findCrosses "MAS" . collect . lines
