import Data.Char
import qualified Data.Map as M
import Data.Map (Map, (!))
import Data.Bifunctor
import Text.ParserCombinators.ReadP

type Col          = Int
type Row          = Int
type Coord        = (Col, Row)
data Height       = H0 | H1 | H2 | H3 | H4
                  | H5 | H6 | H7 | H8 | H9
                    deriving (Eq, Enum, Bounded)
newtype HikingMap = HM {hm :: Map Coord Height} deriving Show

instance Show Height where
  show = show . fromEnum

instance Read HikingMap where
  readsPrec _ = readP_to_S hmap
    where hmap      = HM . M.fromList . fixCoords <$> many row
          row       = zip [0..]                   <$> many height <* char '\n'
          height    = toEnum . digitToInt         <$> satisfy isDigit
          fixCoords = concat . zipWith (map . first . flip (,)) [0..]

walkUp :: HikingMap -> Coord -> [Coord]
walkUp hima start =
  let h = hm hima ! start
  in  filter ((&&) <$> (`M.member` hm hima) <*> (== succ h) . (hm hima !)) cands
  where cands = map ($ start) [first pred, first succ, second pred, second succ]

walksToSummit :: HikingMap -> Coord -> [[Coord]]
walksToSummit hima start
  | hm hima ! start == maxBound = [[start]]
  | otherwise = [ start : rest  | next <- walkUp hima start
                                , rest <- walksToSummit hima next
                                ]

countTrails :: HikingMap -> [(Coord, Int)]
countTrails hima = map ((,) <*> length . walksToSummit hima) trailheads
  where trailheads = M.keys . M.filter (== H0) . hm $ hima

main :: IO ()
main = interact $ show . sum . map snd . countTrails . read
