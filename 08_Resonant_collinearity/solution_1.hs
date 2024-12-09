import Data.Bifunctor
import qualified Data.Set as S
import qualified Data.Map as M
import Text.ParserCombinators.ReadP

type    Row       = Int
type    Col       = Int
type    Location  = (Col, Row)
type    Frequency = Char
type    Bounds    = Location
newtype LocAnts   = LAs { locations :: M.Map Location (Maybe Frequency)
                        } deriving Show
type    FreqLocs  = M.Map Frequency (S.Set Location)

instance Read LocAnts where
  readsPrec _ = readP_to_S $ LAs . M.fromList . combine <$> rows
    where rows      = row `endBy` char '\n'
          row       = zip [0..] <$> many (empty <++ antenna)
          empty     = Nothing   <$  char '.'
          antenna   = Just      <$> satisfy (/= '\n')
          combine   = concat . zipWith (map . first . flip (,)) [0..]

getBounds :: LocAnts -> Bounds
getBounds = fst . M.findMax . locations

inside :: Bounds -> Location -> Bool
inside (maxCol, maxRow) (col, row) = and  [ 0 <= col, col <= maxCol
                                          , 0 <= row, row <= maxRow
                                          ]

toFreqLocs :: LocAnts -> FreqLocs
toFreqLocs = M.fromListWith S.union . swapSet . locations
  where swapSet m = [(f, S.singleton c) | (c, Just f) <- M.toList m]

setAntinodes :: S.Set Location -> S.Set Location
setAntinodes fls = S.fromList . concat $
  [ [a `minus` ab, b `plus` ab]
  | a <- S.toList fls, b <- S.toList fls, a /= b
  , let ab = b `minus` a
  ]
  where (c1,r1) `plus` (c2,r2)  = (c1+c2,r1+r2)
        inverse (c,r)           = (-c,-r)
        l1 `minus` l2           = l1 `plus` inverse l2

antinodes :: LocAnts -> S.Set Location
antinodes las =
  let bs  = getBounds las
      fas = M.map setAntinodes . toFreqLocs $ las
      aas = S.unions . M.elems $ fas
  in  S.filter (inside bs) aas

main :: IO ()
main = interact $ show . S.size . antinodes . read
