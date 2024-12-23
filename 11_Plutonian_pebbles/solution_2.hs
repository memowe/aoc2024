import Data.Bifunctor
import qualified Data.Map as M

split2 :: [a] -> [[a]]
split2 xs = [as, bs]
  where (as, bs) = splitAt (length xs `div` 2) xs

type Engraving  = Int
type Count      = Int
type Stones     = M.Map Engraving Count

step :: Engraving -> Stones
step 0                      = M.singleton 1 1
step n  | even (length sn)  = M.fromListWith (+) $ map (\ds -> (read ds, 1)) (split2 sn)
        | otherwise         = M.singleton (n * 2024) 1
  where sn = show n

blink :: Stones -> Stones
blink = M.fromList . _ . map (first step) . M.toList

-- blink :: (Integral a, Read a, Show a) => [a] -> [a]
-- blink = concatMap step

-- readStones :: String -> [Integer]
-- readStones = map read . words

-- main :: IO ()
-- main = interact $ show . length . (!! 25) . iterate blink . readStones
