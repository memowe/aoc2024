import Data.List
import Control.Applicative

splitSeries :: String -> [[Int]]
splitSeries = map (map read . words) . lines

countSafe :: [[Int]] -> Int
countSafe = length . filter isSafe

isSafe :: [Int] -> Bool
isSafe = isMonotone &&& smallValues
  where isMonotone  = (== 1) . length . nub . map signum . diffs
        smallValues = all ((<= 3) . abs) . diffs
        diffs       = zipWith (-) <*> tail
        (&&&)       = liftA2 (&&)

main :: IO ()
main = interact $ show . countSafe . splitSeries
