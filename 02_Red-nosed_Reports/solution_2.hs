import Data.List
import Control.Applicative

isDampedSafe :: [Int] -> Bool
isDampedSafe = isSafe ||| any isSafe . withouts
  where withouts  = zipWith (++) <$> inits <*> tails.tail
        (|||)     = liftA2 (||); infixr 2 |||

isSafe :: [Int] -> Bool
isSafe = isMonotone &&& notConstant &&& smallDiffs
  where isMonotone  = (== 1) . length . group . map signum . diffs
        notConstant = any (/= 0) . diffs
        smallDiffs  = all ((<= 3) . abs) . diffs
        diffs       = zipWith (-) <*> tail
        (&&&)       = liftA2 (&&)

main :: IO ()
main = interact
  $ show . length
  . filter isDampedSafe
  . map (map read . words) . lines
