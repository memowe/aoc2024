import Data.List
import Data.Function

columns :: String -> ([Int], [Int])
columns = transposePair .  map (toPair . map read . words) . lines
  where transposePair = (,) <$> map fst <*> map snd
        toPair [a,b]  = (a,b)
        toPair xs     = error $ "invalid data: " ++ show xs

distance :: [Int] -> [Int] -> [Int]
distance = zipWith (\a b -> abs (a-b)) `on` sort

reportDistances :: [Int] -> String
reportDistances = show . sum

main :: IO ()
main = interact $ reportDistances . uncurry distance . columns
