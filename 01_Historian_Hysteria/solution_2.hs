import Data.Map (fromListWith, findWithDefault)

columns :: String -> ([Int], [Int])
columns = transposePair .  map (toPair . map read . words) . lines
  where transposePair = (,) <$> map fst <*> map snd
        toPair [a,b]  = (a,b)
        toPair xs     = error $ "invalid data: " ++ show xs

similarityScore :: [Int] -> [Int] -> Int
similarityScore xs = sum . map singleScore
  where singleScore x = x * findWithDefault 0 x (summarize xs)
        summarize     = fromListWith (+) . map (,1)

main :: IO ()
main = interact $ show . uncurry (flip similarityScore) . columns
