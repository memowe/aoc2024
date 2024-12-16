split2 :: [a] -> [[a]]
split2 xs = [as, bs]
  where (as,bs) = splitAt (length xs `div` 2) xs

step :: (Integral a, Read a, Show a) => a -> [a]
step 0                      = [1]
step n  | even (length sn)  = read <$> split2 sn
        | otherwise         = [n * 2024]
  where sn = show n

blink :: (Integral a, Read a, Show a) => [a] -> [a]
blink = concatMap step

readStones :: String -> [Integer]
readStones = map read . words

main :: IO ()
main = interact $ show . length . (!! 25) . iterate blink . readStones
