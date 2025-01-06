import Data.MemoTrie

split2 :: [a] -> [[a]]
split2 xs = [as, bs]
  where (as, bs) = splitAt (length xs `div` 2) xs

type Stone  = Int
type Depth  = Int
type Count  = Int

blinkLen :: Depth -> Stone -> Count
blinkLen =
  let bl 0 _  = 1
      bl n 0  = blinkLen (n-1) 1
      bl n x
        | even (length $ show x)  = sum $ blinkLen (n-1) . read <$> split2 (show x)
        | otherwise               = blinkLen (n-1) (x*2024)
  in  memo2 bl

main :: IO ()
main = interact $ show . sum . map (blinkLen 75 . read) . words . head . lines
