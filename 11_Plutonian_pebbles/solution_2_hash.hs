{-# LANGUAGE MultiWayIf #-}

import Data.HashMap.Strict (HashMap, (!?), insert, empty)
import Control.Monad.State.Strict

split2 :: [a] -> [[a]]
split2 xs = [as, bs]
  where (as, bs) = splitAt (length xs `div` 2) xs

type Stone  = Int
type Depth  = Int
type Count  = Int
type Cache  = HashMap (Depth, Stone) Count

blinkLen :: Depth -> Stone -> State Cache Count
blinkLen 0 _  = return 1
blinkLen n x  = do
  look <- gets (!? (n,x))
  case look of
    Just count  -> return count
    Nothing     ->
      do  result <- if
            | x == 0                  -> blinkLen (n-1) 1
            | even (length $ show x)  -> do let parts = read <$> split2 (show x)
                                            sum <$> mapM (blinkLen (n-1)) parts
            | otherwise               -> blinkLen (n-1) (x*2024)
          modify $ insert (n,x) result
          return result

allBlinks :: Depth -> [Stone] -> Count
allBlinks n = flip evalState empty . fmap sum . mapM (blinkLen n)

main :: IO ()
main = interact $ show . allBlinks 75 . map read . words . head . lines
