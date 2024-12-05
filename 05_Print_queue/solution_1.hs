import Data.Char
import Data.Set (Set, fromList, member)
import Text.ParserCombinators.ReadP
import Control.Monad

middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

type PrintLT  = Set (Int, Int)
type Series   = [Int]

data Print    = P { lt      :: PrintLT
                  , series  :: [Series]
                  } deriving Show

instance Read Print where
  readsPrec _ = readP_to_S printP
    where printP  = do  rs <- many ltP
                        void $ char '\n'
                        ss <- many seriesP
                        return $ P (fromList rs) ss
          ltP     = do  [lo,hi] <- numP `sepBy` char '|'
                        void $ char '\n'
                        return (lo,hi)
          numP    =     read <$> munch1 isDigit
          seriesP = do  s <- numP `sepBy` char ','
                        void $ char '\n'
                        return s

printLT :: PrintLT -> Int -> Int -> Bool
printLT lts a b | (a,b) `member` lts  = True
                | (b,a) `member` lts  = False
                | otherwise           = a <= b

findPrintSorted :: Print -> [Series]
findPrintSorted = filter <$> isSorted . lt <*> series
  where isSorted lts = and . (zipWith (printLT lts) <*> tail)

main :: IO ()
main = interact $ show . sum . map middle . findPrintSorted . read
