import Data.Char
import Data.List
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

printOrd :: PrintLT -> Int -> Int -> Ordering
printOrd lts a b  | a == b              = EQ
                  | (a,b) `member` lts  = LT
                  | (b,a) `member` lts  = GT
                  | otherwise           = compare a b

isPrintSorted :: PrintLT -> Series -> Bool
isPrintSorted lts = and . (zipWith (((/= GT) .) . printOrd lts) <*> tail)

sortPrintUnsorted :: Print -> [Series]
sortPrintUnsorted (P lts ss) =
  let unsorted = filter (not . isPrintSorted lts) ss
  in  map (sortBy $ printOrd lts) unsorted

main :: IO ()
main = interact $ show . sum . map middle . sortPrintUnsorted . read
