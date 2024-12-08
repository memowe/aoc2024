import Data.Char
import Text.ParserCombinators.ReadP
import Control.Monad

data Equation = Equation  { result  :: Int
                          , nums    :: [Int]
                          } deriving Show

instance Read Equation where
  readsPrec _ = readP_to_S $ do
    res <- num
    void $ string ": "
    ns  <- num `sepBy1` char ' '
    return $ Equation res ns
    where num = read <$> munch1 isDigit

checkEquation :: Equation -> Bool
checkEquation (Equation r ns) = r `elem` calculate (reverse ns)

calculate :: [Int] -> [Int]
calculate []      = error "no operands"
calculate [n]     = [n]
calculate (n:ns)  = concatMap calc [(+), (*), dadd]
  where calc op     = op n <$> calculate ns
        a `dadd` b  = read $ show b ++ show a

main :: IO ()
main = interact $ show . sum . map result . filter checkEquation . map read . lines
