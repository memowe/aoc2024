import Data.Char
import Data.Maybe
import Text.ParserCombinators.ReadP

newtype Instr   = Mul [Int]                 deriving Show
newtype Program = Prg {instrs :: [Instr]}   deriving Show

instance Read Program where
  readsPrec _ = readP_to_S (Prg <$> instrsP)
    where instrsP   = do  is <- many $ (Just <$> instrP) <++ (Nothing <$ get)
                          eof
                          return $ catMaybes is
          instrP    = choice [mulP]
          mulP      = Mul <$> between (string "mul(") (char ')')
                              (numP `sepBy1` char ',')
          numP      = read <$> munch1 isDigit

eval :: Program -> Int
eval = sum . map evalInstr . instrs

evalInstr :: Instr -> Int
evalInstr (Mul ns) = product ns

main :: IO ()
main = interact $ show . eval . read
