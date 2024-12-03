import Data.Char
import Data.Maybe
import Text.ParserCombinators.ReadP

data    Instr   = Mul [Int] | Do | Dont     deriving Show
newtype Program = Prg {instrs :: [Instr]}   deriving Show

instance Read Program where
  readsPrec _ = readP_to_S (Prg <$> instrsP)
    where instrsP   = do  is <- many $ (Just <$> instrP) <++ (Nothing <$ get)
                          eof
                          return $ catMaybes is
          instrP    = choice [mulP, doP, dontP]
          mulP      = Mul   <$> between (string "mul(") (char ')')
                                (numP `sepBy1` char ',')
          doP       = Do    <$  string "do()"
          dontP     = Dont  <$  string "don't()"
          numP      = read  <$> munch1 isDigit

undoMul :: Program -> Program
undoMul = Prg . yope . instrs
  where yope (Dont : rest)  = nope rest
        yope (i:is)         = i : yope is
        yope []             = []
        nope (Do : rest)    = yope rest
        nope (_:is)         = nope is
        nope []             = []

eval :: Program -> Int
eval = sum . map evalInstr . instrs . undoMul

evalInstr :: Instr -> Int
evalInstr (Mul ns)  = product ns
evalInstr _         = 0

main :: IO ()
main = interact $ show . eval . read
