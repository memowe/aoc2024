{-# LANGUAGE FlexibleInstances #-}

import Data.List
import Data.Bifunctor
import qualified Data.Map as M
import Text.ParserCombinators.ReadP
    (char, choice, many, readP_to_S, string)
import Control.Monad.RWS
import Control.Monad.Extra

allEnum :: Enum a => [a]
allEnum = enumFrom (toEnum 0)

csucc :: (Eq a, Enum a, Bounded a) => a -> a
csucc x | x == maxBound = minBound
        | otherwise     = succ x

type    Col           = Int
type    Row           = Int
type    Coord         = (Col, Row)
data    Dir           = Up | Rgt | Dwn | Lft      deriving (Eq, Enum, Bounded)
data    Entity        = Empty | Wall | Guard Dir  deriving (Eq)
newtype Field         = Field {field :: M.Map Coord Entity}
type    GuardData     = (Dir, Coord)
type    GuardWalking  = Bool

instance Show Dir where
  show dir = return $ "^>v<" !! fromEnum dir

instance Show Entity where
  show Empty = "."; show Wall = "#"; show (Guard g) = show g

instance Read Field where
  readsPrec _ = readP_to_S $ Field . M.fromList <$> rowsP
    where rowsP   = coords . zip [0..] <$> many (zip [0..] <$> rowP)
          coords  = concatMap (uncurry $ map . first . flip (,))
          rowP    = many entityP <* char '\n'
          entityP = choice
            [ Empty <$  string (show Empty)
            , Wall  <$  string (show Wall)
            , Guard <$> choice (map ((<$) <*> string . show) allEnum)
            ]

nextCoord :: Dir -> Coord -> Coord
nextCoord Up  (x,y) = (x,y-1)
nextCoord Rgt (x,y) = (x+1,y)
nextCoord Dwn (x,y) = (x,y+1)
nextCoord Lft (x,y) = (x-1,y)

extractGuard :: Field -> (Field, GuardData)
extractGuard (Field m) = case M.toList (M.filter isGuard m) of
  [(coord, Guard dir)]  ->  let m' = M.update (const $ Just Empty) coord m
                            in  (Field m', (dir, coord))
  _                     ->  error "Guard not found"
  where isGuard (Guard _) = True; isGuard _ = False

stepGuard :: RWS Field [Coord] GuardData GuardWalking
stepGuard = do
  c <- gets $ uncurry nextCoord
  ifM (asks $ not . (c `M.member`) . field)
    (return False) $ do
    ifM (asks $ (== Wall) . (M.! c) . field)
      (True <$ modify (first csucc)) $ do
      modify (second (const c))
      tell [c]
      return True

walkGuard :: Field -> GuardData -> [Coord]
walkGuard f gd = snd $ evalRWS walk f gd
  where walk = do
          tell [snd gd]
          whileM stepGuard

main :: IO ()
main = interact $ show . length . nub . uncurry walkGuard . extractGuard. read
