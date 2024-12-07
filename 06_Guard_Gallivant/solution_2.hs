{-# LANGUAGE TemplateHaskell #-}

import Data.Bifunctor
import qualified Data.Set as S
import qualified Data.Map as M
import Text.ParserCombinators.ReadP
    (char, choice, many, readP_to_S, string)
import Control.Monad.RWS
import Control.Monad.Extra
import Control.Lens

allEnum :: Enum a => [a]
allEnum = enumFrom (toEnum 0)

csucc :: (Eq a, Enum a, Bounded a) => a -> a
csucc x | x == maxBound = minBound
        | otherwise     = succ x

type    Col           = Int
type    Row           = Int
type    Coord         = (Col, Row)
data    Dir           = Up | Rgt | Dwn | Lft      deriving (Eq, Enum, Bounded, Ord)
data    Entity        = Mpty | Wall | Guard Dir  deriving (Eq)
newtype Field         = Field {field :: M.Map Coord Entity}
type    GuardData     = (Dir, Coord)
type    GuardWalking  = Bool
data    GuardState    = GS
  { _gData  :: GuardData
  , _seen   :: S.Set GuardData
  }

makeLenses ''GuardState

instance Show Dir where
  show dir = return $ "^>v<" !! fromEnum dir

instance Show Entity where
  show Mpty = "."; show Wall = "#"; show (Guard g) = show g

instance Read Field where
  readsPrec _ = readP_to_S $ Field . M.fromList <$> rowsP
    where rowsP   = coords . zip [0..] <$> many (zip [0..] <$> rowP)
          coords  = concatMap (uncurry $ map . first . flip (,))
          rowP    = many entityP <* char '\n'
          entityP = choice
            [ Mpty  <$  string (show Mpty)
            , Wall  <$  string (show Wall)
            , Guard <$> choice (map ((<$) <*> string . show) allEnum)
            ]

next :: Dir -> Coord -> Coord
next Up  (x,y) = (x,y-1)
next Rgt (x,y) = (x+1,y)
next Dwn (x,y) = (x,y+1)
next Lft (x,y) = (x-1,y)

turn :: Dir -> Dir
turn = csucc

extractGuard :: Field -> (Field, GuardData)
extractGuard (Field m) = case M.toList (M.filter isGuard m) of
  [(coord, Guard dir)]  ->  let m' = M.update (const $ Just Mpty) coord m
                            in  (Field m', (dir, coord))
  _                     ->  error "Guard not found"
  where isGuard (Guard _) = True; isGuard _ = False

type Action = RWS Field [Coord] GuardState

step :: Action GuardWalking
step = do
  (seen %=) . S.insert =<< use gData
  coord   <- gData `uses` uncurry next
  inField <- asks ((coord `M.member`) . field)
  if inField then do
    isWall <- asks $ (== Wall) . (M.! coord) . field
    if isWall then gData._1 %= turn
              else gData._2 .= coord
    return True
    else return False

findSeen :: Action Bool
findSeen = do
  walking <- step
  if not walking
    then return False
    else do
      beenThere <- uses seen . S.member =<< use gData
      if beenThere then return True else findSeen

checkLoop :: Action ()
checkLoop = do
  backup <- get
  gData._1 %= turn
  whenM findSeen $ tell [uncurry next $ backup^.gData]
  put backup

walkGuard :: Field -> GuardData -> [Coord]
walkGuard f g = snd $ evalRWS walk f initState
  where initState = GS g S.empty
        walk      = whileM (checkLoop >> step)

main :: IO ()
main = interact $ show . length . uncurry walkGuard . extractGuard. read
