{-# LANGUAGE TemplateHaskell #-}
import Data.Char
import Data.Maybe
import qualified Data.Array as A
import Data.Functor
import Control.Monad.Extra
import Control.Monad.State
import Control.Monad.Writer
import Control.Lens

type File         = Int
type Address      = Int
type Filesystem   = A.Array Address (Maybe File)
data DefragState  = DS  { _pointers   :: (Address, Address)
                        , _filesystem :: Filesystem
                        }

makeLenses ''DefragState

showFilesystem :: Filesystem -> String
showFilesystem = concatMap (maybe "." show) . A.elems

instance Show DefragState where
  show (DS (a,b) fs) = unlines
    [ showFilesystem fs
    , replicate a ' ' ++ "^" ++ replicate (b-a-1) ' ' ++ "^"
    ]


buildFilesystem :: [Int] -> Filesystem
buildFilesystem xs =
  let withHoles = expand xs
      len       = length withHoles
  in  A.listArray (0, len-1) withHoles
  where expand    = concat . zipWith (flip replicate) switchIdx
        switchIdx = concatMap (\i -> [Just i, Nothing]) [0..]

type FSAction = WriterT [File] (State DefragState)

prepare :: FSAction ()
prepare = whileM $ do
  fs    <- use filesystem
  here  <- (pointers._1) `uses` (fs A.!)
  if isJust here
    then (tell [fromJust here] >> pointers._1 %= succ) $> True
    else do
      there <- (pointers._2) `uses` (fs A.!)
      if isNothing there
        then (pointers._2 %= pred) $> True
        else return False

step :: FSAction ()
step = do
  fs      <- use filesystem
  (a, b)  <- use pointers
  let f = fromJust (fs A.! b)
  filesystem .= fs A.// [(a, Just f), (b, Nothing)]

running :: FSAction Bool
running = uncurry (<) <$> use pointers

defrag :: [Int] -> [Int]
defrag xs =
  let fs        = buildFilesystem xs
      initState = DS (A.bounds fs) fs
  in  evalState (execWriterT $ whileM go) initState
  where go = do prepare
                step
                running

checksum :: [Int] -> Int
checksum = sum . zipWith (*) [0..]

main :: IO ()
main = interact $ show . checksum . defrag . map digitToInt . head . lines
