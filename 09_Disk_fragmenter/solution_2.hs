{-# LANGUAGE TemplateHaskell #-}

import Data.Char
import Data.Maybe
import Data.Sequence (Seq(..), ViewL(..), ViewR(..))
import qualified Data.Sequence as S
import Control.Monad.State
import Control.Monad.Extra
import Control.Lens (view, use, uses, (%=), (.=), (+=), makeLenses)

type File     = Int
type Length   = Int
type Pointer  = Int
type Running  = Bool
type Disk     = Seq (Maybe File, Length)
data FSState  = FSS { _pointer  :: Pointer
                    , _disk     :: Disk
                    } deriving Show

makeLenses ''FSState

toDisk :: [Int] -> Disk
toDisk = S.fromList . zip (concatMap (\i -> [Just i, Nothing]) [0..])

type FSAction = State FSState

step :: FSAction Running
step = do
  p <- use pointer
  if p < 0
    then return False
    else do
      (activeDisk, done) <- disk `uses` S.splitAt p
      case S.viewr activeDisk of
        EmptyR            -> void $ return "Impossible"
        (before :> (mf, len))  -> do
          when (isJust mf) $ do
            let (front, fs) = S.breakl (\f -> isNothing (fst f) && (>= len) (snd f)) before
            case S.viewl fs of
              ((_,l) :< rest) -> do
                rest' <- if l == len
                          then return rest
                          else do pointer += 1
                                  return $ (Nothing,l-len) :<| rest
                disk .= mconcat [ front
                                , S.singleton (mf, len), rest'
                                , S.singleton (Nothing, len), done
                                ]
              EmptyL -> void $ return "No hole found"
      pointer %= pred
      return True

defrag :: Disk -> Disk
defrag = view disk . execState (whileM step) . initState
  where initState = FSS =<< S.length

expand :: Disk -> [Maybe File]
expand = concatMap $ uncurry $ flip replicate

checksum :: [Maybe File] -> Int
checksum = sum . zipWith (*) [0..] . map (fromMaybe 0)

main :: IO ()
main = interact $ show
        . checksum . expand . defrag
        . toDisk . map digitToInt . head . lines
