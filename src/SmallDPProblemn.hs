module SmallDPProblemn where

import Control.Concurrent.STM.TVar (TVar)
import Control.Monad.Identity (Identity)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.State (State, evalState, get, put)
import Data.List (foldl', sortOn, subsequences)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map

type MyMemo = ReaderT (TVar (Map Int Int)) Identity

type MemoMap = Map Int Int

runCalc :: Int -> Int
runCalc x = evalState (calculate x) Map.empty

calculate :: Int -> State MemoMap Int
calculate 0 = return 1
calculate x | x < 0 = return 0
calculate x | x > 0 = do
  m <- get
  let maybeMemo = Map.lookup x m
  maybe
    ( do
        ones <- calculate (x -1)
        threes <- calculate (x -3)
        fives <- calculate (x -5)
        let result = ones + threes + fives
        let updatedMap = Map.insert x result m
        put updatedMap
        return $ ones + threes + fives
    )
    return
    maybeMemo

runBiggerProblem :: IO ()
runBiggerProblem = do
  biggerProblem "AEDFHR" "ABCDGH" >>= print
  biggerProblem "AGGTAB" "GXTXAYB" >>= print

biggerProblem :: String -> String -> IO String
biggerProblem l r = do
  let lSubSeqs = subsequences l
  --  print lSubSeqs
  let rSubSeqs = subsequences r
  --  print rSubSeqs
  let potentialSolutions = foldl' (\acc elem -> if elem `List.elem` rSubSeqs then elem : acc else acc) [] lSubSeqs
  --  print potentialSolutions
  let sorted = reverse $ sortOn length potentialSolutions
  --  print sorted
  if List.null sorted then return "" else return $ head sorted

longestIncreasingSubSequence :: [Int] -> [Int]
longestIncreasingSubSequence l =
  let subseqs = longestIncreasingSubSequence' l in
    let highToLow = reverse $ sortOn length subseqs in
      if List.null highToLow then [] else head highToLow

longestIncreasingSubSequence' :: [Int] -> [[Int]]
longestIncreasingSubSequence' [] = []
longestIncreasingSubSequence' (x : xs) =
  fst (foldl' (\(curList, last) elem -> if elem > last then (curList ++ [elem], elem) else (curList, last)) ([x], x) xs) : longestIncreasingSubSequence' xs


runLongestIncreasing :: IO()
runLongestIncreasing = do
  print $ longestIncreasingSubSequence [3, 10, 2, 1, 20]
  print $ longestIncreasingSubSequence [3, 2]
  print $ longestIncreasingSubSequence [50, 3, 10, 7, 40, 80]