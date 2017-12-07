import System.IO
import Data.List
import Data.Array
import qualified Data.Set as Set
import Data.Maybe

findMaxIdx :: [Int] -> Int
findMaxIdx xs = (0 -) . snd . maximum $ zip xs [0,-1..]

redistribute :: [Int] -> [Int]
redistribute xs = [xsWithoutLeader !! i + redistributedAmts !! i | i <- indexes]
  where maxIdx = findMaxIdx xs
        maxAmt  = xs !! maxIdx
        indexes = [0..(length xs - 1)]
        xsWithoutLeader = [ if i == maxIdx then 0 else xs !! i | i <- indexes ]
        redistributedAmts = map (redistributionShare maxIdx maxAmt (length xs)) indexes

redistributionShare :: Int -> Int -> Int -> Int -> Int
redistributionShare inIdx inAmt nSharers outIdx = allAmt + myAmt
    where allAmt = inAmt `div` nSharers
          finalDistLen = inAmt `mod` nSharers
          distInToOut = (outIdx + nSharers - inIdx) `mod` nSharers
          myAmt = if distInToOut /= 0 && finalDistLen >= distInToOut
                  then 1
                  else 0

findDistributionRounds :: Int -> Set.Set [Int] -> [Int] -> Int
findDistributionRounds rounds seen xs 
    | Set.member xs seen = 0
    | otherwise = 1 + findDistributionRounds (rounds+1) newSeen redistributed
         where redistributed = redistribute xs
               newSeen        = Set.insert xs seen

findCycleLength :: [[Int]] -> [Int] -> Int
findCycleLength seen xs 
    | xs `elem` seen = (1 +) $ fromJust $ elemIndex xs seen
    | otherwise      = findCycleLength newSeen redistributed
        where newSeen       = xs : seen
              redistributed = redistribute xs
                      

main :: IO ()
main = do
    content <- readFile "input"
    let banks = map (read::String->Int) . words $ content
        rounds = findDistributionRounds 0 Set.empty banks
        cycleLength = findCycleLength [] banks

    print banks
    print rounds
    print cycleLength
