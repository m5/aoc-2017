import System.IO
import Data.List
import Data.Array


insertAt :: a -> [a] -> Int -> [a]
insertAt x xs idx = let (left, right) = splitAt idx xs
                    in left ++ (x:right)

spinItUp :: Int -> Int -> Int -> Int -> [Int] -> [Int]
spinItUp max steps current idx buffer 
    | current > max = buffer
    | otherwise     = spinItUp max steps (current+1) newIdx newBuffer 
    where newIdx = 1 + ((idx + steps) `mod` current)
          newBuffer = insertAt current buffer newIdx

limitedSpinItUp :: Int -> Int -> Int -> Int -> Int -> Int
limitedSpinItUp max steps current idx afterZero
    | current > max = afterZero
    | otherwise     = limitedSpinItUp max steps (current+1) newIdx newAfterZero 
        where newIdx = 1 + ((idx + steps) `mod` current)
              newAfterZero = if newIdx == 1
                             then current
                             else afterZero

findAfter :: Int -> [Int] -> Int
findAfter toFind (a:b:tail)
    | a == toFind = b
    | otherwise   = findAfter toFind (b:tail)

main :: IO ()
main = do
    let spun = spinItUp 2017 344 1 0 [0]
        spunMax = limitedSpinItUp 50000000 344 1 0 0
        demo = spinItUp 9 3 1 0 [0]
        nextOne = findAfter 2017 spun

    print spun
    print nextOne
    print spunMax
