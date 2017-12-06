import System.IO
import Data.List
import Data.Array

escape :: (Int -> Int) -> Int -> Int -> Array Int Int -> Int
escape incFn levels position jumps
    | position >= length jumps = levels
    | position < 0             = levels
    | otherwise                = escape incFn (levels + 1) newPos newJumps
          where newPos = position + jumps ! position
                newOffset = incFn (jumps ! position)
                newJumps = jumps // [(position, newOffset)]

incPart1 :: Int -> Int
incPart1 = (+ 1)

incPart2 :: Int -> Int
incPart2 x
    | x < 3     = x + 1
    | otherwise = x - 1


main :: IO ()
main = do
    content <- readFile "input"
    let jumps = map (read::String->Int) . words $ content
        njumps = length jumps
        escapePart = \incFn -> escape incFn 0 0 (listArray (0, njumps - 1) jumps)
    print jumps
    print (escapePart incPart1)
    print (escapePart incPart2)
