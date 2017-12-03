findLayer :: Integer -> Integer
findLayer = (+ 1) . (* 2) . (`div` 2) . (+ 1) . floor . sqrt . fromIntegral . (subtract 1)

findDistanceY :: Integer -> Integer
findDistanceY = (`div` 2) . (subtract 1) . findLayer

findDistanceX :: Integer -> Integer
findDistanceX 1 = 0
findDistanceX x = rookDist
    where 
    layer = findLayer x
    patron = layer ^ 2
    rook = patron - (layer `div` 2)
    rookDistPos = abs $ (rook - x) `mod` (layer - 1)
    rookDistNeg = abs $ (x - rook) `mod` (layer - 1)
    rookDist = min rookDistPos rookDistNeg

findDistance :: Integer -> Integer
findDistance x = findDistanceX x + findDistanceY x

main :: IO ()
main = do
--    print [(x, findDistanceX x) | x <- [1..26]]
    print $ findDistance 347991


-- Part 2 -- NO
-- computeRestOfRow :: [Integer] -> [Integer] -> Integer
-- computeRestOfRow [] adjacent = [adjacent !! 0 + adjacent !! 1]
-- computeRestOfRow inprog adjacent = 
-- 
-- computeNextRow :: [Integer] -> [Integer] -> [Integer]
-- computeNextRow prevRow innerRow = [adjacentRow
--      where 
--      adjacentRow = computeAdjacentRow prevRow innerRow
-- 
-- computeAdjacentRow :: [Integer] -> [Integer] -> [Integer]
-- computeAdjacentRow [] innerRow = [0] ++ innerRow ++ [0]
-- computeAdjacentRow prevRow innerRow = [prevRow !! length prevRow - 2] ++ innerRow ++ [0]
-- 
-- computeNextShell :: [[Integer]] -> [[Integer]]
-- computeNextShell prev = 
