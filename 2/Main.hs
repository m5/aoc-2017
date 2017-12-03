import System.IO
import Control.Monad
import Data.Char (digitToInt)
import Data.Maybe
import qualified Data.Text as T
import qualified Control.Foldl as Fold

maybe_subtract :: (Maybe Integer, Maybe Integer) -> Integer
maybe_subtract (Just x, Just y) = x - y
maybe_subtract _ = 0

find_divisor :: Integer -> [Integer] -> Maybe Integer
find_divisor x xs = maybe_head $ filter (\z -> (z `mod` x == 0)) . filter (\z -> z /= x) $ xs

maybe_head :: [Integer] -> Maybe Integer
maybe_head [] = Nothing
maybe_head (x:xs) = Just x

find_divisors :: [Integer] -> (Integer, Integer)
find_divisors xs = head . map (\(x,d) -> (x, fromJust d)) . filter (\(x,d) -> isJust d) . map (\x -> (x, find_divisor x xs)) $ xs


main :: IO ()
main = do
    content <- readFile "input"
    let matrix = map (map (read::String->Integer)) . map words . lines $ content
        minmax = map (Fold.fold ((,) <$> Fold.maximum <*> Fold.minimum)) $ matrix
        deltas = map maybe_subtract minmax
        chksum = sum deltas

        divisors = map find_divisors matrix
        multiples = map (\(d,n) -> (n `div` d)) divisors
        multsum = sum multiples
        


    print matrix
    print deltas
    print chksum
    print divisors
    print multiples
    print multsum
