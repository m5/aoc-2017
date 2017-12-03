import System.IO
import Control.Monad
import Data.Char (digitToInt)
import qualified Data.Text as T

rotate :: Int -> [a] -> [a]
rotate n xs = take (length xs) . drop n . cycle $ xs

areTwins :: (Int, Int) -> Bool
areTwins (a, b) = a == b

takeFirst :: (Int, Int) -> Int
takeFirst (a, b) = a

main :: IO ()
main = do
    handle <- openFile "input" ReadMode
    contents <- hGetContents handle
    let stripped = T.unpack . T.strip . T.pack $ contents
        digits = map digitToInt stripped
        zipped = zip digits (rotate 1 digits)
        digsum = sum . map takeFirst . filter areTwins $ zipped

        zipped2 = zip digits (rotate (length digits `div` 2) digits)
        digsum2 = sum . map takeFirst . filter areTwins $ zipped2

    print digsum
    print digsum2
    hClose handle
