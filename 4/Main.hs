import System.IO
import qualified Data.Set as Set
import Data.List

main :: IO ()
main = do
    content <- readFile "input"
    let passphrases = map words . lines $ content
        goodPhrases = filter checkPhraseUnique passphrases
        checkPhraseUnique phrase = length phrase == length (Set.fromList phrase)
        nanagramPhrases = filter checkPhraseUnique . map (map sort) $ passphrases

    print $ length goodPhrases
    print $ length nanagramPhrases
