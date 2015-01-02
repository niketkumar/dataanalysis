import System.Directory (doesFileExist)
import System.Environment (getArgs)
import Text.CSV

main :: IO ()
main = do args <- getArgs
          let csvfile = case args of
                            [] -> "data/ages.csv"
                            (a:_) -> a
          exists <- doesFileExist csvfile
          content <- if exists then readFile csvfile else return ""
          let csv = parseCSV csvfile content
          let result = case csv of
                            Left err -> show err
                            Right recs -> (show.findOldest.tail) recs
          putStrLn result 

findOldest :: [Record] -> Record
findOldest [] = []
findOldest xs = foldl1 (\a x -> if age x > age a then x else a) xs

age :: Record -> Int
age [] = 0
age (n:[]) = 0
age (n:as) = toInt $ head as

toInt :: String -> Int
toInt = read
