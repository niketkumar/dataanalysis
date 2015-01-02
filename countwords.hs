import Control.Exception (catch, SomeException)
import System.Environment (getArgs)

main :: IO ()
main = do   args <- getArgs
            let filename = case args of 
                            (a:_) -> a 
                            _ -> "data/plato-quote.txt"
            content <- catch (readFile filename) 
                        $ \err -> do print (err::SomeException)
                                     return ""
            print $ countWords content

countWords :: String -> [Int]
countWords str = map (length.words) (lines str)