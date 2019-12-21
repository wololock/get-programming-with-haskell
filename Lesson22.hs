import System.Environment
import Control.Monad

simpleMain :: IO ()
simpleMain = do  
  args <- getArgs
  let linesToRead = if length args > 0
                    then read (head args)
                    else 0 :: Int
  putStrLn $ "Reading " ++ show linesToRead ++ " numbers:"                    
  numbers <- replicateM linesToRead getLine
  let ints = map read numbers :: [Int]
  putStrLn $ "Result: " ++ show (sum ints)

exampleMain :: IO ()
exampleMain = do
  vals <- mapM (\_ -> getLine) [1..3]
  mapM_ putStrLn vals

toInts :: String -> [Int]
toInts = map read . lines

lazyMain :: IO ()
lazyMain = do
  userInput <- getContents
  let numbers = toInts userInput
  print (sum numbers)

main :: IO ()
main = lazyMain
