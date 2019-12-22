import System.IO

readAndWrite :: IO ()
readAndWrite = do
  helloFile <- openFile "hello.txt" ReadMode
  firstLine <- hGetLine helloFile
  putStrLn firstLine
  secondLine <- hGetLine helloFile
  goodbyeFile <- openFile "goodbye.txt" WriteMode
  hPutStrLn goodbyeFile secondLine
  hClose helloFile
  hClose goodbyeFile
  putStrLn "Done!"

getCounts :: String -> (Int,Int,Int)
getCounts input = (charCount, wordCount, lineCount)
  where
    charCount = length input
    wordCount = (length . words) input
    lineCount = (length . lines) input

countsText :: (Int,Int,Int) -> String
countsText (cc,wc,lc) = unwords ["chars:"
                          , show cc
                          , "words:"
                          , show wc 
                          , "lines:"
                          , show lc]

main :: IO ()
main = do
  let fileName = "hello.txt"
  file <- openFile fileName ReadMode
  input <- hGetContents file
  let summary = (countsText . getCounts) input
  putStrLn summary
  hClose file
  appendFile "stats.dat" (mconcat [fileName, " ", summary, "\n"])
