{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Data.Semigroup

aWord :: T.Text
aWord = "Cheese"

sampleInput :: T.Text
sampleInput = "this\nis\ninput"


main :: IO ()
main = do
  print $ T.lines sampleInput
  print $ T.splitOn "\n" sampleInput
  print $ T.splitOn "i" sampleInput
  print $ T.unlines $ T.lines sampleInput
  print $ mconcat ["some", " ", "text"]
  print $ "some" <> " " <> "text"
  