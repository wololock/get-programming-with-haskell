
data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show,Enum,Bounded)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN size c = toEnum rotation
  where half = size `div` 2
        offset = fromEnum c + half
        rotation = offset `mod` size

largestCharNumber :: Int
largestCharNumber = fromEnum (maxBound :: Char)

rotChar :: Char -> Char
rotChar c = rotN size c
  where size = 1 + largestCharNumber

message :: [FourLetterAlphabet]
message = [L1,L3,L4,L1,L1,L2]

fourLetterAlphabetEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterAlphabetEncoder vals = map rot4l vals
  where size = 1 + fromEnum (maxBound :: FourLetterAlphabet)
        rot4l = rotN size

rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n c = toEnum rotation
  where half = n `div` 2
        offset = if even n then fromEnum c + half else 1 + fromEnum c + half
        rotation = offset `mod` n

rotEncoder :: String -> String
rotEncoder text = map rotChar text
  where size = 1 + fromEnum (maxBound :: Char)
        rotChar = rotN size

rotDecoder :: String -> String
rotDecoder text = map rotCharDecoder text
  where size = 1 + fromEnum (maxBound :: Char)
        rotCharDecoder = rotNdecoder size