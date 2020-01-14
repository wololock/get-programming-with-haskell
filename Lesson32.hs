import Control.Monad (guard)

powersOfTwoMap :: Int -> [Int]
powersOfTwoMap n = map (\x -> 2^x) [1..n]

powersOfTwo :: Int -> [Int]
powersOfTwo n = do
  value <- [1..n]
  return (2^value)

allEvenOds :: Int -> [(Int,Int)]
allEvenOds n = do
  evenValue <- [2,4..n]
  oddValue <- [1,3..n]
  return (evenValue,oddValue)

valAndSquare :: [(Int,Int)]
valAndSquare = do
  val <- [1..10]
  return (val,val^2)

evensGuard :: Int -> [Int]
evensGuard n = do
  value <- [1..n]
  guard (even value)
  return value

guardFilter :: (a -> Bool) -> [a] -> [a]
guardFilter f xs = do
  x <- xs
  guard (f x)
  return x

evensGuard' :: Int -> [Int]
evensGuard' n = [val | val <- [1..n], even value]

