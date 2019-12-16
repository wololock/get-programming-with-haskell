
import Debug.Trace

take' 0 _      = []
take' n (x:xs) = x : take' (n-1) xs

take'' n xs = if n == 0 then [] else head xs : take'' (n-1) (tail xs)

take''' n xs | n == 0    = []
             | otherwise = head xs : take''' (n-1) (tail xs)

myTail (_:xs) = xs

drop' n xs = case n of
  0 -> xs
  _ -> drop' (n-1) (tail xs)


myLength xs = case xs of
  []     -> 0
  (x:xs) -> 1 + myLength xs


fib n = fib' 1 1 n
  where
    -- fib' x y z | trace ("fib " ++ show x ++ " " ++ show y ++ " " ++ show z) False = undefined
    fib' x y z = case z of
      0  -> 0
      1  -> y
      _  -> fib' (x+y) x (z-1)

fibR 0 = 0
fibR 1 = 1
fibR n = fibR (n-1) + fibR (n-2)

remove p xs = case xs of
  []     -> []
  (x:xs) -> if p x then remove p xs else x : remove p xs


myProduct xs = foldl (*) 1 xs

elem' x xs = length xs' > 0
  where
    xs' = filter (==x) xs

data Person = Person { name :: String, age :: Int } deriving (Show)       

john = Person { name = "John", age = 23 }

hi :: Person -> String
hi person = "Hi, " ++ name person

makeOlder :: Person -> Person
makeOlder person = person { age = (age person) + 1 }



main :: IO()
main = print 10