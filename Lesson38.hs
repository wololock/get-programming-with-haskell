
myHead :: [a] -> a
myHead []    = error "Empty list"
myHead (x:_) = x

maybeHead :: [a] -> Maybe a
maybeHead []    = Nothing
maybeHead (x:_) = Just x

eitherHead :: [a] -> Either String a
eitherHead []    = Left "There is no head because the list is empty"
eitherHead (x:_) = Right x



primes :: [Int]
primes = sieve [2..10000]

sieve :: [Int] -> [Int]
sieve [] = []
sieve (nextPrime:rest) = nextPrime : sieve noFactors
  where noFactors = filter (not . (==0) . (`mod` nextPrime)) rest

data PrimeError = TooLarge | InvalidValue

instance Show PrimeError where
  show TooLarge = "Value exceed max bound"
  show InvalidValue = "Value is not a valid candidate for prime checking"

isPrime :: Int -> Either PrimeError Bool
isPrime n 
  | n < 2     = Left InvalidValue
  | n > 1000  = Left TooLarge
  | otherwise = Right (n `elem` primes)
