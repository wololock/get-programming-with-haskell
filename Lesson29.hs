
-- Q29.1 Use <$> and <*> to combine two Maybe String types with ++.
q29_1 :: Maybe String
q29_1 = (++) <$> Just "Lorem" <*> Just " ipsum"


-- Q29.2 Make the String "Hello World" into an IO String.
q29_2 :: IO String
q29_2 = pure "Hello World"


doorPrize :: [Int]
doorPrize = [1000,2000,300]

boxPrize :: [Int]
boxPrize = [500,20000]

totalPrize :: [Int] 
totalPrize = pure (+) <*> doorPrize <*> boxPrize

-- Q29.3 Solve this problem if the boxes contain a prize multiplier instead of just an additional prize. The multipliers are 10× and 50×.
totalPrize' :: [Int]
totalPrize' = pure (*) <*> doorPrize <*> [10,50]


primesToN :: Integer -> [Integer]
primesToN n = filter isNotComposite twoThroughN
  where twoThroughN = [2..n]
        composite = pure (*) <*> twoThroughN <*> twoThroughN
        isNotComposite = not . (`elem` composite)

data User = User {
  name :: String,
  gamerId :: Int,
  score :: Int  
} deriving Show

testNames :: [String]
testNames = ["John Smith", "Robert'); DROP TABLE Students;--", "Christina NULL", "Randal Munroe"]

testIds :: [Int]
testIds = [1337,0123,999999]

testScores :: [Int]
testScores = [0,100000,-99999]

testData :: [User]
testData = pure User <*> testNames <*> testIds <*> testScores

-- Q29.1

allFmap :: Applicative f => (a -> b) -> f a -> f b
allFmap f app = (pure f) <*> app

-- Q29.2
example :: Int
example = (*) ((+) 2 4) 6

exampleMaybe :: Maybe Int
exampleMaybe = pure (*) <*> (pure (+) <*> pure 2 <*> pure 4) <*> pure 6

-- Q29.3
beers :: [Int]
beers = (pure (-)) <*> needed <*> left
  where needed = (pure (*)) <*> [3,4] <*> ((+2) <$> [2,3])
        left = (\count -> count -4) <$> [6,12]