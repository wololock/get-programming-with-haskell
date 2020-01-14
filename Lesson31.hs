import qualified Data.Map as Map

askForName :: IO ()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

helloName :: IO ()
helloName = askForName >>
            getLine >>= 
            (\name -> 
              return (nameStatement name)) >>=
            putStrLn

maxPairM :: (Monad m, Ord a) => m (a,a) -> m a
maxPairM pair = pair >>= (\(x,y) -> return (max x y))

helloNameDo :: IO ()
helloNameDo = do
  askForName
  name <- getLine
  putStrLn (nameStatement name)


echo :: IO ()
echo = do
  line <- getLine
  putStrLn line

-- 31.2.1

data Grade = F | D | C | B | A deriving (Eq,Ord,Enum,Show,Read)

data Degree = HS | BA | MS | PhD deriving (Eq,Ord,Enum,Show,Read)

data Candidate = Candidate 
  { candidateId :: Int
  , codeReview :: Grade
  , cultureFit :: Grade
  , education :: Degree } deriving Show

viable :: Candidate -> Bool
viable candidate = all (==True) tests
  where passedCoding = codeReview candidate > B
        passedCultureFit = cultureFit candidate > C
        educationMin = education candidate >= MS
        tests = [passedCoding, passedCultureFit, educationMin]

testCandidate :: Candidate
testCandidate = Candidate
  { candidateId = 1
  , codeReview = A
  , cultureFit = A
  , education = PhD }

readInt :: IO Int
readInt = getLine >>= (return . read)

readGrade :: IO Grade 
readGrade = getLine >>= (return . read)

readDegree :: IO Degree
readDegree = getLine >>= (return . read)

readCandidate :: IO Candidate
readCandidate = do
  putStrLn "Enter ID:"
  cId <- readInt
  putStrLn "Enter code grade:"
  codeGrade <- readGrade
  putStrLn "Enter culture fit grade:"
  cultureGrade <- readGrade
  putStrLn "Enter education:"
  degree <- readDegree
  return (Candidate { candidateId = cId
                    , codeReview = codeGrade
                    , cultureFit = cultureGrade 
                    , education = degree })

assessCandidateIO :: IO String
assessCandidateIO = do
  candidate <- readCandidate
  let passed = viable candidate
  let statement = if passed then "passed" else "failed"
  return statement

candidate1 :: Candidate
candidate1 = Candidate { candidateId = 1 
                       , codeReview = A 
                       , cultureFit = A 
                       , education = BA }

candidate2 :: Candidate
candidate2 = Candidate { candidateId = 2 
                       , codeReview = C 
                       , cultureFit = A 
                       , education = PhD }

candidate3 :: Candidate
candidate3 = Candidate { candidateId = 3 
                       , codeReview = A 
                       , cultureFit = B 
                       , education = MS }

candidateDB :: Map.Map Int Candidate
candidateDB = Map.fromList candidates
  where candidates = map toPair [candidate1,candidate2,candidate3]
        toPair candidate = (candidateId candidate, candidate)


assessCandidateMaybe :: Int -> Maybe String
assessCandidateMaybe cId = do
  candidate <- Map.lookup cId candidateDB
  let passed = viable candidate
  let statement = if passed then "passed" else "failed"
  return statement

failPassOrElse :: Maybe String -> String
failPassOrElse Nothing = "Error: ID not found"
failPassOrElse (Just val) = val

candidates :: [Candidate]
candidates = [candidate1,candidate2,candidate3]

assessCandidateList :: [Candidate] -> [String]
assessCandidateList candidates = do
  candidate <- candidates
  let passed = viable candidate
  let statement = if passed then "passed" else "failed"
  return statement

assessCandidate :: Monad m => m Candidate -> m String
assessCandidate candidates = do
  candidate <- candidates
  let passed = viable candidate
  let statement = if passed then "passed" else "failed"
  return statement
  