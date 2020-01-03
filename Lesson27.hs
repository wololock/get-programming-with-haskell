
import qualified Data.Map as Map

reverseMaybe :: Maybe String -> Maybe String
reverseMaybe (Just s) = Just (reverse s)
reverseMaybe Nothing = Nothing

successfulRequest :: Maybe Int
successfulRequest = Just 6

failedRequest :: Maybe Int
failedRequest = Nothing

incMaybe :: Maybe Int -> Maybe Int
incMaybe (Just n) = Just (n + 1)
incMaybe Nothing = Nothing

successStr :: Maybe String
successStr = show <$> successfulRequest

failStr :: Maybe String
failStr = show <$> failedRequest

reverseMaybe' :: Maybe String -> Maybe String
reverseMaybe' str = reverse <$> str

data RobotPart = RobotPart 
  { name :: String 
  , description :: String
  , cost :: Double 
  , count :: Int
  } deriving Show


leftArm :: RobotPart
leftArm = RobotPart 
  { name = "left arm"
  , description = "left arm for face punching!"
  , cost = 1000.00
  , count = 3
  }

rightArm :: RobotPart
rightArm = RobotPart
  { name = "right arm"
  , description = "right amrm for kind hand gestures"
  , cost = 1025.00
  , count = 5
  }

robotHead :: RobotPart
robotHead = RobotPart
  { name = "robot head"
  , description = "this head looks mad"
  , cost = 5092.25
  , count = 2
  }

type Html = String

renderHtml :: RobotPart -> Html
renderHtml part = mconcat ["<h2>",
    partName,
    "</h2>",
    "<p><h3>desc</h3>",
    partDesc,
    "</p><p><h3>cost</h3>",
    partCost,
    "</p><p><h3>count</h3>",
    partCount,
    "</p>"
  ]
  where partName = name part
        partDesc = description part
        partCost = show (cost part)
        partCount = show (count part)

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
  where keys = [1,2,3]
        vals = [leftArm, rightArm, robotHead]
        keyVals = zip keys vals


partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB

partHtml :: Maybe Html
partHtml = renderHtml <$> partVal

allParts :: [RobotPart]
allParts = snd <$> Map.toList partsDB

allPartsHtml :: [Html]
allPartsHtml = renderHtml <$> allParts

htmlPartsDB :: Map.Map Int Html
htmlPartsDB = renderHtml <$> partsDB

leftArmIO :: IO RobotPart
leftArmIO = return leftArm

htmlSnippet :: IO Html
htmlSnippet = renderHtml <$> leftArmIO

-- Q27.1
data Box a = Box a deriving Show

instance Functor Box where
  fmap func (Box a) = Box (func a)

morePresents :: Box a -> Int -> Box [a]
morePresents box n = take n <$> repeat <$> box

-- Q27.2
myBox :: Box Int
myBox = Box 1

wrapped = Box <$> myBox

unwrap :: Box a -> a
unwrap (Box a) = a

-- Q27.3

checkCost :: Int -> Maybe Double
checkCost id = cost <$> Map.lookup id partsDB
