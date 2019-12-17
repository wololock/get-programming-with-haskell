
import qualified Data.Map as Map


data Box a = Box a deriving Show

wrap :: a -> Box a
wrap x = Box x

unwrap :: Box a -> a
unwrap (Box x) = x

data Triple a = Triple a a a deriving Show

type Point3D = Triple Double

first :: Triple a -> a
first (Triple x _ _) = x

second :: Triple a -> a
second (Triple _ x _) = x

third :: Triple a -> a
third (Triple _ _ x) = x

toList :: Triple a -> [a]
toList (Triple x y z) = [x,y,z]

transform :: (a -> a) -> Triple a -> Triple a
transform f (Triple x y z) = Triple (f x) (f y) (f z)


data List a = Empty | Cons a (List a) deriving Show

lmap :: (a -> b) -> List a -> List b
lmap _ Empty = Empty
lmap f (Cons x xs) = Cons (f x) (lmap f xs)

-- 18.2.3. Data.Map

data Organ = Heart | Brain | Kidney | Spleen deriving (Show,Eq,Ord,Enum)

organs :: [Organ]
organs = [Heart,Heart,Brain,Spleen,Spleen,Kidney]

ids :: [Int]
ids = [2,7,13,14,21,24]

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList (zip ids organs)

-- Q18.1

tripleMap :: (a -> b) -> Triple a -> Triple b
tripleMap f (Triple x y z) = Triple (f x) (f y) (f z)

boxMap :: (a -> b) -> Box a -> Box b
boxMap f (Box x) = Box (f x)

-- Q18.2

organInventory :: Map.Map Organ Int
organInventory = Map.fromList (zip organs counts)
  where organs = [Heart .. Spleen]
        counts = map (\organ -> length $ Map.filter (==organ) organCatalog) organs
        