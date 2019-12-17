import Data.Semigroup

data Color = Red | Yellow | Blue | Green | Purple | Orange | Brown deriving (Show,Eq)

instance Semigroup Color where
  (<>) Red Blue = Purple
  (<>) Blue Red = Purple
  (<>) Yellow Blue = Green
  (<>) Blue Yellow = Green
  (<>) Yellow Red = Orange
  (<>) Red Yellow = Orange
  (<>) a b | a == b = a
           | all (`elem` [Red,Blue,Purple]) [a,b] = Purple
           | all (`elem` [Blue,Yellow,Green]) [a,b] = Green
           | all (`elem` [Red,Yellow,Orange]) [a,b] = Orange
           | otherwise = Brown

instance Semigroup Integer where
  (<>) a b = a + b

instance Monoid Integer where
  mempty = 0
  mappend x y = x + y


-- 17.3.3. Practical Monoids—building probability tables

type Events = [String]
type Probs = [Double]

data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normalizedProbs
  where totalProbs = sum probs
        normalizedProbs = map (\x -> x / totalProbs) probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

instance Show PTable where
  show (PTable events probs) = mconcat pairs
    where pairs = zipWith showPair events probs


cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine f xs ys = zipWith f xs' ys'
  where size = length ys
        repeated = map (take size . repeat) xs
        xs' = mconcat repeated
        ys' = cycle ys

combineEvents:: Events -> Events -> Events
combineEvents = cartCombine combiner
  where combiner = (\x y -> mconcat [x,"-",y])

combineProbs :: Probs -> Probs -> Probs
combineProbs = cartCombine (*)

instance Semigroup PTable where
  (<>) pt1 (PTable [] []) = pt1
  (<>) (PTable [] []) pt2 = pt2
  (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
    where newEvents = combineEvents e1 e2
          newProbs = combineProbs p1 p2

instance Monoid PTable where
  mempty = PTable [] []
  mappend = (<>)

coin :: PTable
coin = createPTable ["heads","tails"] [0.5,0.5]

spinner :: PTable
spinner = createPTable ["red","blue","green"] [0.1,0.2,0.7]

