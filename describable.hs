
class Describable a where
  describe :: a -> String

instance Describable Bool where
  describe v = "A member of the Bool class, " ++ show v ++ " is opposite to " ++ show (not v)

instance Describable Int where
  describe n = "A member of the Int class, the number after " ++ show (n-1) ++ " and before " ++ show (n+1)   


cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n = if n == maxBound then minBound else succ n
