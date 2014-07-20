data IntList = Empty | Cons Int IntList
  deriving Show

absAll :: IntList -> IntList
absAll Empty       = Empty
absAll (Cons x xs) = Cons (abs x) (absAll xs)

squareAll :: IntList -> IntList
squareAll Empty       = Empty
squareAll (Cons x xs) = Cons (x*x) (squareAll xs)

exampleList = Cons (-1) (Cons 2 (Cons (-6) Empty))

addOne x = x + 1
square x = x * x

mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList _ Empty       = Empty
mapIntList f (Cons x xs) = Cons (f x) (mapIntList f xs)

keepOnlyPositive :: IntList -> IntList
keepOnlyPositive Empty = Empty
keepOnlyPositive (Cons x xs)
      | x > 0 = Cons x (keepOnlyPositive xs)
      | otherwise = keepOnlyPositive xs

keepOnlyEven :: IntList -> IntList
keepOnlyEven Empty = Empty
keepOnlyEven (Cons x xs)
      | even x = Cons x (keepOnlyEven xs)
      | otherwise = keepOnlyEven xs

filterIntList :: (Int->Bool) -> IntList -> IntList
filterIntList _ Empty = Empty
filterIntList p (Cons x xs)
      | p x    = Cons x (filterIntList p xs)
      | otherwise = filterIntList p xs

positive x = x > 0

divBy2reminder x = x `mod` 2

even2 :: Int -> Bool
even2 x
    | divBy2reminder x == 0 = True
    | otherwise = False
