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

data List t = E | C t (List t)
    deriving Show

lst1 :: List Int
lst1 = C 3 (C 5 (C 2 E))

lst2 :: List Char
lst2 = C 'x' (C 'y' (C 'z' E))

lst3 :: List Bool
lst3 = C True (C False E)

filterList :: (t -> Bool) -> List t -> List t
filterList _ E = E
filterList p (C x xs)
      | p x         = C x (filterList p xs)
      | otherwise   = filterList p xs

mapList :: (t->t) -> List t -> List t
mapList _ E = E
mapList p (C x xs) = C (p x) (mapList p xs)

doStuff1 :: [Int] -> Int
doStuff1 []  = 0
doStuff1 [_] = 0
doStuff1 xs  = head xs + (head (tail xs)) 

doStuff2 :: [Int] -> Int
doStuff2 []        = 0
doStuff2 [_]       = 0
doStuff2 (x1:x2:_) = x1 + x2

data NonEmptyList a = NEL a [a]
   deriving Show
nelToList :: NonEmptyList a -> [a]
nelToList (NEL x xs) = x:xs

listToNel :: [a] -> Maybe (NonEmptyList a)
listToNel []     = Nothing
listToNel (x:xs) = Just $ NEL x xs

headNEL :: NonEmptyList a -> a
headNEL (NEL a _) = a

tailNEL :: NonEmptyList a -> [a]
tailNEL (NEL _ as) = as
