module Lib where

import           Data.Semigroup
import           Prelude        hiding (even)

plusTwo :: [Int] -> [Int]
plusTwo = map (+2)

-- BLOCK 1

order3 :: Ord x => (x, x, x) -> (x, x, x)
order3 (x, y, z) | (x <= y) && (x <= z) = stageTwo x y z
                 | (y <= y) && (y <= z) = stageTwo y x z
                 | otherwise            = stageTwo z x y
    where
        stageTwo one two three | two <= three = (one, two, three)
                               | otherwise    = (one, three, two)

highestBit :: Int -> Int
highestBit n | n > 1     = 2 * highestBit(n `div` 2)
             | n == 1    = 1
             | otherwise = 0

smartReplicate :: [Int] -> [Int]
smartReplicate = concatMap (\x -> replicate x x)

contains :: Eq a => a -> [[a]] -> [[a]]
contains x = filter $ elem x

-- BLOCK 2

removeAt :: Int -> [a] -> [a]
removeAt _ []                   = []
removeAt n (x:xs) | n > 0       = x:removeAt (n-1) xs
                  | n == 0      = xs
                  | otherwise   = x:xs

collectEvery :: Int -> [a] -> ([a], [a])
collectEvery n list | n > 1 = helper (n - 1) (n - 1) list
                    | n == 1 = ([], list)
                    | otherwise = (list, [])
    where
        helper _      _  []              = ([], [])
        helper 0      m  (x:list')    = let (result, dropped) = helper m            m list' in (result, x:dropped)
        helper runner m  (x:list')    = let (result, dropped) = helper (runner - 1) m list' in (x:result, dropped)

stringSum :: String -> Int
stringSum string = sum $ map read $ words string

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)  | x <= y    = x : merge xs (y:ys)
                     | otherwise = y : merge(x:xs) ys

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = let (left, right) = splitAt (div (length xs) 2) xs
               in merge (mergeSort left) (mergeSort right)

-- BLOCK 3


data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Show, Eq, Ord)

daysInWeek :: Int
daysInWeek = 7

dayNumber :: DayOfWeek -> Int
dayNumber Mon = 0
dayNumber Tue = 1
dayNumber Wed = 2
dayNumber Thu = 3
dayNumber Fri = 4
dayNumber Sat = 5
dayNumber Sun = 6

toDay :: Int -> DayOfWeek
toDay 0 = Mon
toDay 1 = Tue
toDay 2 = Wed
toDay 3 = Thu
toDay 4 = Fri
toDay 5 = Sat
toDay 6 = Sun
toDay n = toDay $ n `mod` daysInWeek

isWeekend :: DayOfWeek -> Bool
isWeekend Sat = True
isWeekend Sun = True
isWeekend _   = False

afterDays :: Int -> DayOfWeek -> DayOfWeek
afterDays n d = toDay $ dayNumber d + n

nextDay :: DayOfWeek -> DayOfWeek
nextDay = afterDays 1

daysToParty :: DayOfWeek -> Int
daysToParty d = (dayNumber Fri - dayNumber d) `mod` daysInWeek

data Creature = Knight {att :: Integer, stamina :: Integer} |
              Monster {att :: Integer, stamina :: Integer} deriving (Show, Eq)

fight :: Creature -> Creature -> (Creature, Integer, Integer)
fight cr1@(Monster _ _) cr2@(Knight _ _)  = simulate cr2 cr1
fight cr1@(Knight _ _)  cr2@(Monster _ _) = simulate cr1 cr2
fight cr1 cr2                     = simulate cr1 cr2 

simulate :: Creature -> Creature -> (Creature, Integer, Integer)
simulate cr1 cr2 = proceedRound 1 (stamina cr1 - att cr2) (stamina cr2 - att cr1)
    where
        proceedRound i hp1 hp2  | hp2 <= 0 = (cr1, i, hp1 + att cr2)
                                | hp1 <= 0 = (cr2, i, hp2)
                                | otherwise         = proceedRound (i + 1) (hp1 - att cr2) (hp2 - att cr1)

data Vector a = Vector2D a a | Vector3D a a a

to3D :: a -> Vector a -> Vector a
to3D def (Vector2D x y) = Vector3D x y def
to3D _   b              = b

vectorLength :: Floating a => Vector a -> a
vectorLength v = stage2 (to3D 0 v)
    where
        stage2 (Vector3D x y z) = (** 0.5) $ sum $ map (** 2.0) [x, y, z]
        stage2 vv               = stage2 (to3D 0 vv)

vectorAdd :: Num a => Vector a -> Vector a -> Vector a
vectorAdd (Vector2D x y) (Vector2D x' y')      = Vector2D (x + x') (y + y')
vectorAdd (Vector3D x y z) (Vector3D x' y' z') = Vector3D (x + x') (y + y') (z + z')
vectorAdd v vv                                 = vectorAdd (to3D 0 v) (to3D 0 vv)

vectorScalar :: Num a => Vector a -> Vector a -> a
vectorScalar (Vector3D x y z) (Vector3D x' y' z') = x * x' + y * y' + z * z'
vectorScalar v vv                                 = vectorScalar (to3D 0 v) (to3D 0 vv)

distOfVectors :: Floating a => Vector a -> Vector a -> a
distOfVectors v vv = let
  (Vector3D x y z) = to3D 0 v
  (Vector3D x' y' z') = to3D 0 vv
  in vectorLength $ Vector3D (x - x') (y - y') (z - z')

vectorProd :: Floating a => Vector a -> Vector a -> Vector a
vectorProd v v' = let
  (Vector3D x y z) = to3D 0 v
  (Vector3D x' y' z') = to3D 0 v'
  in Vector3D (y * z' - z * y') (z * x' - x * z') (x * y' - y * x')

data Nat = Z | S Nat

instance Num Nat where
    (+) Z a     = a
    (+) (S a) b = S (a + b)

    (-) a Z         = a
    (-) Z _         = Z
    (-) (S a) (S b) = a - b

    (*) Z _     = Z
    (*) (S a) b = a * b + b


    fromInteger int | int <= 0  = Z
                    | otherwise = S . fromInteger $ int - 1

    negate _ = Z
    abs = id

    signum Z = Z
    signum _ = S Z

instance Eq Nat where
    (==) Z Z         = True
    (==) (S a) (S b) = a==b
    (==) _ _         = False

instance Ord Nat where
    compare Z Z         = EQ
    compare Z (S _)     = LT
    compare (S _) Z     = GT
    compare (S a) (S b) = compare a b

even :: Nat -> Bool
even Z     = True
even (S b) = not (even b)

natToInteger :: Nat -> Integer
natToInteger Z     = 0
natToInteger (S z) = 1 + natToInteger (z - 1)

data Tree a = Leaf | Node a (Tree a) (Tree a)

instance Show a => Show (Tree a) where
  show Leaf = "Leaf"
  show (Node x l r) = "(" ++ show l ++ ")" ++ " Node " ++ show x ++ " (" ++ show r ++ ")"

instance Eq a => Eq (Tree a) where
  (==) Leaf Leaf                    = True
  (==) (Node x l r) (Node x' l' r') = x == x' && l == l' && r == r'
  (==) _ _                          = False

empty :: Tree a -> Bool
empty Leaf   = True
empty Node{} = False

count :: Tree a -> Int
count Leaf         = 0
count (Node _ l r) = count l + count r + 1

find :: Ord a => a -> Tree a -> Bool
find _ Leaf = False
find x (Node y l r) | y == x    = True
                        | x < y     = find x l
                        | otherwise = find x r

insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Node x Leaf Leaf
insert x (Node y l r) | x == y    = Node x l r
                      | x < y     = Node y (insert x l) r
                      | otherwise = Node y l (insert x r)

fromList :: Ord a => [a] -> Tree a
fromList = foldr insert Leaf

instance Foldable Tree where
    foldr _ x Leaf          = x
    foldr fn x (Node n l r) = foldr fn (fn n $ foldr fn x r) l
    foldMap _ Leaf          = mempty
    foldMap fn (Node n l r) = foldMap fn l `mappend` fn n `mappend` foldMap fn r

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn comp = foldr helper [[]]
    where
        helper _ [] = []
        helper c list@(x:xs) | c == comp = []:list
                             | otherwise = (c : x) : xs


-- FINAL BLOCK --

maybeConcat :: (Monoid a) => [Maybe a] -> a
maybeConcat = foldr func mempty
    where
        func (Just y) x = y `mappend` x
        func Nothing  x = x

data NonEmpty a = a :| [a]

newtype Identity a = Identity { runIdentity :: a }

instance Semigroup (NonEmpty a) where
  (<>) (a :| as) (b :| bs) = a :| (b : as ++ bs)

instance (Semigroup a) => Semigroup (Identity a) where
  (<>) (Identity a) (Identity b) = Identity $ a <> b

instance (Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend (Identity a) (Identity b) = Identity $ a `mappend` b

instance (Ord a) => Monoid (Tree a) where
  mempty = Leaf
  mappend = foldr insert

instance (Ord a) => Semigroup (Tree a) where
  (<>) = mappend
