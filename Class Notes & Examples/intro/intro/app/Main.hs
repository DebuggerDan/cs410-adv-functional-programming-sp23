module Main where

{-
 multi-line
 comment
-}

-- single-line comment

-- const bool x = true;
-- x :: Bool
-- x = True

-- bool f(bool a) { return !a; }
-- f :: Bool -> Bool
-- f a = not (not a)

-- a :: Bool
-- not :: Bool -> Bool
-- not a :: Bool
-- not (not a) :: Bool

-- not a :: Bool
-- f = not
-- e = a
-- f e = not a

-- not (not a) :: Bool
-- f = not
-- e = not a
-- f e = not (not a)

-- f :: A -> B
-- e :: A
-- f e :: B

{-
enum Bool2 {
  True2,
  False2
}
-}

data Bool2 where
  True2 :: Bool2
  False2 :: Bool2
  deriving Show

not2 :: Bool2 -> Bool2
not2 True2 = False2
not2 False2 = True2

-- pattern-matching
and2 :: Bool2 -> Bool2 -> Bool2
and2 True2  True2  = True2
and2 True2  False2 = False2
and2 False2 True2  = False2
and2 False2 False2 = False2

and3 :: Bool2 -> Bool2 -> Bool2
and3 True2 True2 = True2
and3 _ _ = False2

and4 :: Bool2 -> Bool2 -> Bool2
and4 True2 y = y
and4 False2 _ = False2

-- data Foo = X | Y | Z
data Foo where
  X :: Foo
  Y :: Foo
  Z :: Foo
  deriving Show

invert :: Foo -> Foo
invert X = Z
invert Y = Y
invert Z = X

isX :: Foo -> Bool
isX X = True
isX _ = False

data Temperature where
  Fah :: Int -> Temperature   -- constructor
  Cel :: Float -> Temperature -- constructor
  deriving Show

doubleTemp :: Temperature -> Temperature
doubleTemp (Fah f) = Fah (2 * f)
doubleTemp (Cel c) = Cel (2.0 * c)

doubleTemp2 :: Temperature -> Temperature
doubleTemp2 t =
  case doubleTemp t of
    Fah f -> Fah (2 * f)
    Cel c -> Cel (2.0 * c)

toFahrenheit :: Temperature -> Float
toFahrenheit (Fah f) = fromIntegral f
toFahrenheit (Cel c) = (9/5) * c + 32

main :: IO ()
main = putStrLn "Hello, Haskell!"

data List where
  Nil :: List
  Cons :: Int -> List -> List
  deriving Show

data BinTree where
  Leaf :: Int -> BinTree
  Node :: Char -> BinTree -> BinTree -> BinTree

data TwoThree where
  Leaf23 :: TwoThree
  TwoNode :: TwoThree -> TwoThree -> TwoThree
  ThreeNode :: TwoThree -> TwoThree -> TwoThree -> TwoThree

length23 :: TwoThree -> Int
length23 Leaf23 = 1
length23 (TwoNode l r) = length23 l + length23 r
length23 (ThreeNode l m r) = length23 l + length23 m + length23 r

exampleList :: List
exampleList = Cons 1 (Cons 2 (Cons 3 Nil))

doubleEach :: List -> List
doubleEach Nil = Nil
doubleEach (Cons x xs) = Cons (2 * x) (doubleEach xs)

-- doubleEach (Cons 1 (Cons 2 (Cons 3 Nil)))      x = 1, xs = Cons 2 (Cons 3 Nil)
-- = Cons (2 * 1) (doubleEach (Cons 2 (Cons 3 Nil)))
-- = Cons 2 (doubleEach (Cons 2 (Cons 3 Nil)))    x = 2, xs = Cons 3 Nil
-- = Cons 2 (Cons (2 * 2) (doubleEach (Cons 3 Nil)))
-- = Cons 2 (Cons 4 (doubleEach (Cons 3 Nil)))    x = 3, xs = Nil
-- = Cons 2 (Cons 4 (Cons (2 * 3) (doubleEach Nil)))
-- = Cons 2 (Cons 4 (Cons 6 (doubleEach Nil)))
-- = Cons 2 (Cons 4 (Cons 6 Nil))

exampleBuiltInList :: [Int]
exampleBuiltInList = 1 : (2 : (3 : []))

doubleEachBuiltIn :: [Int] -> [Int]
doubleEachBuiltIn [] = []
doubleEachBuiltIn (x : xs) = (2 * x) : (doubleEachBuiltIn xs)

-- int lengthBuiltIn<T>(List<T> xs) { ... }
-- lengthBuiltIn<int>(some list of ints)
lengthBuiltIn :: forall a. [a] -> Int
lengthBuiltIn [] = 0
lengthBuiltIn (_ : xs) = 1 + lengthBuiltIn xs

reverseBuiltIn :: forall a. [a] -> [a]
reverseBuiltIn [] = []
-- reverseBuiltIn (x : xs) = reverseBuiltIn xs ++ (x : [])
reverseBuiltIn (x : xs) = reverseBuiltIn xs ++ [x]

lengthBuiltInOfInts :: [Int] -> Int
lengthBuiltInOfInts xs = lengthBuiltIn @Int xs

listOfBoolFunctions :: [Bool -> Bool]
listOfBoolFunctions = [not, id]

-- map1 where a = (Int -> Float), b = [Bool]
map1 :: forall a b. (a -> b) -> [a] -> [b]
map1 _ [] = []
map1 f (x : xs) = f x : map1 f xs

filter1 :: forall a. (a -> Bool) -> [a] -> [a]
filter1 _ [] = []
filter1 f (x : xs) =
  if f x then
    x : filter1 f xs
  else
    filter1 f xs

id1 :: forall a. a -> a
id1 x = x

const1 :: forall a b. a -> b -> a
const1 x _ = x

const2 :: forall a b. a -> b -> b
const2 _ y = y

add :: Int -> Int -> Int
add x y = x + y

add2 :: (Int, Int) -> Int
add2 (x, y) = x + y

pairOfInts :: (Int, Int)
pairOfInts = (3, 4)

class Stringable a where
  toString :: a -> String

-- toString :: forall a. Stringable a => a -> String

instance Stringable Bool where
  toString :: Bool -> String
  toString b = if b then "True" else "False"

instance Stringable String where
  toString :: String -> String
  toString x = x
