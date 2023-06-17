-- This module defines the polymorphic data type that we use to represent a
-- "board" in our game of minesweeper. We deal with several different types of
-- "cells" in different parts of the code, so it's helpful to have a single
-- board type that can support any cell type.

module Grid where

import Data.List qualified as List
import Data.Maybe
import Data.Vector (Vector, (!?))
import Data.Vector qualified as Vector

-- The "type" keyword defines a *type synonym*, like a "typedef" or "type
-- alias" in other languages. We are just introducing a shorthand: when we
-- write something like "Grid Int", Haskell sees "Vector (Vector Int)".
type Grid a = Vector (Vector a)

-- We can construct Vectors with Vector.toList, so we can construct Grids with
-- nested applications of Vector.toList.

exampleGrid1 :: Grid Int -- or "exampleGrid1 :: Vector (Vector Int)"
exampleGrid1 =
  Vector.fromList
    [ Vector.fromList [1, 2, 3]
    , Vector.fromList [4, 5, 6]
    , Vector.fromList [7, 8, 9]
    , Vector.fromList [10, 11, 12]
    , Vector.fromList [13, 14, 15]
    ]

exampleGrid2 :: Grid Bool -- or "exampleGrid2 :: Vector (Vector Bool)"
exampleGrid2 =
  Vector.fromList
    [ Vector.fromList [True, False, False, True]
    , Vector.fromList [True, True, False, True]
    ]

-- Convert a Grid to a nested list of lists.
toLists :: Grid a -> [[a]]
toLists = List.map Vector.toList . Vector.toList

-- Convert a Grid to a flat list.
toList :: Grid a -> [a]
toList = concat . toLists


-- Apply a function to each element of a Grid:
--
--       (x11, x21, x31)   (f x11, f x21, f x31)
-- map f (x12, x22, x32) = (f x12, f x22, f x32)
--       (x13, x23, x33)   (f x13, f x23, f x33)
map :: (a -> b) -> Grid a -> Grid b
map = Vector.map . Vector.map


-- Apply a function to cells in the same position across two Grids:
--
--           (x11, x21, x31) (y11, y21, y31)   (f x11 y11, f x21 y21, f x31 y31)
-- zipWith f (x12, x22, x32) (y12, y22, y32) = (f x12 y12, f x22 y22, f x32 y32)
--           (x13, x23, x33) (y13, y23, y33)   (f x13 y13, f x23 y23, f x33 y33)
zipWith :: (a -> b -> c) -> Grid a -> Grid b -> Grid c
zipWith = Vector.zipWith . Vector.zipWith

-- Apply a function to cells in the same position across three Grids.
zipWith3 :: (a -> b -> c -> d) -> Grid a -> Grid b -> Grid c -> Grid d
zipWith3 = Vector.zipWith3 . Vector.zipWith3

-- Make tuples of cells in the same position across two Grids:
--
--     (x11, x21, x31) (y11, y21, y31)   ((x11, y11), (x21, y21), (x31, y31))
-- zip (x12, x22, x32) (y12, y22, y32) = ((x12, y12), (x22, y22), (x32, y32))
--     (x13, x23, x33) (y13, y23, y33)   ((x13, y13), (x23, y23), (x33, y33))
zip :: Grid a -> Grid b -> Grid (a, b)
zip = Grid.zipWith (,)

-- Make tuples of cells in the same position across three Grids.
zip3 :: Grid a -> Grid b -> Grid c -> Grid (a, b, c)
zip3 = Grid.zipWith3 (,,)

-- Combine every element of a Grid into a single value:
--
--           (x11, x21, x31)
-- foldr f z (x12, x22, x32) = f x11
--           (x13, x23, x33)     (f x21
--                                 (f x31
--                                   (f x12
--                                     (f x22
--                                       (f x32
--                                         (f x13
--                                           (f x23
--                                             (f x33 z))))))))
foldr :: (a -> b -> b) -> b -> Grid a -> b
foldr f z = Vector.foldr (flip (Vector.foldr f)) z

-- Check whether the given function returns true for *at least one* element in
-- a Grid.
any :: (a -> Bool) -> Grid a -> Bool
any = Vector.any . Vector.any

-- check whether the given function returns true for *every* element in a Grid.
all :: (a -> Bool) -> Grid a -> Bool
all = Vector.all . Vector.all

-- Convert a Grid to a String, separating the cells with spaces and newlines.
-- Use with putStr, or use printGrid.
pretty :: forall a. Show a => Grid a -> String
-- pretty g = unlines (Vector.toList (Vector.map (unwords . Vector.toList) (Grid.map show g)))
pretty g = unlines (List.map unwords (Grid.toLists (Grid.map show g)))

-- Print a Grid to standard console output separating the cells with spaces and
-- newlines.
prettyPrint :: forall a. Show a => Grid a -> IO ()
prettyPrint = putStr . pretty

data Dimensions where
  Dimensions ::
    { width :: Int
    , height :: Int
    } -> Dimensions
    deriving (Eq, Ord, Show)

data Index where
  Index ::
    { column :: Int
    , row :: Int
    } -> Index
    deriving (Eq, Ord)

instance Show Index where
  show i = "<" ++ show i.column ++ ", " ++ show i.row ++ ">"

left :: Index -> Index
left i = i { column = i.column - 1 }

right :: Index -> Index
right i = i { column = i.column + 1 }

up :: Index -> Index
up i = i { row = i.row - 1 }

down :: Index -> Index
down i = i { row = i.row + 1 }

-- Force an index to be in-bounds with modular arithmetic.
wraparound :: Dimensions -> Index -> Index
wraparound d i =
  Index
    { column = i.column `mod` d.width
    , row = i.row `mod` d.height
    }

-- Get a list of all the indices between 0 and the given upper bounds.
allIndices :: Dimensions -> [Index]
allIndices d =
  [ Index { column = c, row = r }
  | c <- [0 .. d.width-1]
  , r <- [0 .. d.height-1]
  ]

-- Access an individual cell in a Grid. Returns Nothing if the index is out of
-- bounds.
index :: forall a. Grid a -> Index -> Maybe a
index g i = (g !? i.row) >>= (!? i.column)

-- Produce a grid with the given dimensions, with each cell's value generated
-- by the given function:
--
--                    (f (0,0), f (1,0), f (2,0), f (3,0))
--   generate 4 3 f = (f (0,1), f (1,1), f (2,1), f (3,1))
--                    (f (0,2), f (1,2), f (2,2), f (3,2))
generate :: Dimensions -> (Index -> a) -> Grid a
generate d f =
  Vector.generate d.height $ \r ->
    Vector.generate d.width $ \c ->
      f (Index c r)

-- Produce a grid with the given dimensions, with the same value in every cell:
--
--                     (x, x, x, x)
--   replicate 4 3 x = (x, x, x, x)
--                     (x, x, x, x)
replicate :: Dimensions -> a -> Grid a
replicate d = Grid.generate d . const

-- Pair each cell's value with its index:
--
--          (x00, x10, x20)   (((0,0), x00), ((1,0), x10), ((2,0), x20))
--  indexed (x01, x11, x21) = (((0,1), x01), ((1,1), x11), ((2,1), x21))
--          (x02, x12, x22)   (((0,2), x02), ((1,2), x12), ((2,2), x22))
indexed :: Grid a -> Grid (Index, a)
indexed g =
  Vector.map
    (\(r, xs) -> Vector.map (\(c, x) -> (Index c r, x)) (Vector.indexed xs))
    (Vector.indexed g)

-- Get just the "shape" of the grid by replacing every cell's value with its index:
--
--        (x00, x10, x20)   ((0,0), (1,0), (2,0))
--  shape (x01, x11, x21) = ((0,1), (1,1), (2,1))
--        (x02, x12, x22)   ((0,2), (1,2), (2,2))
shape :: Grid a -> Grid Index
shape = Grid.map fst . indexed

-- Map over each element of a grid along with its index:
--
--                 (x00, x10, x20)   (f (0,0) x00, f (1,0) x10, f (2,0) x20)
--  mapWithIndex f (x01, x11, x21) = (f (0,1) x01, f (1,1) x11, f (2,1) x21)
--                 (x02, x12, x22)   (f (0,2) x02, f (1,2) x12, f (2,2) x22)
mapWithIndex :: forall a b. (Index -> a -> b) -> Grid a -> Grid b
mapWithIndex f = Grid.map (uncurry f) . indexed

-- Replace an element at a specific Index in a Grid. This isn't necessarily the
-- most efficient way to implement this operation, but it's a good example of
-- how many common operations can be seen as special cases of our general
-- higher-order functions.
replace :: forall a. Index -> a -> Grid a -> Grid a
replace i x = mapWithIndex (\j y -> if i == j then x else y)

-- Get a list of indices that are adjacent to a given Index. The list might
-- include out-of-bounds indices.
neighborIndices :: forall a. Index -> [Index]
neighborIndices i =
  [ Index (i.column - 1) (i.row - 1) -- up left
  , Index  i.column      (i.row - 1) -- up
  , Index (i.column + 1) (i.row - 1) -- up right
  , Index (i.column - 1)  i.row      -- left
  , Index (i.column + 1)  i.row      -- right
  , Index (i.column - 1) (i.row + 1) -- down left
  , Index  i.column      (i.row + 1) -- down
  , Index (i.column + 1) (i.row + 1) -- down right
  ]

-- Get a list of every cell adjacent to a given Index in a Grid. This returns a
-- list instead of a Vector because it may have anywhere from 3 to 8 values
-- (the corners of a Grid each have only 3 adjacent cells).

---- CS 410 [Adv. Functional Programming], Spring 2023 - Dan Jang
---- Project 2: Minesweeper, Exercise #1.)
--- "Implement the `neighborhood` function in `Grid.hs` correctly.
--- The function should return the values of all cells adjacent to the given `Index` on the given `Grid`.
--- The list may be in any order.
--- For out-of-bounds indices, the function should return an empty list.

-- Original Function (for reference just in case):
--- neighborhood :: forall a. Grid a -> Index -> [a]
--- neighborhood g i = [] 

neighborhood :: forall a. Grid a -> Index -> [a]
neighborhood idx2 idx = catMaybes $ fmap (index idx2) (neighborIndices idx)--[] 

-- Construct a Grid where each cell contains the neighborhood of the
-- corresponding cell in the input Grid. Try this out with small grids in the
-- REPL to see how it works. This will be part of our mine counting algorithm.
neighborhoods :: forall a. Grid a -> Grid [a]
neighborhoods g = Grid.map (neighborhood g) (shape g)

-- Replace all cells in the neighborhood of an index with the same one value.
-- This is convenient in our search algorithm later in the program.
replaceNeighborhood :: forall a. Index -> a -> Grid a -> Grid a
replaceNeighborhood i x =
  Grid.mapWithIndex (\j y -> if elem j (neighborIndices i) then x else y)
