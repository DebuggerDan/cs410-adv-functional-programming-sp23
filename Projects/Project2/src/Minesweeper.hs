-- This module defines the game logic for our implementation of the game
-- Minesweeper, based on the Grid type in src/Grid.hs.

module Minesweeper where

import Control.Monad.State
import Data.Foldable (for_)
import Data.List (uncons)
import Data.List qualified as List
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Traversable (for)
import Data.Vector qualified as Vector
import System.Random (randomRIO)

import Grid

-- There are three core concepts in our Minesweeper algorithms, which I've
-- somewhat arbitrarily named "field", "cover", and "survey". (I'm not sure if
-- there are common terms for these things, they're hard to search for.)


-- A Field is a Grid that represents where each mine is on the game board.

data FieldCell where
  Mine :: FieldCell
  NoMine :: FieldCell
  deriving (Show, Eq)

type Field = Grid FieldCell


-- A Cover is a Grid that represents the visibility of each cell on the game
-- board (whether the cell's value is hidden to the user in the UI).

data CoverCell where
  Covered :: CoverCell
  Uncovered :: CoverCell
  -- For Project #3, Exercise 3.)
  Flagged :: CoverCell
  deriving (Show, Eq)

type Cover = Grid CoverCell


-- A Survey is a Grid that represents the number shown on each cell on the game
-- board when the cell is visible to the user.

type SurveyCell = Int

type Survey = Grid SurveyCell


-- Decide if a player has lost a game: have they uncovered any cells that are
-- mines?
gameLost :: Field -> Cover -> Bool
gameLost field cover =
  Grid.any (\(fc,cc) -> fc == Mine && cc == Uncovered) $
    Grid.zip field cover

-- Decide if the player has won a game: have they uncovered all the cells that
-- aren't mines?
gameWon :: Field -> Cover -> Bool
gameWon field cover =
  Grid.all (\(fc,cc) -> fc == Mine || cc == Uncovered) $
    Grid.zip field cover

-- "Survey" a single field cell: how many mines are in it, zero or one?
surveyCell :: FieldCell -> SurveyCell
surveyCell Mine = 1
surveyCell NoMine = 0

---- CS 410 [Adv. Functional Programming], Spring 2023 - Dan Jang
---- Project 2: Minesweeper, Exercise #2.)
-- Implement the `surveyField` function in `Minesweeper.hs` correctly.
-- The function should return a `Survey`,
-- where each cell in the grid contains the number of mines that are adjacent to the given `Index` in the given `Field`.
-- (An `Index` is not considered adjacent to itself.)

-- "Survey" a whole field: how many mines are adjacent to each cell?

--- Original Function (for reference just in case):
--- surveyField :: Field -> Survey
--- surveyField field = Grid.map (const 9) field

-- Prev.:
surveyField :: Field -> Survey
surveyField boom = Grid.map metalDetector (shape boom)-- (const 9) field
  where
    metalDetector :: Index -> SurveyCell
    metalDetector gps_coordinates = sum[1 | Mine <- neighborhood boom gps_coordinates]

-- Prev. Prev.:
-- surveyField :: Field -> Survey
-- surveyField boom = Grid.map metalDetector (shape boom)
--   where
--     metalDetector idx =
--       sum . fmap surveyCell . fmap (index boom) . neighborIndices $ idx

-- Convert a cell on the game board to a string representing its state:
--   "#" represents any covered cell
--   "@" represents an uncovered mine
--   " " represents a cell with zero adjacent mines
--   "n" represents a cell with n adjacent mines (for 0 < n <= 8)
renderCell :: FieldCell -> SurveyCell -> CoverCell -> String
renderCell _ _ Covered = "#"
-- For Project #3, Exercise 3.)
renderCell _ _ Flagged = "!"
renderCell Mine _ Uncovered = "@"
renderCell NoMine 0 Uncovered = " "
renderCell NoMine n Uncovered = show n

-- Convert a game board to a string representing its state. Use with
-- Text.putStr, or use printBoardBoard.
renderBoard :: Field -> Cover -> String
renderBoard field cover =
  unlines $ Vector.toList $
    fmap (concat . Vector.toList) $
      Grid.zipWith3 renderCell field (surveyField field) cover

-- Prints a game board to standard console output. This is not actually used in
-- the UI code, since Brick handles the rendering there; this is just handy for
-- debugging in the REPL.
printBoard :: Field -> Cover -> IO ()
printBoard field cover = putStr $ renderBoard field cover 

-- The type of state in our "uncovering" search algorithm, which is a
-- depth-first graph search:
--   seen: the set of indices we've already visited
--   unseen: the working stack of indices
--   currentCover: the state of the cover, which we uncover cells in as we go
data SearchState where
  SearchState ::
    { seen :: Set Index
    , unseen :: [Index]
    , currentCover :: Cover
    } -> SearchState

-- Get a list of all of the neighbors that have not been seen yet.
unseenNeighbors :: SearchState -> Index -> [Index]
unseenNeighbors st i =
  List.filter
    -- (\j -> Set.notMember j st.seen)
    -- For Project #3, Exercise 3.)
    (\j -> Set.notMember j st.seen && index st.currentCover j /= Just Flagged)
    (neighborIndices i)

-- Pop an index off of the top of the working stack.
pop :: State SearchState (Maybe Index)
pop = do
  stack <- gets (uncons . (.unseen))
  for stack $ \(top,rest) -> do
    modify $ \st -> st { unseen = rest }
    pure top

-- Search for all zero-mine-count cells connected to the index at the top of
-- the working stack, and uncover each of them and their neighborhoods.
uncoverState :: Survey -> State SearchState ()
uncoverState survey = do
  top <- pop
  for_ top $ \i -> do
    modify $ \st ->
      st
        { currentCover = replace i Uncovered st.currentCover
        , seen = Set.insert i st.seen
        }
    when (index survey i == Just 0) $ do
      modify $ \st ->
        st
          { currentCover = replaceNeighborhood i Uncovered st.currentCover
          , unseen = unseenNeighbors st i ++ st.unseen
          }
    uncoverState survey

-- Run the action of a user uncovering a single cell in the game, which will
-- also uncover more cells via uncoverState if the selected cell is a
-- zero-mine-count cell. This doesn't check whether the selected cell contains
-- a mine in the field, that's handled in the UI.
uncoverCell :: Index -> Survey -> Cover -> Cover
uncoverCell i survey cover =
  let
    newState =
      execState (uncoverState survey) $
        SearchState
          { seen = Set.empty
          , unseen = [i]
          , currentCover = cover
          }
  in
    newState.currentCover


-- Choose a random element from a set, or return Nothing if the set is empty.
-- This is helpful in populating a random Field.
chooseRandom :: forall a. StateT (Set a) IO (Maybe a)
chooseRandom = do
  xs <- get
  if null xs then
    pure Nothing
  else do
    i <- lift (randomRIO (0, Set.size xs - 1))
    modify (Set.deleteAt i)
    pure (Just (Set.elemAt i xs))

-- Populate a Field with the given number of mines placed in random locations,
-- making sure that the randomly-chosen mine locations don't overlap.
populateField :: Dimensions -> Int -> StateT (Set Index) IO Field
populateField d mineCount =
  if mineCount == 0 then
    pure $ Grid.replicate d NoMine
  else do
    field <- populateField d (mineCount - 1)
    choice <- chooseRandom
    pure $ case choice of
      Nothing -> field
      Just i -> replace i Mine field

-- Generate a random Field from scratch by populating a field with all possible
-- indices as choices for random selection.
randomField :: Dimensions -> Int -> IO Field
randomField d mineCount =
  evalStateT
    (populateField d mineCount)
    (Set.fromList (allIndices d))
