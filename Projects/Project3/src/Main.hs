----- CS 410 [Adv. Functional Programming], Spring 2023 - Dan Jang
----- Project 3: Tic-tac-toe, Main.hs (based on Main.hs that was originally authored by Katie Casamento, (c) 2022 [see LICENSE & README.md])

-- This module defines the UI logic of our Minesweeper program, based on the
-- game logic in src/Minesweeper.hs and the Brick library for terminal UI.

module Main where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Table
import Control.Monad
import Data.Void
import Graphics.Vty.Attributes.Color
import Graphics.Vty.Input.Events (Event(..), Key(..), Modifier(..), Button(..))
import Options.Applicative

import Grid
import Tictactoe

-- This is the core type of values that will *change* throughout the execution
-- of the UI thread. The focus field stores the currently-selected cell in the
-- UI, and gets updated when the user selects a different cell.

--- Modified for Project #3: Tic-tac-toe
data AppState where
  AppState ::
    { --cover :: Cover
      board :: Toe
    , focus :: Index
    , turn :: Player
    } -> AppState


-- A Brick UI widget to represent a single cell on the game board. Surrounds
-- the cell text with a border if it's selected, or one space of padding if
-- it's unselected.
cellWidget ::
  Bool -> Index ->
  (FieldCell, SurveyCell, CoverCell) ->
  Widget Void
cellWidget selected i (fc, sc, cc) =
  let
    cellText = renderCell fc sc cc
    baseWidget =
      withAttr (attrName cellText) $
        Brick.str cellText
  in
    if selected then
      border baseWidget
    else
      padAll 1 baseWidget

-- A Brick UI widget to represent an entire game board as a table of cells.
-- Puts borders between each cell.
boardWidget :: Field -> AppState -> Widget Void
boardWidget field st =
  renderTable $ table $ Grid.toLists $
    Grid.zipWith3 cellWidget
      (Grid.map (\i -> i == st.focus) (shape field))
      (shape field)
      (Grid.zip3 field (surveyField field) st.cover)

-- Update the UI after the user selects a cell by pressing the Enter key while
-- the cell is highlighted. If the cell is already uncovered, there's nothing
-- to update.
selectCell :: CoverCell -> Survey -> AppState -> AppState
selectCell Covered survey st = st { cover = uncoverCell st.focus survey st.cover }
selectCell _ survey st = st

selectCellM :: Field -> Survey -> EventM Void AppState ()
selectCellM field survey = do
  st <- get
  case index st.cover st.focus of
    Just cc -> put $ selectCell cc survey st
    Nothing -> error ("crash: invalid index " ++ show st.focus)

-- Update the UI after the user flags a cell by pressing the Backspace key
-- while the cell is highlighted. If the cell is already uncovered, there's
-- nothing to update.
flagCell :: CoverCell -> AppState -> AppState
-- For Project #3, Exercise 3.)
flagCell Covered st = st { cover = replace st.focus Flagged st.cover }
flagCell Flagged st = st { cover = replace st.focus Covered st.cover }

flagCell _ st = st

flagCellM :: Field -> EventM Void AppState ()
flagCellM field = do
  st <- get
  case index st.cover st.focus of
    Just cc -> put $ flagCell cc st
    Nothing -> error ("crash: invalid index " ++ show st.focus)

-- Handle a BrickEvent, which represents a user input or some other change in
-- the terminal state outside our application. Brick gives us two relevant
-- commands in the EventM monad:
--   halt takes a final AppState and exits the UI thread
--   continue takes a next AppState and continues the UI thread

--- Modified for Project 3: Tic-tac-toe
handleEvent ::
  Options ->
  --Dimensions -> Field -> Survey ->
  BrickEvent Void Void ->
  EventM Void AppState ()
handleEvent argv event = --dim field survey event =
  case event of
    -- The VtyEvent constructor with an EvKey argument indicates that the user
    -- has pressed a key on the keyboard. The empty list in the pattern
    -- indicates that no modifier keys (Shift/Ctrl/...) were being held down
    -- while the key was pressed.
    VtyEvent (EvKey key []) ->
      case key of
        KLeft  -> modify $ \st -> st { focus = wraparound dim (left  st.focus) }
        KRight -> modify $ \st -> st { focus = wraparound dim (right st.focus) }
        KUp    -> modify $ \st -> st { focus = wraparound dim (up    st.focus) }
        KDown  -> modify $ \st -> st { focus = wraparound dim (down  st.focus) }
        KEsc   -> halt
        KEnter -> do--selectCellM field survey
          st <- get
          
          let toe = leToe st.turn--st.board--st.toe
          
          when (index st.board st.focus == Just Empty) $ do
            
            put $ st { board = replace st.focus toe st.board, turn = bigToe st.turn }

            -- Checkin' if there is a winner after each tic or tac on le toe
            -- Uses modified gameWon for tic-tac-toe winningness
            when (gameWon st.board toe) $ do
              
              -- liftIO is a nifty monad transformer thingy 
              liftIO $ putStrLn $ show "very nice, " ++ st.turn ++ " wins!"
              halt
            
            -- If it's all bruh, it's bruh moment (tie condition)
            -- Hopefully this does not trigger immediately when game starts somehow
            when (Grid.all (/= Bruh) st.board) $ do
              liftIO $ putStrLn "bruh, it is a tie"
              halt
        KBS    -> do
          liftIO $ putStrLn $ show st.turn ++ " loses automatically for trying to take back a turn heh (unless it was other that tried to do it, in that case, they lose instead)"--" ++ bigToe player 
          halt
        _      -> pure ()
    -- We don't care about any other kind of events, at least in this version
    -- of the code.
    _ -> pure ()
  where
    argv <- options

    dim = dimensions argv

    leToe Player1 = X
    bigToe Player1 = Player2

    leToe Player2 = O
    bigToe Player2 = Player1

-- The attribute map for our application, which Brick uses to apply text styles
-- to our widgets. If you want to change the color scheme, the color values are
-- defined here:
--   hackage.haskell.org/package/vty/docs/Graphics-Vty-Attributes-Color.html
gameAttrMap :: AttrMap
gameAttrMap =
  attrMap
    (brightWhite `on` black) -- default scheme for names not listed below
    [ --(attrName "1", fg brightBlue)
  --  , (attrName "2", fg green)
  --  , (attrName "3", fg brightRed)
      (attrName "O", fg blue)
    , (attrName "X", fg red)
  --  , (attrName "6", fg cyan)
  --  , (attrName "7", fg brightBlack)
  --  , (attrName "8", fg white)
    ]

-- The Brick application definition, which is used to generate our main
-- function. appChooseCursor and appStartEvent are given "default" values that
-- just opt out of those Brick features. appDraw calls boardWidget to render
-- the UI, appHandleEvent calls handleEvent to respond to user inputs, and
-- gameAttrMap defines the text style of the UI.
app :: Dimensions -> Field -> Survey -> App AppState Void Void
app dim field survey =
  App
    { appDraw = \st -> [boardWidget field st]
    , appChooseCursor = \_ _ -> Nothing
    , appHandleEvent = handleEvent dim field survey
    , appStartEvent = pure ()
    , appAttrMap = \_ -> gameAttrMap
    }


-- The application state when the game starts: all cells are covered, and the
-- focus is in the upper-left. Requires at least one cell in the grid, because
-- the focus has to be somewhere.
initialAppState :: Dimensions -> AppState
initialAppState dim =
  AppState
    { cover = Grid.replicate dim Bruh--Covered
    , focus = Index 0 0
    }

--- Modified for Project #3: Tic-tac-toe
-- This is the type of options that will **stay the same** throughout the
-- execution of the game.
data Options where
  Options ::
    { dimensions :: Dimensions
    ,  n :: Int -- n x n tic-tac-toe board
    --, mines :: Int
    , firstTic :: Player
    } -> Options
    deriving Show

-- The description of the application options, which is handled by the
-- optparse-applicative library.
optionsParser :: Parser Options
optionsParser = do
  --width  <- option auto (short 'w' <> value 8  <> help "board width")
  --height <- option auto (short 'h' <> value 10 <> help "board height")
  --mines  <- option auto (short 'm' <> value 7  <> help "mine count")
  size <- option auto (short 's' <> value 3 <> help "a tic-tac-toe board, with a size s-by-s!")
  --cointoss <- option auto () (short 'p' value)
  -- oneof from optparse-applicative library
  --cointoss <- option (oneof[char 'X', char 'O']) (short 'p' <> value 'X' <> help "the winner of the coin-toss gets to play first! (the letter 'X' or the letter 'O')")
  -- actually, eitherReader might be better
  cointoss <- option (eitherReader coinReferee) (short 'p' <> value Player <> help "the winner of the coin-toss gets to play first! (the letter 'X' or the letter 'O')")
  pure $ Options
    { dimensions = Dimensions { size, size }
    , n = size
    , firstTic-- = cointoss
    }
  where
    --coinReferee :: String -> Either String String
    coinReferee :: String -> Either String Player

    coinReferee "X" = Right Player1
    coinReferee "O" = Right Player2
    coinReferee _ = Left "that ain't a valid player, please provide either the letter X (Player #1) or the letter O (Player #2)!"
-- An IO action to print the help text and/or parse the user-supplied options.
options :: IO Options
options =
  execParser $
    info
      (helper <*> optionsParser)
      (fullDesc <> progDesc "Play a game of tic-tac-toe! Command-line arguments (-s for board-size, s x s & -p for which Player plays first): -s integer, -p (either a X or O [alphabet letter])")--minesweeper!")


-- Finally, to run the game, we just generate a random field and let Brick do
-- its work. When the UI thread exits, we check the final board state to see if
-- the user lost or won, and print out the final board state.

---- Modified for Project 3: Tic-Tac-Toe
main :: IO ()
main = do
  opts <- options
  
  let dim = dimensions opts
  let field = Grid.replicate dim Bruh-- <- toeField opts.dimensions
  let initialAppState = AppState { board = field, focus = Index 0 0, turn = opts.firstTic opts}

  finalAppState <-
    defaultMain
      (app opts)
      initialAppState

  putStrLn "Thank you for playing!"

  -- field <- randomField opts.dimensions opts.mines
  -- finalAppState <-
  --   defaultMain
  --     (app opts.dimensions field (surveyField field))
  --     (initialAppState opts.dimensions)
  -- when (gameLost field finalAppState.cover) $
  --   putStrLn "you lose!"
  -- when (gameWon field finalAppState.cover) $
  --   putStrLn "you win!"
  -- putStrLn "final board:"
  -- printBoard field finalAppState.cover
