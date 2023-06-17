# Assignment 2: Advanced Functional Programming, Spring 2023

This codebase contains a minimal implementation of the game [Minesweeper](https://en.wikipedia.org/wiki/Minesweeper_%28video_game%29). Right now it has a couple logic bugs and (at least) one missing feature; your job is to fix these issues.

We will discuss this assignment in week 5 of lecture. Don't worry if you feel lost at first, just try to take it one piece at a time: you have three whole weeks to work on this assignment.

Any code that involves the `IO`, `State`, or `EventM` types is code that you are **not** expected to understand yet. You will be able to complete the entire assignment without understanding the implementations of these functions.

You **are** expected to read and understand **all** of the other code. We are here to help whenever you have questions!

# Minesweeper

Take a few minutes to familiarize yourself with the rules of Minesweeper on Wikipedia or another source - the rules are pretty simple, but somewhat non-obvious if you've never seen the game before.

Our code is divided into three files, which I recommend reading first in this order:

- `src/Grid.hs` defines the polymorphic `Grid` data structure that we use to represent various aspects of the Minesweeper game grid.
- `src/Minesweeper.hs` defines the rules of Minesweeper using the `Grid` type.
- `src/Main.hs` defines an interactive console UI for playing Minesweeper.

Each module is commented, but it will still probably take serious effort to decipher much of the code. You will most likely need to spend a lot of time experimenting in the interpreter.

# Building and running

I recommend using GHC 9.2.7 for this project: `ghcup install --set ghc 9.2.7`

On Windows, you might have problems with the built-in terminal; the WSL terminal will be more reliable.

This project comes with configuration for Cabal and Stack, so you should be able to compile and run it like any other Haskell project.

- Enter the REPL: `cabal repl`
- Compile the project: `cabal build`
- Run the compiled Minesweeper program: `cabal exec minesweeper`
- Run the compiled Minesweeper program with options: `cabal exec minesweeper -- -h` (the `--` causes the arguments to be passed to `minesweeper` instead of `cabal`)

In the game UI:

- `#` represents a covered cell.
- `@` represents an uncovered mine.
- An empty space represents an uncovered cell with no adjacent mines.
- `1` through `8` represent uncovered cells with the corresponding number of adjacent mines.
- `9` represents a bug that you have to fix!

To play the game:

- Use the arrow keys to move around the board.
- Press the Enter key to select a cell.
- Press the Backspace key to flag a cell (once you've implemented the feature).
- Press the Escape key to exit the game.

# Requirements

These requirements apply to all of the code that **you write** in this assignment. Provided code and library code are not subject to these requirements.

These requirements apply to the code that you **submit** for this assignment, but you might find it helpful to break the requirements **at first**, then come back to clean up your code before submitting it.

- You must not modify the `assignment2.cabal` file.
- You must not write any calls to the functions `head`, `tail`, or `fromJust`, or the `!` or `!!` operators, or any other functions that have any possibility of a runtime crash.
- You must not write any calls to the `error` function or any uses of `undefined`.
- You must not write any module imports that include the word `Unsafe`.
- All of the code that you write must run in finite time for all possible inputs. You will not be graded on performance, but your code must not go into any infinite recursion.

# Exercises

Each exercise is weighted equally.

Once you've correctly implemented exercises 1 and 2, the game will play correctly!

1. Implement the `neighborhood` function in `Grid.hs` correctly. The function should return the values of all cells adjacent to the given `Index` on the given `Grid`. The list may be in any order. For out-of-bounds indices, the function should return an empty list. For example:

```
    1 2 3
g = 4 5 6
    7 8 9

neighborhood g (0,  0) = [2, 4, 5]
neighborhood g (1,  1) = [1, 4, 7, 2, 8, 3, 6, 9]
neighborhood g (2,  1) = [9, 8, 5, 3, 2]
neighborhood g (2,  0) = [2, 5, 3]
neighborhood g (-1, 1) = []
neighborhood g (1,  3) = []
```

You may find these functions helpful:

- `catMaybes`
- `mapMaybe`

2. Implement the `surveyField` function in `Minesweeper.hs` correctly. The function should return a `Survey` where each cell in the grid contains the number of mines that are adjacent to the given `Index` in the given `Field`. (An `Index` is not considered adjacent to itself.) For example:

```
(@ is mine, # is no mine)

        @ # @
field = # @ #
        # @ @

                    1 3 1
surveyField field = 3 3 4
                    2 2 2

        @ # # #
        # # @ #
field = # # # #
        # # # #
        # # # @

                    0 2 1 1
                    1 2 0 1
surveyField field = 0 1 1 1
                    0 0 1 1
                    0 0 1 0
```

You will find these functions helpful:

- `List.map`
- `Grid.map`
- `sum :: [Int] -> Int` (specialized type)

3. Implement the feature of flagged cells, where a player can flag a **covered** cell to indicate that they think it has a mine. In the game UI, a flag should appear as an exclamation mark (`!`). In the search algorithm that uncovers cells, a flagged cell should always be treated as "seen", so that flagged cells never get uncovered automatically.

If the player tries to flag a cell that has already been uncovered, the game state should not change. If the player tries to flag a covered cell that is already flagged, the flag should be removed from the cell.

You will need to modify these definitions:

- `CoverCell`
- `renderCell`
- `unseenNeighbors`
- `flagCell` in `Main.hs`, following the pattern of `selectCell`
