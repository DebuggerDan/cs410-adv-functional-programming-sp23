# Assignment 3: *Tic-Tac-Toe* - Advanced Functional Programming, Spring 2023

This codebase contains a *Tic-Tac-Toe* game based on a modified, minimal implementation-codebase of the game [Minesweeper](https://en.wikipedia.org/wiki/Minesweeper_%28video_game%29), from Project #2.

We will discuss this assignment in week 5 of lecture. Don't worry if you feel lost at first, just try to take it one piece at a time: you have three whole weeks to work on this assignment.

Any code that involves the `IO`, `State`, or `EventM` types is code that you are **not** expected to understand yet. You will be able to complete the entire assignment without understanding the implementations of these functions.

You **are** expected to read and understand **all** of the other code. We are here to help whenever you have questions!

# Tic-Tac-Toe

Take a few minutes to familiarize yourself with the rules of Minesweeper on Wikipedia or another source - the rules are pretty simple, but somewhat non-obvious if you've never seen the game before.

Our code is divided into three files, which I recommend reading first in this order:

- `src/Grid.hs` defines the polymorphic `Grid` data structure that we use to represent various aspects of the Tic-Tac-Toe game grid.
- `src/tictactoe.hs` defines the rules of Tic-Tac-Toe using the `Grid` type.
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

# License & Acknowledgements

The almost all of the entire codebase for Project #3: *Tic-Tac-Toe* was created & written by Professor Katie Casamento, 2022.

`LICENSE` contains the license for the original `assignment2` codebase as follows:

```
Copyright (c) 2022, Katie Casamento

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Katie Casamento nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
```

In adherence to the academic requirements (3.1) & to just generally be honest, to implement this tic-tac-toe project, special thanks & credit to ***indirect*** assistance in the form of the following *OpenAI* prompt using the *GPT-4* model:

```
Without providing the actual code, suppose we are to create a command-line, keyboard-interactive tic-tac-toe game in Haskell.

This is for a college programming assignment that asks to refrain from using non-specifically licensed shared code, thus, also to actually learn about Haskell and logic, please explain in thoughtful manner that is also concise for implementation specifics / tips.

Assuming that we are starting off with a codebase that is, in contrast, implemented to represent Minesweeper. This codebase is comprised of three Haskell files, one for main implementation (containing the UI interface), one for definitions & functions of a polymorphic tic-tac-toe game grid data structure, and one for the definitions & functions of the rules of tic-tac-toe.

Additionally, the tic-tac-toe board size & the order of playing (e.g., Player 1 goes first) will be determinable via command-line arguments.

Tic-tac-toe rules would be described as, being on a board of size n by n, where n within the set of integers, there would exist 2n + 2 paths for any given tic-tac-toe player to win.

Such a environment is given with n number of rows, n number of columns, and always exactly 2 diagonal paths only for winning on a n by n board for tic-tac-toe.

To win, a player needs to fully mark a line that can either be vertical, horizontal, or a diagonal paths of n grid-squares are marked by either player 1 or player 2's specific move-markers.

The game should end automatically upon a full n number of squares are connected in a way that achieves that of a full line as described above.

After considering all the factors, specifications, definitions, and instructions aforementioned, please explain in human language, how to best (in terms of simplicity & efficiency) modify the pre-existing Minesweeper database to implement the game of tic-tac-toe.
```

Where the following was the generated response:

```
To modify the pre-existing Minesweeper codebase to implement the game of Tic-Tac-Toe, follow these steps:

1. Modify the data structure for the game grid:
   - In the file containing the grid data structure, replace the Minesweeper-specific elements with Tic-Tac-Toe elements.
   - Define a data type for grid squares, which can be either Empty, Player1, or Player2.

2. Modify the game rules:
   - In the file containing the rules of Tic-Tac-Toe, create a function that checks for a win condition. This function should take the current game grid and the player's move as input and return a boolean value.
   - Iterate through rows, columns, and diagonals to check for a win condition.
   - If a player has n number of their markers in a row, column, or diagonal, the game should end.

3. Modify the main implementation file:
   - Parse command-line arguments to allow for custom board size and player order.
   - Modify the game loop to handle Tic-Tac-Toe turns and moves instead of Minesweeper actions.
   - At each turn, display the current game grid, prompt the player for input (row and column), and update the grid accordingly.
   - Check for win conditions after each move using the win-checking function.
   - If the game reaches a win condition or a draw (no more empty grid squares), display the appropriate message and exit the game loop.

4. Update the user interface:
   - Replace Minesweeper-specific text and visuals with Tic-Tac-Toe elements.
   - Ensure that the input prompts and error messages are clear and relevant to Tic-Tac-Toe.

By following these steps, you should be able to modify the Minesweeper codebase to create a working command-line, keyboard-interactive Tic-Tac-Toe game in Haskell. Remember to test and debug your code thoroughly to ensure a smooth gameplay experience.
```

Thus, there was no code given or taken from such a source, just a thorough explanation of how to go about the implementation process. I believe this would be 'fair game' according to requirements, but I am including this prompt & response to be 100% honest & give proper credit.

Also, in following *OpenAI*'s *Sharing & Publication [Policy](https://openai.com/policies/sharing-publication-policy)* (last updated November 14th, 2022), which has a section that is somewhat applicable to this project, regarding "*Content co-authored with the OpenAI API*", here is the following disclaimer:

```
The co-author (Dan J.) notes conceptual knowledge assistance, but of in-direct & not direct code generation form, in part with GPT-4, OpenAI’s large-scale language-generation model. Upon generating conceptual explanation of Haskell methodologies & the explanation of how to approach implementation of Tic-Tac-Toe in Haskell, the co-author used the explanations & steps in implementation approach to their own liking and takes ultimate responsibility for their contribution to this modified codebase, originally authored by Professor Katie Casamento, 2022.
```
