module Main where

{-
multi line comment
-}

-- single line comment

-- constant bool  = true; {C-style comparison<s>}
x :: Bool
x = True

-- bool f(bool a) { return !a; }
f :: Bool -> Bool
f a = not a

main :: IO ()
main = putStrLn "Hello World!"