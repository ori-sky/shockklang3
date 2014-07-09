module Main where

import Tokenizer (tokenize)
import Parser (parse)

{-

add = [x y] x + y
five = 5
add5 = [x] add x five
main = add5 2

-}

eval :: String -> String
eval = show . tokenize

main :: IO ()
main = interact eval >> putStrLn ""
