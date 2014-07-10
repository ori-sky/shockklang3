module Main where

import Tokenizer (tokenize)
import Parser (parse)

{-

add = [x y] x + y
five = 5
add5 = [x] add x five
main = add 5 2 * add 4 (add5 1)

-}

eval :: String -> String
eval = show . parse . tokenize

main :: IO ()
main = interact eval >> putStrLn ""
