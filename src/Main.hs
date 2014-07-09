module Main where

import Tokenizer (tokenize)

eval :: String -> String
eval = show . tokenize

main :: IO ()
main = interact eval >> putStrLn ""
