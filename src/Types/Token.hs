module Types.Token where

{-

add = [x y] x + y
five = 5
add5 = [x] add x five
main = add5 2

-}

data Token = Equals
           | ArgsBegin
           | ArgsEnd
           | Identifier String
           | Operator String
           | Literal Integer
             deriving Show
