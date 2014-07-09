module Types.Token where

data Token = Equals
           | BracketOpen
           | BracketClose
           | ArgsBegin
           | ArgsEnd
           | Identifier String
           | Operator String
           | Literal Integer
             deriving Show
