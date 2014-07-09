module Types.Token where

data Token = Equals
           | ArgsBegin
           | ArgsEnd
           | Identifier String
           | Operator String
           | Literal Integer
             deriving Show
