module Types.AST where

import Types.Token (Token)

data AST = Function Integer AST
         | NamedFunction String
         | Call AST [AST]
         | Literal Integer
         | Dummy [AST]
           deriving Show
