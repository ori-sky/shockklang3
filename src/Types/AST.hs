module Types.AST where

data AST = Function Integer AST
         | NamedFunction String
         | Call AST [AST]
         | Literal Integer
           deriving Show
