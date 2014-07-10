module Types.AST where

data AST = Function Integer AST
         | Call AST [AST]
         | Literal Integer
           deriving Show
