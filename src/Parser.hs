module Parser where

import Control.Monad.Trans.State
import Types.Token (Token)
import qualified Types.Token as T
import Types.AST
import Types.Decl

parsePrimary :: [Token] -> Either String (AST, [Token])
parsePrimary xs = fail "not implemented"

parseExpr' :: [Token] -> AST -> Integer -> Either String (AST, [Token])
parseExpr' xs lhs minPrec = fail "not implemented"

parseExpression :: [Token] -> Either String (AST, [Token])
parseExpression xs = do
    (lhs, xss) <- parsePrimary xs
    parseExpr' xss lhs 0

parseDeclaration :: [Token] -> Either String (Decl, [Token])
parseDeclaration (T.Identifier ident : T.Equals : xs) = do
    (ast, xss) <- parseExpression xs
    return (Decl ident ast, xss)
parseDeclaration _ = fail "expected top-level declaration"

parse :: [Token] -> AST
parse = error "not implemented"
