module Parser where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Types.Token (Token)
import qualified Types.Token as T
import Types.AST
import Types.Decl

type TokenState = State [Token]

parsePrimary :: ExceptT String TokenState AST
parsePrimary = do
    ts <- lift get
    case ts of
        T.Identifier ident : _ -> fail "parsePrimary not implemented"
        T.Literal lit : _ -> return (Literal lit)
        T.BracketOpen : _ -> do
            lift (modify tail)
            parseExpr
        _ -> fail "expected primary"

parseOperand :: ExceptT String TokenState AST
parseOperand = do
    ast <- parsePrimary
    return ast

{-
  where (operand, xs) = span f ts
        f (T.Operator _) = True
        f _ = False
-}

parseExpr' :: Ord a => a -> AST -> ExceptT String TokenState AST
parseExpr' minPrec lhs = do
    ts <- lift get
    case ts of
        [] -> return lhs
        _ -> fail (show ts)
        --_  -> fail "parseExpr' not implemented"
--    op <- lift $ gets head
--    rhs <- parseOperand

parseExpr :: ExceptT String TokenState AST
parseExpr = do
    ts <- lift get
    case ts of
        [] -> fail "expected expression"
        _ -> parseOperand >>= parseExpr' 0

--parseExpression :: ExceptT String TokenState AST
--parseExpression = fail $ show (get >>= takeWhile 
--parseExpression = parsePrimary >>= parseExpr' 0
--parseExpression = do
--    get >>= span f

{-
parseDeclaration :: ExceptT String TokenState Decl
parseDeclaration = do
    ts <- lift get
    case ts of
        T.Identifier ident : T.Equals : xs -> do
            lift $ modify (drop 2)
            parseExpr >>= return . (Decl ident)
        _ -> fail "expected top-level declaration"
-}

parse :: [Token] -> Either String AST
parse ts = fst $ runState (runExceptT parseExpr) ts
