module Parser where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Types.Token (Token)
import qualified Types.Token as T
import Types.AST
import Types.Decl

parsePrimary :: ExceptT String (State [Token]) AST
parsePrimary = fail "not implemented"

parseExpr' :: Integer -> AST -> ExceptT String (State [Token]) AST
parseExpr' minPrec lhs = fail "not implemented"

parseExpression :: ExceptT String (State [Token]) AST
parseExpression = parsePrimary >>= parseExpr' 0

parseDeclaration :: ExceptT String (State [Token]) Decl
parseDeclaration = do
    ts <- lift get
    case ts of
        T.Identifier ident : T.Equals : xs -> do
            lift $ modify (drop 2)
            parseExpression >>= return . (Decl ident)
        _ -> fail "expected top-level declaration"

parse :: [Token] -> AST
parse = error "not implemented"
