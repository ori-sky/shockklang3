module Parser where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Types.Token (Token)
import qualified Types.Token as T
import Types.Assoc
import Types.Decl
import Types.AST

type TokenState = State [Token]

parsePrimary :: ExceptT String TokenState AST
parsePrimary = do
    ts <- lift get
    case ts of
        T.Identifier ident : _ -> fail "parsePrimary not implemented"
        T.Literal lit : _ -> do
            lift (modify tail)
            return (Literal lit)
        T.BracketOpen : _ -> do
            lift (modify tail)
            parseExpr
        _ -> fail "expected primary"

parseOperand :: ExceptT String TokenState AST
parseOperand = do
    ast <- parsePrimary
    return ast

parseExpr1 :: Integer -> AST -> ExceptT String TokenState AST
parseExpr1 minPrec lhs = do
    ts <- lift get
    case ts of
        [] -> return lhs
        T.Operator op : _ -> case precedence op of
            Right prec -> if prec >= minPrec
                then do
                    lift (modify tail)
                    rhs <- parseOperand >>= parseExpr2 prec
                    case associativity op of
                        Right assoc -> parseExpr1 minPrec $ Call (NamedFunction op) $ case assoc of
                            LeftAssoc  -> [lhs, rhs]
                            RightAssoc -> [rhs, lhs]
                        Left e -> fail e
                else return lhs
            Left e -> fail e
        _ -> fail (show ts)

parseExpr2 :: Integer -> AST -> ExceptT String TokenState AST
parseExpr2 minPrec rhs = do
    ts <- lift get
    case ts of
        [] -> return rhs
        T.Operator op : _ -> case precedence op of
            Right prec -> case associativity op of
                Right assoc -> if rightAssoc assoc || prec > minPrec
                    then parseExpr1 prec rhs >>= parseExpr2 minPrec
                    else return rhs
                Left e -> fail e
            Left e -> fail e
        _ -> fail (show ts)

parseExpr :: ExceptT String TokenState AST
parseExpr = do
    ts <- lift get
    case ts of
        [] -> fail "expected expression"
        _ -> parseOperand >>= parseExpr1 0

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
