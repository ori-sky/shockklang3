{-# LANGUAGE LambdaCase #-}

module Parser where

import Control.Monad.Except
import Control.Monad.State
import Types.Token (Token)
import qualified Types.Token as T
import Types.Assoc
import Types.Decl
import Types.AST

type TokenState = State [Token]

parsePrimary :: ExceptT String TokenState (Maybe AST)
parsePrimary = get >>= \case
    T.Identifier ident : _ -> modify tail >> return (Just (NamedFunction ident))
    T.Literal lit : _ -> modify tail >> return (Just (Literal lit))
    T.BracketOpen : _ -> modify tail >> parseExpr >>= return . Just
    _ -> return Nothing

parseOperand' :: ExceptT String TokenState [AST]
parseOperand' = parsePrimary >>= \case
    Just ast -> parseOperand' >>= return . (ast :)
    Nothing  -> return []

parseOperand :: ExceptT String TokenState AST
parseOperand = parseOperand' >>= return . Dummy

parseExpr1 :: Integer -> AST -> ExceptT String TokenState AST
parseExpr1 minPrec lhs = get >>= \case
    [] -> return lhs
    T.Operator op : _ -> case precedence op of
        Right prec -> if prec >= minPrec
            then do
                modify tail
                rhs <- parseOperand >>= parseExpr2 prec
                case associativity op of
                    Right assoc -> parseExpr1 minPrec $ Call (NamedFunction op) $ case assoc of
                        LeftAssoc  -> [lhs, rhs]
                        RightAssoc -> [rhs, lhs]
                    Left e -> fail e
            else return lhs
        Left e -> fail e
    _ -> fail "parseExpr1"

parseExpr2 :: Integer -> AST -> ExceptT String TokenState AST
parseExpr2 minPrec rhs = get >>= \case
    [] -> return rhs
    T.BracketClose : _ -> modify tail >> return rhs
    T.Operator op : _ -> case precedence op of
        Right prec -> case associativity op of
            Right assoc -> if rightAssoc assoc || prec > minPrec
                then parseExpr1 prec rhs >>= parseExpr2 minPrec
                else return rhs
            Left e -> fail e
        Left e -> fail e
    _ -> fail "parseExpr2"

parseExpr :: ExceptT String TokenState AST
parseExpr = get >>= \case
    [] -> fail "expected expression"
    _ -> parseOperand >>= parseExpr1 0

parseDecl :: ExceptT String TokenState (Maybe Decl)
parseDecl = get >>= \case
    [] -> return Nothing
    T.Identifier ident : T.Equals : _ -> modify (drop 2) >> parseExpr >>= return . Just . (Decl ident)
    _ -> fail "expected top-level declaration"

parseDecls :: ExceptT String TokenState [Decl]
parseDecls = parseDecl >>= \case
    Just decl -> parseDecls >>= return . (decl :)
    Nothing   ->  return []

parse :: [Token] -> Either String [Decl]
parse = fst . runState (runExceptT parseDecls)
