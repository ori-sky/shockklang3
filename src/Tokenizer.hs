module Tokenizer where

import Data.Char (isAlphaNum)
import Types.Token

isOperatorChar :: Char -> Bool
isOperatorChar '+' = True
isOperatorChar _ = False

tokenize :: String -> [Token]
tokenize [] = []
tokenize ('=':xs) = Equals : tokenize xs
tokenize ('[':xs) = ArgsBegin : tokenize xs
tokenize (']':xs) = ArgsEnd : tokenize xs
tokenize (' ':xs) = tokenize xs
tokenize ('\n':xs) = tokenize xs
tokenize ('\r':xs) = tokenize xs
tokenize ('\t':xs) = tokenize xs
tokenize ('\v':xs) = tokenize xs
tokenize ('\f':xs) = tokenize xs
tokenize s@(x:_) = if isAlphaNum x
    then case reads ident :: [(Integer, String)] of
        [(lit, "")] -> Literal lit : tokenize identxs
        _ -> Identifier ident : tokenize identxs
    else if isOperatorChar x
        then Operator op : tokenize opxs
        else error $ "syntax error: unexpected token `" ++ x : "`"
  where
    (ident, identxs) = span isAlphaNum s
    (op, opxs) = span isOperatorChar s
