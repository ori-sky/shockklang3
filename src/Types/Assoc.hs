module Types.Assoc where

data Assoc = LeftAssoc | RightAssoc

precedence :: String -> Either String Integer
precedence "+" = return 60
precedence "*" = return 70
precedence op = fail $ "failed to look up precedence for `" ++ op ++ "`"

associativity :: String -> Either String Assoc
associativity "+" = return LeftAssoc
associativity "*" = return LeftAssoc
associativity op = fail $ "failed to look up associativity for `" ++ op ++ "`"

leftAssoc :: Assoc -> Bool
leftAssoc assoc = case assoc of
    LeftAssoc  -> True
    RightAssoc -> False

rightAssoc :: Assoc -> Bool
rightAssoc = not . leftAssoc
