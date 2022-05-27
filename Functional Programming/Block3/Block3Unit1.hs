-- The original filename is Set5.hs

{-
    This file includes Exercise 3-FP.2 to Exercise 3-FP.6
-}

module Block3Unit1 where

import Data.List
import Data.Functor
import Data.Either

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Test.QuickCheck

import FPPrac.Trees -- For RoseTree

fromLeft' :: Either l r -> l
fromLeft' (Left x) = x -- Newer GHC versions contain a fromLeft :: l -> Either l r -> l

fromRight' :: Either l r -> r
fromRight' (Right x) = x -- Newer GHC versions contain a fromRight :: r -> Either l r -> r

parser :: Parser a -> String -> a
parser p xs | isLeft res = error $ show $ fromLeft' res
          | otherwise  = fromRight' res
  where res = parse p "" xs





data Expr = Const Integer
          | Var String
          | Mult Expr Expr
          | Add Expr Expr

        -- Extended for Exercise 3-FP.4.1
          | Dec Expr
          | If Condition Expr Expr

        -- Extended for Exericise 3-FP.5.1
          | FuncCall String Expr

          deriving Show


-- fib :: Integer -> Integer
--fib = (evalfun . parserFun)
-- "function fib x = if x == 0 then 1 else (if x == 1 then 1 else fib(dec x)+fib(dec dec x))"


-- Functions for showRoseTree
class PP a where
 pp :: a -> RoseTree

instance PP Expr where
 pp (Const n) = RoseNode  (show $ Const n) []
 pp (Var v) = RoseNode (show $ Var v) []
 pp (Mult e1 e2) = RoseNode "*" [pp e1, pp e2]
 pp (Add e1 e2) = RoseNode "+" [pp e1, pp e2]
 pp (Dec e1) = RoseNode "Dec" [pp e1]
 pp (If cond e1 e2) = RoseNode (show cond) [pp e1, pp e2]
 pp (FuncCall fnName e1) = RoseNode (fnName ++ "(" ++ (show e1) ++ ")") []

-- Exercise 3-FP.2

languageDef = 
    emptyDef {
        Token.identStart = letter,
        Token.identLetter = alphaNum,
        Token.reservedNames = ["if", "then", "else", "dec", "function"],
        Token.reservedOpNames = ["+", "*", "=="]
    }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
integer = Token.integer lexer
parens = Token.parens lexer
symbol = Token.symbol lexer
reserved = Token.reserved lexer

-- Exercise 3-FP.3

-- (1) & (3)
parseFactor :: Parser Expr
parseFactor = try ( Const <$> integer ) <|> 
    try (FuncCall <$> identifier <*> parens parseExpr) <|> -- For Exercise 3-FP.5
    try (Var <$> identifier) <|>
    parens parseExpr

-- (2)
parseTerm :: Parser Expr
parseTerm = try (Mult <$> parseFactor <* (symbol "*") <*> parseTerm ) <|> 
    parseFactor

-- (3)
parseExpr :: Parser Expr
-- *** Order Matters ***
parseExpr = try( Add <$> parseTerm <* (symbol "+") <*> parseExpr ) <|> 
    try (parseTerm) <|> 
    try (parseIf) <|>
    parseDec

-- Exercise 3-FP.4

-- (1)
data Condition = Eq Expr Expr deriving Show

parseCondition :: Parser Condition
parseCondition = Eq <$> parseExpr <* (symbol "==") <*> parseExpr

-- (2)
parseIf :: Parser Expr
parseIf = If <$> ( (reserved "if") *> parseCondition ) <* (reserved "then") <*> parseExpr <* (reserved "else") <*> parseExpr

-- (3)
parseDec :: Parser Expr
parseDec = Dec <$> ( (reserved "dec") *> parseExpr )

-- Exercise 3-FP.5

-- (1)
data FunDef = Func String String Expr deriving Show

-- (2)
parseFunc :: Parser FunDef
parseFunc = Func <$> ( (reserved "function") *> identifier ) <*> identifier <* (symbol "=") <*> parseExpr

-- (3)
parserFun :: String -> FunDef
parserFun = parser parseFunc

fib :: FunDef
fib = parserFun 
    "function fib x = if x == 0 then 1 else (if x == 1 then 1 else fib(dec x)+ fib(dec dec x))"

-- Exercise 3-FP.6
-- The answer 

-- (1)
evalfun :: FunDef -> Integer -> Integer
evalfun (Func fnName paramName expr) value = evalExpr expr paramName value
    where 
        evalExpr :: Expr -> String -> Integer -> Integer
        evalExpr (Const value) _ _ = value
        -- As Feel free to ignore the function name and argument name
        evalExpr (Var varName) paramName value = value 
        evalExpr (Mult e1 e2) paramName value = (*) (evalExpr e1 paramName value) (evalExpr e2 paramName value)
        evalExpr (Add e1 e2) paramName value = (+) (evalExpr e1 paramName value) (evalExpr e2 paramName value)
        evalExpr (Dec e1) paramName value = (evalExpr e1 paramName value) - 1
        evalExpr (If (Eq e1 e2) e3 e4) paramName value = if (evalExpr e1 paramName value) == (evalExpr e2 paramName value) then 
            evalExpr e3 paramName value else evalExpr e4 paramName value
        -- hence both will correspond to the defined function
        evalExpr (FuncCall fnName e1) paramName value  = evalfun (Func fnName paramName expr)  (evalExpr e1 paramName value)

-- (2)
fn = (evalfun . parserFun) "function f x = x*x + 2*x + 1"
fn' = (evalfun . parserFun) "function f x = (x+1) * (x+1)"
prop_fn n = n >= 0 ==> fn n == fn' n

-- (3)
factorial :: Integer -> Integer
factorial = (evalfun . parserFun) "function factorial x = if x == 0 then 1 else factorial(dec x) * x"
prop_factorial n = n >= 0 ==> factorial n == product [1..n]

