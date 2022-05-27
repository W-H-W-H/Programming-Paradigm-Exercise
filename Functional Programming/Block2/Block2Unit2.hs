import FPPrac.Trees
import Debug.Trace

{-
  This file includes 2-FP.11 to 2-FP.15
-}

class PP a where
 pp :: a -> RoseTree

-- Exercise 2-FP.11

data BinTree a b = Leaf b
 | Node a (BinTree a b) (BinTree a b)
 deriving (Show, Eq)

instance (Show a, Show b ) => PP (BinTree a b) where
 pp (Leaf b) = RoseNode (show b) []
 pp (Node a tL tR) = RoseNode (show a) [pp tL, pp tR]

exampleBinTree = (Node 'a') (Leaf 10) ( (Node 'b') (Leaf 11) (Leaf 12) )

-- Exercise 2-FP.12

-- (1)
type AST1 = BinTree Char Int 

parseExpr :: String -> AST1
parseExpr s
 | elem '+' s = let splitted = break (=='+') s in Node '+' (parseTerm $ fst splitted) (parseExpr $ tail $ snd splitted)
 | otherwise = parseTerm s

parseTerm :: String -> AST1
parseTerm s
 | elem '*' s = let splitted = break (=='*') s
     in Node '*' (parseFactor $ fst splitted) (parseTerm $ tail $ snd splitted)
 | otherwise = parseFactor s

parseFactor :: String -> AST1
parseFactor s = Leaf $ read s

-- (2) -- Seems OK
data Value = Const Int
 | Id String
 deriving Show

letter = ['a'..'z'] ++ ['A'.. 'Z']
digit = ['0'..'9']
letdig = letter ++ digit

type AST2 = BinTree Char Value

parseExpr' :: String -> (AST2, String)
parseExpr' str
 | s1 == "" = (t1, s1)
 | (head s1 == '+') = (Node '+' t1 t2, s2)
 | otherwise = (t1, s1) -- (head s1) can be other stuff
  where
   (t1, s1) = parseTerm' str -- Output: (<term>, rem)
   (t2, s2) = parseExpr' $ tail s1 -- Output: (<expr>, rem)


parseTerm' :: String -> (AST2, String)
parseTerm' str
-- Use s1 for condition 
 | s1 == "" = (t1, s1)
 | (head s1 == '*') = (Node '*' t1 t2, s2)
 | otherwise = (t1, s1)
  where
   (t1, s1) = parseFactor' str -- Output: (<factor>, rem)
   (t2, s2) = parseTerm' $ tail s1 -- Output: (<term>, rem)


parseFactor' :: String -> (AST2, String)
parseFactor' str
 | (head str) `elem` digit = (Leaf (Const $ read s2 ), s3)
 | (head str) `elem` letdig = (Leaf (Id s4), s5)
 | (head str) == '(' = (t1, tail s7) 
  where
   (s2, s3) = span (`elem` digit) str -- Output: (digit, rem)
   (s4, s5) = span (`elem` letdig) str -- Output: (id, rem)
   (s6, s7) = break (==')') str -- Output: (expr, rem)
   (t1, _) = parseExpr' $ tail s6 -- Output: (<expr>, rem)


-- Exercise 2-FP.13

data Token = NUMBER String
 | IDENTIFIER String
 | OPEN
 | CLOSE
 | PLUS
 | MULTI
 deriving (Show, Eq)

-- letter = ['a'..'z'] ++ ['A'.. 'Z']
-- digit = ['0'..'9']
-- letdig = letter ++ digit

tokenizer :: String -> [Token]
tokenizer [] = []
tokenizer (' ':xs) = tokenizer xs -- Omit any sapce
tokenizer ('\t':xs) = tokenizer xs -- Omit any tab
tokenizer ('(':xs) = OPEN : tokenizer xs
tokenizer (')':xs) = CLOSE : tokenizer xs
tokenizer ('+':xs) = PLUS : tokenizer xs
tokenizer ('*':xs) = MULTI : tokenizer xs
tokenizer xs
 | (head xs) `elem` letter = let (id, rem) = span (`elem` letdig) xs in IDENTIFIER id : tokenizer rem
 | otherwise = let (num, rem) = span (`elem` digit) xs in NUMBER num : tokenizer rem

-- Exercise 2-FP.14
type AST3 = BinTree Token Token

parseExpr'' :: [Token] -> (AST3, [Token])
parseExpr'' tokens
 | tokens1 == [] = (tree1, tokens1)
 | (head tokens1) == PLUS = (Node PLUS tree1 tree2, tokens2)
 | otherwise = (tree1, tokens1)
 where
  (tree1, tokens1) = parseTerm'' tokens
  (tree2, tokens2) = parseExpr'' $ tail tokens1

parseTerm'' :: [Token] -> (AST3, [Token])
parseTerm'' tokens
 | tokens1 == [] = (tree1, tokens1)
 | (head tokens1) == MULTI = ( Node MULTI tree1 tree2, tokens2)
 | otherwise = (tree1, tokens1) -- It means (head tokens1 == other stuff)
  where
   (tree1, tokens1) = parseFactor'' tokens -- Output (<factor>, ...)
   (tree2, tokens2) = parseTerm'' $ tail tokens1

parseFactor'' :: [Token] -> (AST3,  [Token])
parseFactor'' tokens
 | (head tokens) == OPEN = (t1, tail s2)
 | otherwise = (Leaf (head tokens), tail tokens)
  where
   (s1 ,s2) =  break (==CLOSE) tokens -- Output ([OPEN, ....] , [CLOSE, ...] )
   (t1, _) = parseExpr'' $ tail s1 -- Output (<Expr>, _)
   
-- Exercise 2-FP.15
type VarMap = [(String, Int)]

var_map = [("a", 10), ("b", 70)]

eval :: String -> VarMap -> Int
eval "" _ = error "Empty String"
-- Safely assume that the format of String is legit
eval s var_map = evalTree $ fst $ parseExpr'' $ tokenizer s 
 where
  evalTree :: AST3 -> Int
  evalTree (Node PLUS tL tR) = (+) (evalTree tL) (evalTree tR)
  evalTree (Node MULTI tL tR) = (*) (evalTree tL) (evalTree tR)
  evalTree (Leaf (IDENTIFIER id)) = findVar var_map id
  evalTree (Leaf (NUMBER n)) = read n
  findVar :: VarMap -> String -> Int
  findVar [] key = error "You didn't assign the variable"
  findVar (entry:table) key
   | (fst entry) == key = snd entry
   | otherwise = findVar table key
