import FPPrac.Trees
import Data.List
import Test.QuickCheck
import Data.Maybe

{-
  This file includes 2-FP.1 to 2-FP.10
-}

-- Exercise 2-FP.1

-- (1)

data Tree1a = Leaf1a Int 
 | Node1a Int Tree1a Tree1a
 deriving (Show, Eq)

exampleTree1a =  Node1a 50
 (
  Node1a 10 (
    Node1a 100 ( Node1a 1000 (Leaf1a 99) (Leaf1a 9) ) (Leaf1a 65)
  ) 
  
  (Leaf1a 15)
 )
 (Leaf1a 100)

pp1a :: Tree1a -> RoseTree
pp1a (Leaf1a n) = RoseNode (show n) []
pp1a (Node1a n t1 t2) = RoseNode (show n) [pp1a t1 ,pp1a t2]

-- (2)
data Tree1b = Leaf1b (Int, Int)
 | Node1b (Int, Int) Tree1b Tree1b deriving (Show, Eq)

exampleTree1b = Node1b (50, 50) 
 ( Leaf1b (1,2) )
 ( Node1b (100,50) 
  ( Leaf1b (100,101) ) 
  ( Leaf1b (100,101) ) 
 )

pp1b :: Tree1b -> RoseTree
pp1b (Leaf1b n) = RoseNode (show n) []
pp1b (Node1b n t1 t2) = RoseNode (show n) [pp1b t1 ,pp1b t2]

-- (3)

data Tree1c = Leaf1c Int
 | Node1c Tree1c Tree1c deriving (Show, Eq)

exampleTree1c =  Node1c 
 (
  Node1c  (Leaf1c 5) (Leaf1c 15)
 )
 (Leaf1c 100)

pp1c :: Tree1c -> RoseTree
pp1c (Leaf1c n) = RoseNode (show n) []
pp1c (Node1c t1 t2) = RoseNode "" [pp1c t1 ,pp1c t2]

-- (4)

data Tree1d = Leaf1d (Int, Int)
 | Node1d [Tree1d] deriving (Show, Eq)

exampleTree1d = Node1d [t1, t2, t4]
 where 
  t1 = Leaf1d (1, 2)
  t2 = Node1d [Leaf1d (3, 4), Leaf1d (5, 6)]
  t3 = Node1d [Leaf1d (5, 6), Leaf1d (7, 8) ]
  t4 = Node1d [Leaf1d (9, 0), t3]

pp1d :: Tree1d -> RoseTree
pp1d (Leaf1d n) = RoseNode (show n) []
pp1d (Node1d ts) = RoseNode "" $ map pp1d ts

-- (5)

class PP a where
 pp :: a -> RoseTree

instance PP Tree1a where  
 pp (Leaf1a n) = RoseNode (show n) []
 pp (Node1a n t1 t2) = RoseNode (show n) [pp t1 ,pp t2]

instance PP Tree1b where
 pp (Leaf1b n) = RoseNode (show n) []
 pp (Node1b n t1 t2) = RoseNode (show n) [pp t1 ,pp t2]

instance PP Tree1c where
 pp (Leaf1c n) = RoseNode (show n) []
 pp (Node1c t1 t2) = RoseNode "" [pp t1 ,pp t2]

instance PP Tree1d where
 pp (Leaf1d n) = RoseNode (show n) []
 pp (Node1d ts) = RoseNode "" $ map pp ts


-- Exercise 2-FP.2

-- (1)

treeAdd :: Tree1a -> Int -> Tree1a
treeAdd (Leaf1a n) x = Leaf1a $ n + x
treeAdd (Node1a n t1 t2) x = Node1a (n + x) (treeAdd t1 x) (treeAdd t2 x)

-- (2)

treeSquare :: Tree1a -> Tree1a
treeSquare (Leaf1a n) = Leaf1a $ n * n
treeSquare (Node1a n t1 t2) = Node1a (n * n) (treeSquare t1) (treeSquare t2)

mapTree :: (Int -> Int) -> Tree1a -> Tree1a
mapTree f (Leaf1a n) = Leaf1a $ f n
mapTree f (Node1a n t1 t2) = Node1a (f n) (mapTree f t1) (mapTree f t2)

treeAdd' :: Int -> Tree1a -> Tree1a
treeAdd' x tree = mapTree (+x) tree

treeSquare' :: Tree1a -> Tree1a
treeSquare' tree = mapTree (^2) tree

-- (4)
addNode :: Tree1b -> Tree1a
addNode ( Leaf1b (x, y) ) = Leaf1a $ x + y
addNode ( Node1b (x, y) t1 t2 ) = Node1a (x + y) (addNode t1) (addNode t2)

-- (5)

mapTree1b :: ( (Int,Int) -> Int ) -> Tree1b -> Tree1a
mapTree1b f ( Leaf1b tup ) = Leaf1a $ f tup
mapTree1b f ( Node1b tup t1 t2 ) = ( Node1a (f tup) ) (mapTree1b f t1) (mapTree1b f t2)

-- Exercise 2-FP.3

-- (1)

binMirror1a :: Tree1a -> Tree1a
binMirror1a (Leaf1a n) = Leaf1a n
binMirror1a (Node1a n t1 t2) = (Node1a n) (binMirror1a t2) (binMirror1a t1)

-- (2)

class BinMirror tree where
  binMirror :: tree -> tree

instance BinMirror Tree1a where
  binMirror (Leaf1a n) = Leaf1a n
  binMirror (Node1a n t1 t2) = (Node1a n) (binMirror t2) (binMirror t1)

-- (3)

instance BinMirror Tree1d where
  binMirror ( Leaf1d (x, y) ) = Leaf1d (y, x)
  binMirror (Node1d ts) = Node1d ( map binMirror ts )

-- Exercise 2-FP.4

data TreeInt = EmptyLeaf | NodeInt Int TreeInt TreeInt deriving (Show, Eq)

instance PP TreeInt where
  pp (EmptyLeaf) = RoseNode "" []
  pp (NodeInt n t1 t2) = RoseNode (show n) [pp t1, pp t2]

-- (1)

insertTree :: TreeInt -> Int -> TreeInt
insertTree EmptyLeaf new_value = NodeInt new_value EmptyLeaf EmptyLeaf
insertTree (NodeInt curr_value t1 t2) new_value
 | new_value <= curr_value = NodeInt curr_value (insertTree t1 new_value) t2
 | new_value > curr_value = NodeInt curr_value t1 (insertTree t2 new_value)

insertTree' = flip insertTree

exampleIntTree1 = (1 `insertTree'` (2 `insertTree'` (0 `insertTree'` EmptyLeaf) ) )


-- (2)
makeTree :: [Int] -> TreeInt
makeTree xs = foldr insertTree' EmptyLeaf xs

exampleIntTree2 = makeTree [-10,27,-42,71,23,-88,-80,35,50,-53,49,63,-26,-4,-25,22,30,2,-76]

-- (3)
-- infix operation?
makeList :: TreeInt -> [Int]
makeList (EmptyLeaf) = []
makeList (NodeInt n EmptyLeaf EmptyLeaf) = [n]
makeList (NodeInt n t1 t2) = (makeList t1) ++ [n] ++ (makeList t2)

-- (4)
prop_sort :: [Int] -> Bool
prop_sort xs = (sort xs) == (makeList (makeTree xs))

-- (5)
sortTree :: TreeInt -> TreeInt
sortTree t = makeTree $ makeList t


-- Exercise 2-FP.5
subtreeAt :: TreeInt -> Int -> Maybe TreeInt
subtreeAt (EmptyLeaf) target = Nothing
subtreeAt (NodeInt current t1 t2) target
 | target == current = Just (NodeInt current t1 t2)
 | target < current = subtreeAt t1 target
 | target > current = subtreeAt t2 target

-- Exercise 2-FP.6

cutOffAt :: Int -> Tree1a -> Tree1a
cutOffAt _ (Leaf1a n) = Leaf1a n
cutOffAt depth (Node1a n t1 t2)
 | depth == 0 = Leaf1a n
 | otherwise = (Node1a n ( cutOffAt (depth - 1) t1) (cutOffAt (depth - 1) t2) )


-- Exercise 2-FP.7
data BinTree a = Leaf
 | Node a (BinTree a) (BinTree a)
 deriving (Show, Eq)

exampleBinTree = (Node "Specter") (Leaf) (Node "F" Leaf Leaf)

-- (1)

instance Show a => PP (BinTree a) where
  pp (Leaf) = RoseNode "" []
  pp (Node value t1 t2) = ( RoseNode ( show value) ) [pp t1, pp t2]


-- (2)
instance BinMirror (BinTree a) where
 binMirror (Leaf) = Leaf
 binMirror (Node value t1 t2) = Node value (binMirror t2) (binMirror t1)

-- (3)
-- Why not (BinTree a), need to study more
instance Functor BinTree where
 fmap f (Leaf) = Leaf
 fmap f (Node value t1 t2) = Node (f value) (fmap f t1) (fmap f t2)

-- Exercise 2-FP.8
data MyList a = Nil | Cons a (MyList a)
 deriving (Show, Eq)

exampleMyList = Cons 1 $ Cons 2 $ Cons 3 $ Nil

-- (1)
instance Functor MyList where
 fmap f Nil = Nil
 fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- (2)
fromList :: [a] -> MyList a
fromList [] = Nil
fromList (x:xs) = Cons x (fromList xs)

-- (3)
prop_1stFunctorLaw :: [Int] -> Bool
prop_1stFunctorLaw xs = (fromList xs) == (fmap id $fromList xs)

-- (4)
prop_2ndFunctorLaw :: [Int] -> Bool
prop_2ndFunctorLaw xs = (fmap ( g . f ) $ fromList xs) == fmap g (fmap f $fromList xs)
 where 
  f = (^3)
  g = (+10)

-- (5) Written Question

-- Exercise 2-FP.9

-- (1)
data Person = Person { 
  name :: String, 
  age :: Int, 
  sex :: String, 
  place :: String
  } deriving Show



database = [ Person {name="Specter", age=99, sex="Male", place="Rome"},
 Person {name="Peter", age=20, sex="Male", place="Hong Kong"},
 Person {name="Mary", age=22, sex="Female", place="the US"},
 Person {name="May", age=24, sex="Female", place="the NL"},
 Person {name="Alexander", age=24, sex="Male", place="Macedon"},
 Person {name="Ana", age=35, sex="Female", place="Overwatch"}
 ]

-- (2)

plus :: Int -> [Person] -> [Person]
plus _ [] = []
plus n (row:rest) = row {age = (age row) + n } : (plus n rest)


-- (3)
names :: [Person] -> [String]
names [] = []
names (row:rest) = name row : names rest

-- Exercise 2-FP.10

-- We are not allow to use "do" and "<-"

getInt :: IO Integer
-- Enforce read type
getInt = fmap (read :: String -> Integer ) getLine

