import Data.Foldable
import Data.Monoid
import Text.Read
import Test.QuickCheck
import Block3Unit1 -- Original version: import Set5

{-
    This file includes Exercise 3-FP.12 to Exercise 3-FP.13
    Many of them are incompleted, sorry about that.
-}

-- Exercise 3-FP.12

-- (1)
fibibacci :: IO (Integer -> Integer)
fibibacci = (evalfun . parserFun) <$> readFile "fib.txt"

-- (2)
fib5 :: IO Integer
fib5 = fibibacci <*> pure 5

fibs :: IO [Integer]
fibs = (map <$> fibibacci) <*> pure [0..]

-- (3)
fact :: FunDef
fact = parserFun "function factorial x = if x == 0 then 1 else factorial(dec x) * x"

prompt :: IO Integer
prompt = (evalfun . parserFun) <$> getLine <*> ( (read :: String -> Integer) <$> getLine )

-- (4) incompleted

calculations :: Integer -> [String] -> [Integer]
calculations val [] = []
--calculations val (e:expr) =  : 

-- (5)


-- Exercise 3-FP.13

-- (1)
-- :t generate :: Gen a -> IO a

-- (2)
-- generate (Just <$> (arbitrary :: Gen Int))

-- (3)
data A = R [Int]
    | Q Int Char deriving Show

-- R <$> generate (arbitrary :: Gen [Int])

-- (4)
-- Q <$> generate (arbitrary :: Gen Int) <*> generate (arbitrary :: Gen Char)

-- (5)
instance Arbitrary A where
    arbitrary = oneof [
        R <$> ( arbitrary :: Gen [Int] ),
        Q <$> ( arbitrary :: Gen Int ) <*> ( arbitrary :: Gen Char )]
