-- The original filename is Set6.hs
module Block3Unit2 where

import Data.Foldable
import Data.Monoid
import Text.Read
import Control.Applicative
--import Set5

{-
    This file includes Exercise 3-FP.7 to Exercise 3-FP.11
-}

-- Exercise 3-FP.7

addstr :: String -> String -> Maybe String
-- fmap over a functor, not value. Thus, we cannot use show (...), instead, we use show <$> (...)
addstr xs ys = show <$> ( (+) <$> xs' <*> ys' )
    where
        xs' = readMaybe xs :: Maybe Int
        ys' = readMaybe ys :: Maybe Int


-- Exercise 3-FP.8

data MyList a = Nil | Cons a (MyList a)
              deriving (Show, Eq)

mylst  = Cons 1   $ Cons 2   $ Cons 3   $ Nil
mylst2 = Cons 10  $ Cons 20  $ Cons 30  $ Nil
mylst3 = Cons 100 $ Cons 200 $ Cons 300 $ Nil

-- rewrite functor instance
instance Functor MyList where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs) 

-- (1)

myzipWith3' :: (a -> b -> c -> d) -> MyList a -> MyList b -> MyList c -> MyList d
myzipWith3' f Nil _ _ = Nil
myzipWith3' f _ Nil _ = Nil
myzipWith3' f _ _ Nil = Nil
myzipWith3' f (Cons x xs) (Cons y ys) (Cons z zs) = Cons (f x y z) (myzipWith3' f xs ys zs)

-- (2)
instance Applicative MyList where 
    pure x = Cons x $ Nil
    --pure x = Nil -- I am not sure whether it is correct
    Nil <*> xs = Nil
    fs <*> Nil = Nil
    (Cons f fs) <*> (Cons x xs) = Cons (f x) (fs <*> xs)

-- (3)
myzipWith :: (a -> b -> c) -> MyList a -> MyList b -> MyList c
myzipWith f xs ys = f <$> xs <*> ys

myzipWith3 :: (a -> b -> c -> d) -> MyList a -> MyList b -> MyList c -> MyList d
myzipWith3 f xs ys zs = f <$> xs <*> ys <*> zs


-- Exercise 3-FP.9
getInt :: IO Integer
getInt = fmap (read :: String -> Integer) getLine

f :: IO Integer
f = (+) <$> getInt <*> getInt


-- Exercise 3-FP.10
justs :: [Maybe a] -> Maybe [a]
justs [] = Just []
justs (x:xs) = (:) <$> x <*> (justs xs)


-- Exercise 3-FP.11
data Parser r = P {
    runParser :: String -> [(r, String)]
}

char :: Char -> Parser Char
char c = P p
    where
        p [] = []
        p (x:xs) | c == x = [(x, xs)]
            | otherwise = []

-- (1)
-- runParser (char '1') "231"
-- runParser (char '1') "123"

-- (2)
instance Functor Parser where
    -- Alternative Way
    --fmap f p = P p'
    --    where 
    --        p' x = [ (f z, zs) | (z, zs) <- runParser p x ]

    -- Proper way
    fmap f p = P (\s -> [ (f x, xs ) | (x,xs) <- runParser p s ])


-- (3)
parseOneInt :: Parser Int
parseOneInt = read <$> (:[]) <$> char '1'


-- (4)
instance Applicative Parser where 
    pure x = P (\s -> [(x,s)]) -- What is the purpose of pure
    p1 <*> p2 = P ( \s -> [ (r1 r2, s2) | (r1,s1) <- runParser p1 s, (r2,s2) <- runParser p2 s1 ] )

parseAB :: Parser (Char, Char)
parseAB = (,) <$> char 'a' <*> char 'b'

-- (5)
parseString :: String -> Parser String
parseString "" = pure "" :: Parser String
parseString (x:xs) = (:) <$> char x <*> parseString xs
-- In values case: f <$> x <*> y like f x y


-- (6)
instance Alternative Parser where
    empty = P (\_ -> []) -- Define a Additive identity
    p1 <|> p2 = P (\s -> runParser p1 s ++  runParser p2 s)
