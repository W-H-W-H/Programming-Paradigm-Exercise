import Test.QuickCheck
import Data.Char
import Debug.Trace
import Data.List

-- **** Exercise 1-FP.1
f :: Float -> Float
f x = 2 * x * x + 3 * x - 5
-- ************ EndOf Exercise 1-FP.1

-- **** Exercise 1-FP.2
total1 :: Int -> Int
total1 0 = 0
total1 n = total1 (n-1) + n

total2 :: Int -> Int
total2 n = (n * (n+1)) `div` 2

prop_total n = (n >= 0) ==> total1 n == total2 n

-- quickCheck prop_total

-- ************ EndOf Exercise 1-FP.2


-- **** Exercise 1-FP.3
prop_addition :: Int -> Int -> Bool
prop_addition x y = (x + y) == (y + x)

prop_substraction :: Int -> Int -> Bool
prop_substraction x y = (x - y) == (y - x)

-- ************ EndOf Exercise 1-FP.3

-- **** Exercise 1-FP.4
code :: Int -> Char -> Char
code n c
    | 'A' <= c && c <= 'Z' = chr (  65 + ( (ord c + n - 65) `mod` 26 ) )
    | 'a' <= c && c <= 'z' = chr (  97 + ( (ord c + n - 97) `mod` 26 ) )
    | otherwise = c

prop_code :: Int -> Char -> Bool
prop_code n c = code n (code (26-n) c) == c
-- ************ EndOf Exercise 1-FP.4

-- **** Exercise 1-FP.5
totalMoney :: Float -> Float -> Float -> Float
totalMoney a r 0 = a
totalMoney a r n = ( totalMoney a r (n-1) ) * (1+r)
-- ************ EndOf Exercise 1-FP.5

-- **** Exercise 1-FP.6
discr :: Float -> Float -> Float -> Float
discr a b c = b^2 - 4 * a * c

root1 :: Float -> Float -> Float -> Float
root1 a b c
    | d < 0 = error "negative discriminant "
    | otherwise = (-b + sqrt d)/ (2*a)
    where
        d = discr a b c

root2 :: Float -> Float -> Float -> Float
root2 a b c
    | d < 0 = error "negative discriminant "
    | otherwise = (-b - sqrt d)/ (2*a)
    where
        d = discr a b c
-- ************ EndOf Exercise 1-FP.6

-- **** Exercise 1-FP.7
extrX :: Float -> Float -> Float -> Float
extrX a b c = - b / (2 * a)

extrY :: Float -> Float -> Float -> Float
extrY a b c = a * x^2 + b * x + c
    where x = - b / (2 * a)
-- ************ EndOf 1-FP.7

-- **** Exercise 1-FP.8

mylength :: [a] -> Int
mylength [] = 0
mylength (x:xs) = 1 + mylength xs

mysum :: [Float] -> Float
mysum [] = 0
mysum (x:xs) = x + mysum xs

myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = myreverse xs ++ [x]

mytake :: Int -> [a] -> [a]
mytake 0 xs = []
mytake n [] = []
mytake n (x:xs) = [x] ++ (mytake (n-1) xs)

myelem :: (Eq a) => a -> [a] -> Bool
myelem target [] = False
myelem target (x:xs) = ( if(x == target) then True else False ) ||  (myelem target xs)

myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (x:xs) = x ++ myconcat xs

mymaximum :: Ord a => [a] -> a
mymaximum [] = error "Undefined"
mymaximum (x:[]) = x
mymaximum (x:xs) = if (x > y) then x else y
    where y = mymaximum xs

myzip :: [a] -> [b] -> [(a,b)]
myzip xs [] = []
myzip [] ys = []
myzip (x:xs) (y:ys) = [(x,y)] ++ (myzip xs ys)

-- ************ EndOf Exercise 1-FP.8

-- **** Exercise 1-FP.9

-- (1)
-- Define index of first element is 0
r :: Num a => a -> a -> [a]
r a d = [a] ++ r (a+d) d

-- (2)
-- More reasonable method
r1 :: Num a => [a] -> Int -> a
r1 [] index = error "Fail to take any element in empty list"
r1 (x:xs) 0 = x
r1 (x:xs) index
    | index < 0 = error "index must >= 0"
    | otherwise = r1 xs (index-1)

-- (3)
-- sum from a[i] to a[j] if i <= j
totalr :: Num a => [a] -> Int -> Int -> a
totalr xs 0 end = sum $ take (end+1) xs
totalr xs begin end
    | begin < 0 || end < 0 = error "IndexOutOfBound: index should >= 0"
    | begin >= (length xs) || end >= (length xs) = error "IndexOutOfBound: index >= length xs"
    | begin <= end = totalr (tail xs) (begin-1) (end-1) -- j <= i does not support
    | otherwise = totalr xs end begin

-- ************ EndOf Exercise 1-FP.9

-- **** Exercise 1-FP.10

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x:[]) = True
allEqual (x:xs) = allEqualHelper x xs
    where
        allEqualHelper :: Eq a => a -> [a] -> Bool 
        allEqualHelper picked []  = True
        allEqualHelper picked (x:xs) = (x == picked) && (allEqualHelper picked xs)



isAS :: (Num a, Eq a) => [a] -> Bool
isAS xs = allEqual (rowSum xs (reverse xs))
    where
        rowSum :: Num a => [a] -> [a] -> [a]
        rowSum [] ys = []
        rowSum xs [] = []
        rowSum (x:xs) (y:ys) = [x+y] ++ (rowSum xs ys)
-- ************ EndOf 1-FP.10


-- **** Exercise 1-FP.11

-- Reuse mylength
lengthArray :: Num a => [[a]] -> [Int]
lengthArray [] = []
lengthArray (row:rest) = [(mylength row)] ++ lengthArray rest 

allRowsEquallyLong :: Num a => [[a]] -> Bool
allRowsEquallyLong matrix = allEqual (lengthArray matrix)

rowTotals :: Num a => [[a]] -> [a]
rowTotals [] = []
rowTotals (row:restRows) = [sum row] ++ rowTotals restRows


mytranspose :: (Eq a, Num a) => [[a]] -> [[a]]
mytranspose [] = []
mytranspose (xs:mat)
    | allRowsEquallyLong mat' == False = error "This is not a matrix"
    | xs == [] = []
    | otherwise = formNewRow mat' : mytranspose mat''
    where
        mat' = (xs:mat)
        mat'' = if xs /= [] then trimMatrix mat' else []

        trimMatrix :: (Eq a, Num a) => [[a]] -> [[a]]
        trimMatrix [] = []
        trimMatrix (xs:mat) = tail xs : trimMatrix mat

        formNewRow :: Num a => [[a]] -> [a]
        formNewRow [] = []
        formNewRow (xs:mat) = head xs : formNewRow mat

colTotals :: (Eq a, Num a) => [[a]] -> [a]
colTotals matrix = rowTotals (mytranspose matrix)

-- ************ EndOf Exercise 1-FP.11


-- **** Exercise 1-FP.12

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter pred xs = [x | x <- xs, pred x]

myfoldl :: (b -> a -> b) -> b -> [a] -> b
myfoldl f z [x] = f z x
myfoldl f z (x:xs) = myfoldl f (f z x) xs

myfoldr :: (a->b->b) -> b -> [a] -> b
myfoldr f z [x] = f x z
myfoldr f z xs = myfoldr f (f lastEle z) otherEles
    where 
        lastEle = last xs
        otherEles = init xs

myzipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myzipWith f _ [] = []
myzipWith f [] _ = []
myzipWith f (x:xs) (y:ys) = [f x y] ++ myzipWith f xs ys
-- myzipWith f xs ys = [ z | (x,y) <- zip xs ys , let z = f x y ]



-- ************ EndOf Exercise 1-FP.12

-- **** Exercise 1-FP.13

-- (1)
type Person = (String, Int, String, String)

names = ["Specter", "Peter", "Mary", "May", "Alexander", "Ana"]
ages = [99,20,22,24,24,35] :: [Int]
genders = ["Male", "Male", "Female", "Female", "Male", "Female"]
places = ["Rome", "Hong Kong", "the US", "the NL", "Macedon", "Overwatch"]

database = zip4 names ages genders places


-- (2)
extract :: String -> Person -> String
extract field person
    | field == "Name" = name
    | field == "Age" = show age
    | field == "Gender" = gender
    | field == "Place of residence" = place
    where
        (name, age, gender, place) = person

-- (3)
increaseAgeByRec :: Int  -> [Person] -> [Person]
increaseAgeByRec _ [] = []
increaseAgeByRec n (p:ps) = 
    let 
        (name, age, gender, place) = p 
    in 
        (name, age+n, gender, place) : increaseAgeByRec n ps

increaseAgeByLstCom :: Int  -> [Person] -> [Person]
increaseAgeByLstCom n ps = [(a,b + n,c,d) |  (a,b,c,d) <- ps]

increaseAgeByHiOrdFn :: Int  -> [Person] -> [Person]
increaseAgeByHiOrdFn n ps = map ( \(a,b,c,d) -> (a,b+n,c,d) ) ps 


-- (4)
findWomenByRec :: [Person] -> [String]
findWomenByRec [] = []
findWomenByRec (p:ps)
    | 30 <= age && age <= 40 && gender == "Female" = name : findWomenByRec ps
    | otherwise = findWomenByRec ps
        where
            (name, age, gender, place) = p

findWomenByLstCom :: [Person] -> [String]
findWomenByLstCom ps = [name |  (name, age, gender, place) <- ps , 30 <= age && age <= 40 && gender == "Female"]

findWomenByHiOrdFn :: [Person] -> [String]
findWomenByHiOrdFn ps = 
    let 
        f = filter (\(name, age, gender, place) ->30 <= age && age <= 40 && gender == "Female")
        m = map ( \(name, age, gender, place) -> name )
    in (m . f) ps 

-- (5)
findAgeByName :: String -> [Person] -> Int
findAgeByName name' ps = 
    let 
        toLowerString = map toLower 
        f = filter (\(name, age, gender, place) -> (toLowerString name) == (toLowerString name') )
        m = map ( \(name, age, gender, place) -> age )
    in head ( (m . f) ps )

-- (6)
unique :: Eq a => [a] -> [a]
unique [] = []
unique [x] = [x]
unique (x1:x2:xs) = if x1 /= x2 then x1:x2:unique xs else x1:unique xs

sortedAges = unique $ sort [ age | (name, age, gender, place) <- database ]

sortedDatabase [] _ = []
sortedDatabase db [] = []
sortedDatabase db (a:ages) =  filter ( \(name, age, gender, place) -> age == a) db ++ sortedDatabase db ages


-- ************ EndOf Exercise 1-FP.13

-- **** Exercise 1-FP.14 -- Try Integer

sieve :: [Int]
sieve = 
    let 
        sieveHelper :: [Int] -> [Int]
        sieveHelper [] = []
        sieveHelper (x:xs) = x : sieveHelper [ x' | x' <- xs, (mod x' x) /= 0 ]
    in sieveHelper [2..]

-- Not Good
isPrime :: Int -> Bool
isPrime n = 
    let 
        helper :: [Int] -> Bool
        helper (x:xs)
            | x < n = helper xs
            | x == n = True
            | x > n = False
    in helper sieve

firstNPrimes :: Int -> [Int]
firstNPrimes n = take n sieve

-- Not Good
primeSmallerThan :: Int -> [Int]
primeSmallerThan n = 
    let
        helper :: [Int] -> [Int]
        helper (x:xs)
            | x < n = x : helper xs
            | otherwise = []
    in helper sieve

dividers :: Int -> [Int]
dividers k = 
    let 
        helper :: [Int] -> Int -> [Int]
        helper (n:ns) m
            -- Order matters
            | n > m = []
            | m `mod` n == 0 = [n] ++ (helper ns m)
            | m `mod` n /= 0 = helper ns m
            | otherwise = []
    in helper sieve k



-- ************ EndOf Exercise 1-FP.14

-- **** Exercise 1-FP.15

pyth :: Int -> [(Int, Int, Int)]
pyth n = [(a,b,c) | a <- [1..n], b <- [a..n], c <- [b..n], a*a + b*b == c*c,  a < n, b < n, c < n]

-- ************ EndOf Exercise 1-FP.15


-- **** Exercise 1-FP.16
increasing :: (Num a, Ord a) => [a] -> Bool
increasing [] = False
increasing xs = and (zipWith (<) xs (tail xs))

-- Ask TA
weaklyIncreasing :: (Ord a, Num a, Fractional a) => [a] -> Bool
weaklyIncreasing [x] = True -- True trivailly if (For all x)(Exist y)(x \= y -> x > y)
weaklyIncreasing xs = 
    let 
        meanOfPreivous :: (Ord a, Num a, Fractional a) => [a] -> Int -> a
        meanOfPreivous _ 0 = -999999999 
        meanOfPreivous xs n = (sum (take n xs) ) / n'
            where n' = fromIntegral n
    in and [ x > ( meanOfPreivous xs i ) | (i, x) <- zip [0..] xs ]

-- ************ EndOf Exercise 1-FP.16


-- **** Exercise 1-FP.17
sublist :: Eq a => [a] -> [a] -> Bool
sublist _ [] = False
sublist xs (y:ys) = 
    let isPrefixOf :: Eq a => [a] -> [a] -> Bool
        isPrefixOf xs ys = and ( zipWith (==) xs ys )
    in ( isPrefixOf xs (y:ys) ) || (sublist xs ys)

partial_sublist :: Eq a => [a] -> [a] -> Bool
partial_sublist [] _ = True
partial_sublist (x:xs) ys =
    let isIn :: Eq a => a -> [a] -> Bool
        isIn x ys = or $ map (==x) ys 
    in  (isIn x ys) && (partial_sublist xs ys)

-- ************ EndOf Exercise 1-FP.17


-- **** Exercise 1-FP.18

-- (1)
bsort :: Ord a => [a] -> [a]
bsort [] = []
bsort xs = 
    let 
        maxEle = maximum xs
        maxElements = filter (==maxEle) xs
        restElements = filter (/=maxEle) xs
    in (bsort restElements) ++ maxElements

prop_bsort :: [Int] -> Bool
prop_bsort xs = sort xs == bsort xs

-- ** EndOf (1)

-- (2)
mmsort :: Ord a => [a] -> [a]
mmsort [] = []
mmsort xs = 
    let
        count :: Ord a => [a] -> Int
        count [] = 0
        count (x:xs) = 1 + (count xs)
        maxNum = maximum xs
        minNum = minimum xs
        countOfmaxNum = count ( filter (==maxNum) xs )
        countOfminNum = count ( filter (==minNum) xs )
        repeatedArray :: Ord a => a -> Int -> [a] -- Consider repeated element
        repeatedArray x 0 = []
        repeatedArray x n = [x] ++ ( repeatedArray x (n-1) )
        restElements = ( filter (/= maxNum) . filter (/= minNum) ) xs
    in
        if maxNum == minNum 
            then xs 
            else (repeatedArray minNum countOfminNum) ++ (mmsort restElements) ++ (repeatedArray maxNum countOfmaxNum)

prop_mmsort :: [Int] -> Bool
prop_mmsort xs = (sort xs) == (mmsort xs)

-- ** EndOf (2)

-- (3)
isort :: Ord a => [a] -> [a]
isort [] = []
isort xs = foldr insert [] xs


prop_isort :: [Int] -> Bool
prop_isort xs = sort xs == isort xs
-- ** EndOf (isort)

-- (4)
msort :: Ord a => [a] -> [a]
msort [] = []
msort [a] = [a]
msort xs = 
    let 
        count :: Ord a => [a] -> Int
        count [] = 0
        count (x:xs) = 1 + (count xs)
        
        n = count xs
        k = div (count xs) 2
        
        left = take k xs
        right = drop k xs

        merge :: Ord a => [a] -> [a] -> [a]
        merge xs [] = xs
        merge [] ys = ys
        merge [x] [y] = if x < y then [x, y] else [y, x]
        merge (x:xs) (y:ys)
            | x < y = [x] ++ ( merge xs (y:ys) )
            | otherwise = [y] ++ ( merge (x:xs) ys )

    in merge (msort left) (msort right)

prop_msort :: [Int] -> Bool
prop_msort xs = sort xs == msort xs

-- ** EndOf (4)

-- (5)
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = 
    let left =  qsort ( filter (<x) xs ) 
        mid = ( filter (==x) xs ) ++ [x]
        right = qsort ( filter (>x) xs ) 
    in left ++ mid ++ right

prop_qsort :: [Int] -> Bool
prop_qsort xs = (sort xs) == (qsort xs)

-- ** EndOf (5)

-- ************ EndOf Exercise 1-FP.18

-- **** Exercise 1-FP.19

myflip :: (a->b->c) -> (b->a->c)
--myflip f = let g y x = f x y in g
myflip f = (\y x -> f x y)

-- ************ EndOf Exercise 1-FP.19

-- **** Exercise 1-FP.20

-- Point-free style
transform :: [Char] -> [Char]
transform = let 
    upper = map (\c -> if ('a' <= c && c <= 'z') then chr $ ord c - 32 else c )
    filtering = filter (\c -> if ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z') then True else False)
    in upper . filtering . reverse

--transform s = let 
--    upper = map (\c -> if ('a' <= c && c <= 'z') then chr $ ord c - 32 else c )
--    filtering = filter (\c -> if ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z') then True else False)
--    in (upper . filtering . reverse) s

-- ************ EndOf Exercise 1-FP.20