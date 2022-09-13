module Test where

-- to load a module in ghci use :l
-- to reload the last module loaded use :r

doubleMe x = x + x
--doubleUs x y = x*2 + y*2
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                        then x
                        else x*2

{-
' at the end of the function name.
That apostrophe doesn't have any special meaning in Haskell's syntax
-}
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

-- a definition (or a name) is a function itself but takes no params
name = "Mario"

-- ############################################################################
{- LISTS -}
-- ############################################################################

-- add lists (strings are lists of chars)
-- appending an element to a list causes haskell to
-- traverse the whole list so it's not performant
newList = [1,2,3,4,5] ++ [6,7,8]
helloworld = "hello" ++ " " ++ "world"
helloworld' = ['h','e','l','l','o'] ++ [' '] ++ ['w','o','r','l','d']

-- adding an element to the head is though
headList = 'A' : " SMALL CAT"
headList' = 5:[1,2,3,4,5]

-- get an element of a list
element6 = newList !! 6 -- indices start at 0

-- set comprehensions
-- S = {2 * x | x belongs N, x <= 10}
newSet = take 10 [2,4..]
-- with list comprehensions
-- x is drawn from [1..10] and the function x*2 is applied
-- to each element drawn
newSet' = [x*2 | x <- [1..10]]
-- with a predicate (also called filtering)
newSetPredicate = [x | x <- [50..100], x `mod` 7 == 3]

-- function that takes a list and returns a list with
-- a list comprehension that returns a value based on the value of x
-- which is first filtered drawn from the input list and applyint the odd function
-- (which returns True only if the x is odd)
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

-- we can have multiple predicates with "," and an element
-- must satisfy all predicates
multPredicates = [ x | x <- [10..20], x /= 13, x /= 15, x /= 20 ]

-- draw from multiple lists
multDraws = [ x * y | x <- [2,5,10], y <- [8,10,11], x*y > 50 ]

nouns = ["hobo","frog","people"]
adjectives = ["lazy","grouchy","scheming"]
nounsadj = [ adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns ]

-- rewrite the length function
-- "_" is like it's used in other languages
-- when we won't use the variable drawn in this case
-- so we draw every number from the list and we apply the
-- 1 value for every item, and then apply the sum function
-- to the list of 1's
length' xs = sum [1 | _ <- xs]

-- predicates and list comprehension on list of lists
xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]
removeOdds xxs = [ [ x | x <- xs, even x ] | xs <- xxs ]

-- ############################################################################
{- TUPLES -}
-- ############################################################################

-- tuples don't have to be homogenous (can contain different types)
twoDVecList = [[1,2],[8,11],[4,5]]
twoDVecTuple = [(1,2),(8,11),(4,5)]

-- using zip to match the elements of 2 lists
-- into tuples (returns a single list of tuples)
-- zipLists[1,2,3,4,5] [5,5,5,5,5]  
zipLists xs ys = zip xs ys

-- find all right triangles with sides <= 10 with a perimeter of 24
-- compose triples by drawing from lists that go from 1 to 10
-- then put the constraint that a^2 + b^2 == c^2 as a predicate
triangles = [ (a,b,c) | a <- [1..10], b <- [1..10], c <- [1..10], a^2 + b^2 == c^2, a+b+c == 24 ]

-- ############################################################################
{- TYPES AND TYPECLASSES -}
-- ############################################################################

-- to examine a type in ghci use :t, it works on functions as well
-- it's best practice to declare functions with types
-- even if they can be inferred
removeNonUppercase :: [Char] -> [Char] -- or :: String -> String
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z'] ]

-- The return type is the last item in the declaration and the parameters are the first three
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- polymorphic functions are functions with type variables
-- (similar to generic/template programming in other languages)
-- for example the fst function is of type: fst :: (a, b) -> a
-- it takes two types and returns a type which is equal to that of the first component
-- a, b do not have to be different type they can also be the same
-- :t (==) gives (==) :: (Eq a) => a -> a -> Bool which is the signature of the == function
-- since == is an infix function to use it as a prefix function we have to surround it in ()
-- much like prefix function have to be surrounded by `` to be used as infix functions
-- everything before => is a class constraint which for the == functions tells us that
-- the values must be members of the Eq class.

-- A typeclass is a sort of interface that defines some behavior.
-- If a type is a part of a typeclass, that means that it supports and implements the behavior the typeclass describes (like Rust's traits).

-- ############################################################################
{- PATTERN MATCHING -}
-- ############################################################################

-- patterns are checked from top to bottom
-- the last pattern matches anything and binds it to x
-- all patterns should be accounted for like in Rust
lucky :: (Integral a) => a -> String
lucky 13 = "LUCKY NUMBER!"
lucky x = "Sorry, out of luck!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial x = x * factorial (x - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
--addVectors a b = (fst a + fst b, snd a + snd b) -- without pattern matching
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- fst and snd and trd for triples
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

-- pattern matching with list comprehensions
-- returns a list of values where the value
-- is the sum of the two values in the tuple
xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)] 
sumTupleVal xs = [a+b | (a,b) <- xs]

-- re-implementation of Prelude.head
head' :: [a] -> a
head' [] = error "Can't call head on an empty list"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
-- (x:[]) can also be [x] since [1,2,3] is syntactic sugar for 1:2:3:[].
-- same for (x:y:[]). To bind more than one value we need to surround them with ()
tell (x:[]) = "The list has element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "The list is long. It starts with: " ++ show x ++ "," ++ show y ++ "..."

-- length using pattern matching and recursion instead of list comprehension
length'' :: (Num b) => [a] -> b
length'' [] = 0
length'' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- as patterns are used to break something up
-- according to a pattern and binding it to names
-- i.e, xs@(x:y:ys) will match exactly the same thing as x:y:ys but
-- we can use the name xs to reference it in the function body
capital :: String -> String
capital "" = ""
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- ############################################################################
{- GUARDS -}
-- ############################################################################
-- a boolean expression. If it evaluates to True, then the corresponding function body is used
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | weight / height ^ 2 <= 18.5 = "Under weight"
    | weight / height ^ 2 <= 25.0 = "Normal weight"
    | weight / height ^ 2 <= 30.0 = "Over weight"
    | otherwise   = "Obese" -- similar to the default branch in a pattern matching expression

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b     = a
    | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b     = GT
    | a == b    = EQ
    | otherwise = LT

-- ############################################################################
{- WHERE -}
-- ############################################################################

-- declare variables scoped to the function
bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
    | bmi <= underweight = "Under weight"
    | bmi <= normalweight = "Normal weight"
    | bmi <= overweight = "Over weight"
    | otherwise   = "Obese" -- similar to the default branch in a pattern matching expression
    where bmi = weight / height ^ 2
          underweight = 18.5
          normalweight = 25.0
          overweight = 30.0 -- can also be (underweight, normalweight, overweight) = (18.5,25.0,30.0)

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

-- we can also define functions in a where block
calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

-- ############################################################################
{- LET -}
-- ############################################################################

-- let <bindings> in <expressions>, the name defined in the let
-- part are accessible to the expression in the in part.
-- let bindings are expressions themselves, whereas where bindings
-- are just constructs, so let can be used in place of any expression (for example and if else expression)
-- or to introduce functions in a local scope
-- the values of variables should be aligned or separated by ";" if doing it inline
-- ghci> (let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar)

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r^2
    in  sideArea + 2 * topArea

destructureTriple :: (a,a,a) -> [a]
destructureTriple (x,y,z) =
    let (a,b,c) = (x,y,z)
    in  [a,b,c]

-- calcBmis with let instead of where
-- We include a let inside a list comprehension much like we would a predicate,
-- only it doesn't filter the list, it only binds to names.
-- bmi >= 25.0 is a predicate and goes after the let part
calcBmis' :: (RealFloat a) => [(a,a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

-- ############################################################################
{- CASE EXPRESSIONS -}
-- ############################################################################

-- like switch statements in C or Java
-- case expression of pattern -> result
--                    pattern -> result

-- expression is matched against the patterns
-- pattern matching can be done only when defining functions
-- but case expressions can be used anywhere

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."

-- with pattern matching and where
-- since pattern matching in function definitions
-- is syntactic sugar for case expressions
describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
    where what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list."

-- ############################################################################
{- RECURSION -}
-- ############################################################################

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "There is no maximum for an empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs

maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "There is no maximum for an empty list"
maximum'' [x] = x
maximum'' (x:xs) = max x (maximum' xs)

-- x:replicate' (n-1) x
-- because we return a list that has x as the head
-- and then x replicated n-1 times
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0   = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys