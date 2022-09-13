module Functions where

double x = x + x
quadruple x = double(double x)

-- generate list of 1 thru n and multiply each member 
factorial n = product [1 .. n]

-- we use backticks otherwise to apply the div
-- to sum and length [div(sum length)]
-- we would have to write
-- div (sum ns) (length ns)
average ns = sum ns `div` length ns

-- instead we can use a local variable
-- in order to non re-compute x + 1
--poly x = (x+1) * (x+1)
poly x = let
    y = x + 1
    in y * y

-- without local variables
--sumEvenOdds xs = foldr (+) 0 (map (+1)
--                (filter (\x -> x `mod` 2 == 0) xs))
-- let and where are the same thing
sumEvenOdds xs = sum (incr (evens xs))
    where
        sum xs = foldr (+) 0 xs
        incr xs = Prelude.map (+1) xs
        evens xs = filter (\x -> x `mod` 2 == 0) xs

-- statements in let/where do not need to be in
-- order
f = let
        x = 5
        y = x + 6
        z = y + x
    in z

-- polymorphic functions
-- functions that can be applied to
-- different input types
-- a and b are type parameters
-- much like template programming
--fst2 :: (a, b) -> a

-- function composition
-- chain together multiple functions
-- in this case lastElement will take a list
-- of a and return the last element a
lastElement :: [a] -> a
lastElement = Prelude.head . reverse

-- without polymorphic functions
lastElement2 xs = (Prelude.head . reverse) xs

-- we can put the $ sign to eliminate sets of parenthesis
-- (i.e opening and closing)
f2 (x: xs) = foldr (+) 0 $ Prelude.map (+2) $ reverse $ init $ x: xs

-- type classes
-- allows to define functions that have different implementations
-- depending on the type of their input (i.e methods)
-- example the Eq class in the Prelude
--class Eq a where
--    (==) :: a -> a -> Bool
--    (/=) :: a -> a -> Bool

-- all basic types have instances of the Eq type class
-- defined in the Prelude
--instance Eq Bool where
--    x == y = if x then (if y then True else False)
--                  else (if y then False else True)
--    x /= y = not (x == y)

-- overloaded functions
-- a polymorphic function is called overloaded
-- if its type contains one or more class constraints
-- example

-- a can be any type except it has to be part of the Num class
-- sum          :: Num a => [a] -> a
-- fromIntegral :: (Num b, Integral a) => a -> b

-- the / is part of the Fractional class
-- this is not the most general type
--divide :: Float -> Float -> Float
divide :: Fractional a => a -> a -> a
divide x y = x / y

-- this does integral division
divide2 x y = x `div` y

-- list representation
-- under the hood
--[1,2,3,4] == 1:(2:(3:(4:[])))

-- functions can be defined using (x:xs) patterns
-- where x is the first element of the list and xs the rest
-- to use them we need to prepend the module Conditionals
-- like Conditionals.head otherwise it conflicts with the head and tail
-- functions defined in the Prelude
head :: [a] -> a
-- this is not entirely correct because head returns a value and not a list
--head [] = [] -- add pattern matching for the empty list, otherwise x,xs wont match it
head (x:xs) = x

tail :: [a] -> [a]
tail [] = [] -- add pattern matching for the empty list, otherwise x,xs wont match it
tail (x:xs) = xs

-- lambda expressions
-- basically anonymous functions like in python and javascript
add = \x -> (\y -> x + y)

-- map, takes a function as its argument
-- and applies it to each element of a list
add1 :: Int -> Int
add1 x = x + 1

sum1 :: [Int] -> [Int]
sum1 xs = Prelude.map add1 xs

-- sum1 without the add1 function
-- where we replace it with a lambda expression
sum12 xs = Prelude.map (\x -> x + 1) xs

-- defining our map function
--map :: (a -> b) -> [a] -> [b]
--map f []
--map f (x:xs) = f x : (map f xs)
