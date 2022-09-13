module Recursion where

factorial :: Int -> Int
factorial n = product [1..n]

-- or define factorial using itself
factorial2 0 = 1
factorial2 n = n * factorial2 (n-1)

-- m ^ n
-- redefining pow
pow m 0 = 1 -- base case for the end of recursion, must be defined first otherwise it won't match
pow m n = m * pow m (n-1)

{-
    pow 2 4
        = 2 * pow 2 (4-1)
        = 2 * pow 2 3
        = 2 * (2 * pow 2 (3-1))
        = 2 * (2 * (2 * pow 2 (2-1)))
        = 2 * (2 * (2 * (2 * pow 2 (1-1))))
        = 2 * (2 * (2 * (2 * 1)))
-}

-- we can also define recursive types
-- define a natural number as either 0
-- or a successor of another natural number
-- nat contains an infinite sequence of values
data Nat = Zero | Succ Nat
    deriving Show -- derives from the Show class to be able to print it in ghci

zero = Zero
one = Succ Zero
two = Succ (one)
three = Succ (Succ (Succ Zero))

nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + (nat2int n)

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat x = Succ (int2nat (x-1))

addnat :: Nat -> Nat -> Nat
addnat m n = int2nat (nat2int m + nat2int n)

-- better implementation which is faster because
-- we don't have to convert it back to Int each time
addnatb :: Nat -> Nat -> Nat
addnatb Zero n = n
addnatb (Succ m) n = Succ (addnatb m n)

-- data type to represent an expression
-- for example 1 + 2 * 3
data Expr = Val Int | Add Expr Expr
                    | Mult Expr Expr
    deriving Show

expr = Add (Mult (Val 2) (Val 5)) (Val 6) -- "2 * 5 + 7"

-- pattern match on an expression to return the Int result
-- run eval expr after importing it on ghci or in main
eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y 
eval (Mult x y) = eval x * eval y

-- binary trees structure (data type)
data Tree = Leaf Int | Node Int Tree Tree

containedInTree :: Int -> Tree -> Bool
containedInTree m (Leaf n) = m == n
-- for the condition where we match a node we
-- use recursion on containedInTree with the Leaf
-- pattern matching and check both trees t1 and t2
-- in or with the first condition

{-containedInTree m (Node n t1 t2) = m == n
                                   || containedInTree m t1
                                   || containedInTree m t2
-}
-- search tree implementation, this works only if the tree is sorted from left to right in asc order
{-containedInTree m (Node n t1 t2) = 
    | m == n = True -- if it's the current leaf then return true
    | m < n = containedInTree m t1 -- only look in the left tree
    | m > n = containedInTree m t2 -- only look in the right tree
-}

-- ++ merges two lists
tree2list :: Tree -> [Int]
tree2list (Leaf n) = [n]
tree2list (Node n t1 t2) = tree2list t1 ++
                           [n] ++
                           tree2list t2

-- create a variable of type Tree
-- this is the tree represented
-- with the first Node having 5 as Int
-- and two other Tree(s), where one is just
-- a Leaf Int and the other is another Node
{-
        5
    |       |
    4       3
        |       |
        2       1
-}
tree :: Tree
tree = Node 5 (Leaf 4)
              (Node 3 (Leaf 2)
                      (Leaf 1))

-- run with containedInTree 5|4|3|2|1 tree to return True
-- run with containedInTree 7 tree to return False