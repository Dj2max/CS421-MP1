--dmackey2
--kedaruk2
--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where

-- This line imports the Prelude module without certain functions
import Prelude hiding ( take, drop, reverse
                      , zip, zipWith
                      , map, foldl, foldr
                      , iterate, repeat
                      , replicate, cycle
                      , (++)
                      )
-- When you are allowed to use builtin functions Prepend them with "P."
-- for example `P.take`
import qualified Prelude as P

--- Problems
--- ========

--- Recursion
--- ---------

--- ### mytake

mytake :: Int -> [a] -> [a]
mytake a [] = []
mytake a (x:xs) | a <= 0 = []
                | otherwise =  x:(mytake (a-1) xs)

--- ### mydrop

mydrop :: Int -> [a] -> [a]
mydrop a [] = []
mydrop a (x:xs) | a <= 0 = (x:xs)
                | otherwise =  (mydrop (a-1) xs)

--- ### rev

rev :: [a] -> [a]
rev = undefined

--- ### app

app :: [a] -> [a] -> [a]
app = undefined

--- ### inclist

inclist :: Num a => [a] -> [a]
inclist [] = [] --base case first
inclist (x:xs) = x + 1 : inclist (xs)

--- ### sumlist

sumlist :: Num a => [a] -> a
sumList [] = 0
sumList (x:xs) = x + sumList xs

--- ### myzip

--probably wrong
myzip :: [a] -> [b] -> [(a,b)]
myzip [] [] = []
myzip x [] = []
myzip [] y = []
myzip (x:xs) (y:ys) = ((x) ++ (y)): myzip (xs ys)

--- ### addpairs

--doesnt work yet
addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs [] = []
addpairs (x:xs) (y:ys) = myzip((x:xs) (y:ys))

--- ### ones

ones :: [Integer]
ones = 1:ones

--- ### nats

nats :: [Integer]
nats = 0:inclist(nats)

--- ### fib

--this works for any given fib but MUST USE addpairs and create an infinite list
fib :: [Integer]
fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib (n-2)


--- Set Theory
--- ----------

--- ### add

add :: Ord a => a -> [a] -> [a]
add x [] = [x]
add x (y:ys) | x < y = x:y:ys
             | otherwise = y:add(x ys)

--- ### union

union :: Ord a => [a] -> [a] -> [a]
union = undefined

--- ### intersect

intersect :: Ord a => [a] -> [a] -> [a]
intersect = undefined

--- ### powerset

powerset :: Ord a => [a] -> [[a]]
powerset = undefined

--- Higher Order Functions
--- ----------------------

--- ### inclist'
inc x = x+1

inclist' :: Num a => [a] -> [a]
inclist' = map inc

--- ### sumlist'

sumlist' :: (Num a) => [a] -> a
sumlist' = foldr (\a b -> plus a b) 0
