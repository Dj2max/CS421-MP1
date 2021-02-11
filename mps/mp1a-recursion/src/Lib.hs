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

rev_helper :: [a] -> [a] -> [a]
rev_helper retval [] = retval
rev_helper retval (y:ys) = rev_helper (y:retval) ys

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev_helper [] (x:xs)

--- ### app

app :: [a] -> [a] -> [a]
app [] [] = []
app x [] = x
app [] y = y
app (x:xs) y = x:(app xs y)

--- ### inclist

inclist :: Num a => [a] -> [a]
inclist [] = [] --base case first
inclist (x:xs) = x + 1 : inclist (xs)

--- ### sumlist

sumlist :: Num a => [a] -> a
sumList [] = 0
sumList (x:xs) = x + sumList xs

--- ### myzip

myzip :: [a] -> [b] -> [(a, b)]
myzip [] [] = []
myzip x [] = []
myzip [] y = []

myzip (x:xs) (y:ys) = ( (x, y)  : myzip (xs) (ys) )

--- ### addpairs

myzip_help :: [a] -> [a] -> [(a, a)] -- this should only be called by actualadd or things where they must be the same type
myzip_help [] [] = []
myzip_help x [] = []
myzip_help [] y = []
myzip_help (x:xs) (y:ys) = ( (x, y)  : myzip_help (xs) (ys) )

actualadd :: (Num a) => [(a, a)] -> [a]
actualadd [] = []
actualadd (x:xs) = ( fst(x) + snd(x) : actualadd (xs) ) 

addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs [] [] = []
addpairs x [] = []
addpairs [] y = []
addpairs (x:xs) (y:ys) = actualadd(myzip_help (x:xs) (y:ys))

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
