{- |
Module      : Questoes
Description : 50 quesões de programação funcional
Copyright   : Diogo Marques 
-}

module Questoes where 

import Prelude hiding (enumFromTo, enumFromThenTo, (++), (!!), reverse, take, drop, zip, replicate, concat, unwords, unlines, lookup)

-- | Exercicio 1

enumFromTo :: Int -> Int -> [Int]
enumFromTo x y | x > y = []
               | otherwise = x : enumFromTo (x+1) y


-- | Exercicio 2

enumFromThenTo :: Int -> Int -> Int -> [Int]
enumFromThenTo x y z | x > z = []
                     | otherwise = x : enumFromThenTo y (2*y-x) z

-- | Exercicio 3

(++) :: [a] -> [a] -> [a]
(++) [] x = x
(++) (x:xs) y = x : (++) xs y

-- | Exercicio 4

(!!) :: [a] -> Int -> a
(!!) (x:xs) 0 = x
(!!) (x:xs) n = (!!) xs (n-1)

-- | Exercicio 5

reverse :: [a] -> [a]
reverse [] = []
reverse x = last x : reverse (init x)

-- | Exercicio 6

take :: Int -> [a] -> [a]
take 0 _ = []
take n [] = []
take n (x:xs) = x : take (n-1) xs

-- | Exercicio 7

drop :: Int -> [a] -> [a]
drop 0 x = x
drop _ [] = []
drop n (x:xs) = drop (n-1) xs

-- | Exercicio 8

zip :: [a] -> [b] -> [(a,b)]
zip _ [] = []
zip [] _ = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

-- | Exercicio 9

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n-1) x

-- | Exercicio 10

intersperse :: a-> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse x (y:ys) = y : x : intersperse x ys

-- | Exercicio 11

group :: Eq a => [a] -> [[a]]
group [] = []
group l = auxgroup l []
    where
        auxgroup [] acc = [acc]
        auxgroup [x] acc = [acc ++ [x]]
        auxgroup (x:xs) acc | x == head xs = auxgroup xs (acc ++ [x])
                            | otherwise = (acc ++ [x]) : auxgroup xs []

-- | Exercicio 12

concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ concat xs

-- | Exercicio 13

inits :: [a] -> [[a]]
inits [] = [[]]
inits l = inits (init l) ++ [l] -- ^ tenho de colocar [l], caso contrário o elemento do fim não teria uma lista vaiza para fechar a lista

-- | Exercicio 14

tails :: [a] -> [[a]]
tails [] = [[]]
tails l = l : tails (tail l)

-- | Exercicio 15

heads :: [[a]] -> [a]
heads [] = []
heads (x:xs) | length x == 0 = heads xs
             | otherwise = head x : heads xs

-- | Exercicio 16

total :: [[a]] -> Int
total [] = 0
total (x:xs) = length x + total xs

-- | Exericico 17

fun :: [(a,b,c)] -> [(a,c)]
fun [] = []
fun ((a,b,c):z) = (a,c) : fun z

-- | Exercicio 18

cola :: [(String,b,c)] -> String
cola [] = ""
cola ((a,b,c):z) = a ++ cola z

-- | Exercicio 19

idade :: Int -> Int -> [(String,Int)] -> [String]
idade _ _ [] = []
idade ano i ((x,y):z) | ano - i >= y = x : idade ano i z  
                      | otherwise = idade ano i z

-- Exercicio 20

powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom n m | m < 0 = []
                  | otherwise = auxpower n m 0
    where
        auxpower n m counter | counter == m = []
                             | otherwise = n^counter : auxpower n m (counter+1)

-- | Exercicio 21

isPrime :: Int -> Bool -- ^ não utilizer o algoritmo que era pedido no enunciado, utilizei um mais básico
isPrime 1 = False -- ^ eu defendo que um numero primo tem dois divisores positivos
isPrime n = analisa n 2 
    where
        analisa n d | n <= d = True
                    | mod n d == 0 = False
                    | otherwise = analisa n (d+1)

-- | Exercicio 22

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) | x == y = isPrefixOf xs ys
                         | otherwise = False

-- | Exercicio 23

isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf [] _ = True
isSuffixOf _ [] = False
isSuffixOf x y | last x == last y = isSuffixOf (init x) (init y)
               | otherwise = False

-- | Exercicio 24

isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (x:xs) (y:ys) | x == y = isSubsequenceOf xs ys
                              | otherwise = isSubsequenceOf (x:xs) ys

-- | Exercicio 25

elemIndicies :: Eq a => a -> [a] -> [Int]
elemIndicies _ [] = []
elemIndicies x l = auxIndices x l 0
    where
        auxIndices _ [] _ = []
        auxIndices x (y:ys) n | x == y = n : auxIndices x ys (n+1)
                              | otherwise = auxIndices x ys (n+1)

-- | Exercicio 26

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) | elem x xs = nub xs
           | otherwise = x : nub xs

nub' :: Eq a => [a] -> [a] -- ^ elementos ordenados tal como aparece no exemplo do enuciado
nub' l = auxnub' l []
    where
        auxnub' [] _ = []
        auxnub' (x:xs) acc | elem x acc = auxnub' xs acc
                           | otherwise = x : auxnub' xs (acc ++ [x])

-- | Exercicio 27

delete :: Eq a => a -> [a] ->[a]
delete _ [] = []
delete x (y:ys) | x == y = ys
                | otherwise = y : delete x ys

-- | Exercicio 28

(\\) :: Eq a => [a] -> [a] -> [a]
(\\) x [] = x
(\\) [] _ = []
(\\) l (x:xs) = (\\) (delete x l) xs

-- | Exercicio 29

union :: Eq a => [a] -> [a] -> [a]
union [] x = x
union x [] = x
union l (x:xs) | elem x l = union l xs
               | otherwise = union (l ++ [x]) xs

-- | Exercicio 30

intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []
intersect _ [] = []
intersect (x:xs) y | elem x y = x : intersect xs y
                   | otherwise = intersect xs y