{- |
Module      : Questoes
Description : 50 quesões de programação funcional
Copyright   : Diogo Marques 
-}

module Questoes where 

import Prelude hiding (enumFromTo, enumFromThenTo, (++), (!!), reverse, take, drop, zip, replicate, concat, unwords, unlines, lookup)

data Movimento = Norte | Sul | Este | Oeste deriving Show

type Ponto = (Float,Float)

data Rectangulo = Rect Ponto Ponto

data Equipamento = Bom | Razoavel | Avariado deriving Show

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

-- | Exercicio 31

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x l@(y:ys) | x < y = [x] ++ l
                  | otherwise = y : insert x ys

-- | Exercicio 32

unwords :: [String] -> String
unwords [] = ""
unwords [x] = x
unwords (x:xs) = x ++ " " ++ unwords xs

-- | Exercicio 33

unlines :: [String] -> String
unlines [] = ""
unlines (x:xs) = x ++ ['\n'] ++ unlines xs

-- | Exercicio 34

pMaior :: Ord a => [a] -> Int
pMaior (x:xs) = auxpMaior 1 0 x xs
    where
        auxpMaior _ pm _ [] = pm
        auxpMaior pa pm maior (x:xs) | x > maior = auxpMaior (pa+1) pa x xs
                                     | otherwise = auxpMaior (pa+1) pm maior xs

-- | Exercicio 35

lookup :: Eq a => a -> [(a,b)] -> Maybe b
lookup _ [] = Nothing
lookup x ((y,ys):z) | x == y = Just ys
                    | otherwise = lookup x z

-- | Exercicio 36

preCrescente :: Ord a => [a] -> [a]
preCrescente [] = []
preCrescente (x:xs) | x < head xs = x : preCrescente xs
                    | otherwise = [x]

-- | Exercicio 37

iSort :: Ord a => [a] -> [a] -- ^ versão com insertion sort
iSort [] = []
iSort (x:xs) = insert x (iSort xs)

iSort' :: Ord a => [a] -> [a] -- ^ versão com bublle sort (o Obama não curte desta)
iSort' [] = []
iSort' l = replica k auxiSort' l
    where
        k = length l -- ^ tenho de repetir este processo tantas vezes quanto o tamanho do array
        replica 0 _ x = x
        replica n f x = replica (n-1) f (f(x))
        auxiSort' [x] = [x]
        auxiSort' (x:xs) | x > head xs = head xs : auxiSort' ([x] ++ (tail xs))
                         | otherwise = x : auxiSort' xs

-- | Exercicio 38

menor :: String -> String -> Bool
menor [] [] = True
menor [] _ = True
menor _ [] = False
menor (x:xs) (y:ys) |  x == y = menor xs ys
                    | x < y = True
                    | otherwise = False

-- | Exercicio 39

elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet _ [] = False
elemMSet x ((y,ys):z) | x == y = True
                      | otherwise = elemMSet x z

-- | Exercicio 40

convertMSet :: [(a,Int)] -> [a]
convertMSet [] = []
convertMSet (x:xs) = replicate (snd x) (fst x) ++ convertMSet xs

-- | Exercicio 41

insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet x [] = [(x,1)]
insereMSet x (y:ys) | x == fst y = (fst y, snd y + 1) : ys -- ^ não existem pares cuja primeira componente coincida
                    | otherwise = y : insereMSet x ys

-- | Exercicio 42

removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)] -- ^ acho que interpretei este exercicio de forma mais complexa do que aquilo que realmente pede
removeMSet _ [] = []
removeMSet x (y:ys) | x == fst y && snd y == 1 = ys
                    | x == fst y = (fst y, snd y - 1) : ys
                    | otherwise = y : removeMSet x ys

-- | Exercicio 43

constroiMSet :: Ord a => [a] -> [(a,Int)] -- ^ o maldito do Jbb chumbou-me na 50 questões porque corrigiu mal este exercicio, "lista ordenada" aprende a ler Jbb
constroitMSet [] = []
constroiMSet l = auxconstroiMSet 1 (head l) (tail l)
    where
        auxconstroiMSet acc x [] = [(x,acc)]
        auxconstroiMSet acc x (y:ys) | x == y = auxconstroiMSet (acc+1) x ys
                                     | otherwise = (x,acc) : auxconstroiMSet 1 y ys

constroiMSet' :: Ord a => [a] -> [(a,Int)] -- ^ versão para listas não ordenadas
constroiMSet' [] = []
constroiMSet' l = constroiFinal k l 
    where
        k = nub' l
        conta x [] = 0
        conta x (y:ys) | x == y = 1 + conta x ys
                       | otherwise = conta x ys
        constroiFinal [] _ = []
        constroiFinal (x:xs) lista = (x,conta x lista) : constroiFinal xs lista

-- | Exercicio 44

partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers [] = ([],[])
partitionEithers l = case (head l) of Right x -> (e,x:d)
                                      Left x -> (x:e,d)
    where (e,d) = partitionEithers (tail l)

-- | Exercicio 45

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes l = case (head l) of Nothing -> catMaybes (tail l)
                               Just x -> x : catMaybes (tail l)

catMaybes' :: [Maybe a] -> [a] -- ^ versão com listas por compreensão (não façam isto no teste da 50 questões, o Jbb vai chumbar-vos)
catMaybes' lista = [x | Just x <- lista]

-- | Exercico 46

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x,y) (fx,fy) | x == fx && y == fy = []
                      | x < fx = Este : caminho (x+1,y) (fx,fy)
                      | x > fx = Oeste : caminho (x-1,y) (fx,fy)
                      | y < fy = Norte : caminho (x,y+1) (fx,fy)
                      | y > fy = Sul : caminho (x,y-1) (fx,fy)

-- | Exercicio 47

hasLoops :: (Int,Int) -> [Movimento] -> Bool
hasLoops _ [] = False
hasLoops x movs = elem x (auxhasLoops x movs)
    where
        auxhasLoops x [] = [x]
        auxhasLoops (x,y) (z:zs) = case z of Norte -> (x,y+1) : auxhasLoops (x,y+1) zs
                                             Sul -> (x,y-1) : auxhasLoops (x,y-1) zs
                                             Este -> (x+1,y) : auxhasLoops (x+1,y) zs
                                             Oeste -> (x-1,y) : auxhasLoops (x-1,y) zs

-- | Exercicio 48

contaQuadrados :: [Rectangulo] -> Int
contaQuadrados [] = 0
contaQuadrados ((Rect x y):z) | abs (fst x - snd x) == abs (fst y - snd y) = 1 + contaQuadrados z
                              | otherwise = contaQuadrados z

-- | Exercicio 49

--areaTotal :: [Rectangulo] -> Float
--areaTotal [] = 0
--areaTotal

-- | Exercicio 50

naoReparar :: [Equipamento] -> Int
naoReparar [] = 0
naoReparar l = case (head l) of Avariado -> naoReparar (tail l)
                                _ -> 1 + naoReparar (tail l)