{- |
Module      : Listas de Movimentos
Description : Ordenação lexicográfica de listas de Movimentos
Copyright   : Diogo Marques 
-}

module Ordenacao where

import Data.List
import Questoes


checkMaior :: Movimento -> Movimento -> Bool
checkMaior Este _ = False
checkMaior Norte Este = True
checkMaior Norte _ = False
checkMaior Oeste Sul = False
checkMaior Oeste _ = True
checkMaior Sul _ = True


ordCaminho :: [Movimento] -> [Movimento]
ordCaminho [] = []
ordCaminho l@(x:xs) = replicaM k (auxordCaminho) (x:xs)
    where
        k = length l
        replicaM 0 _ x = x
        replicaM n func x  = replicaM (n-1) func (func x)
        auxordCaminho :: [Movimento] -> [Movimento]
        auxordCaminho [] = []
        auxordCaminho [x] = [x]
        auxordCaminho (x:y:z) | checkMaior x y = y : auxordCaminho (x:z)
                              | otherwise = x : auxordCaminho (y:z)