{- |
Module      : Fixtures
Description : Armazenamento de alguns dados
Copyright   : Diogo Marques
-}

module Fixtures where

import Ordenacao
import Questoes


m1 :: [Either Int Int]
m1 = [Right 6, Right 3, Right 8, Right 7, Right 8]

m2 :: [Either String String]
m2 = [Left "foo", Left "portugal", Left "bar", Left "portugal", Left "baz"]

m3 :: [Maybe Int]
m3 = [Nothing,Nothing,Nothing,Nothing]