{- |
Module      : Testes
Description : Palete de testes para as 50 questões de programação funcional
Copyright   : Diogo Marques 
-}

module Testes where

import Prelude hiding (enumFromTo, enumFromThenTo, (++), (!!), reverse, take, drop, zip, replicate, concat, unwords, unlines, lookup)
import Test.HUnit
import Questoes

correrTestes = runTestTT testes

testes =
  test
    [ "Teste 1" ~: enumFromTo 1 10 ~=? [1,2,3,4,5,6,7,8,9,10]
    , "Teste 2" ~: enumFromTo 5 5 ~=? [5]
    , "Teste 3" ~: enumFromTo 5 1 ~=? []
    , "Teste 4" ~: enumFromTo (-7) 3 ~=? [-7,-6,-5,-4,-3,-2,-1,0,1,2,3]
    , "Teste 5" ~: enumFromThenTo 1 3 10 ~=? [1,3,5,7,9]
    , "Teste 6 - teste para ciclo infinito" ~: enumFromThenTo 10 5 20 ~=? [10,15,20]
    , "Teste 7" ~: enumFromThenTo 11 2 2 ~=? []
    , "Teste 8" ~: enumFromThenTo 1 4 10 ~=? [1,4,7,10]
    , "Teste 9" ~: (++) [] [6] ~=? [6]
    , "Teste 10" ~: (++) [5] [] ~=? [5]
    , "Teste 11" ~: (++) [5,9] [0,2] ~=? [5,9,0,2]
    , "Teste 12" ~: (++) [] [1,2,3,4,5] ~=? [1,2,3,4,5]
    , "Teste 13" ~: (!!) "ola" 0 ~=? 'o'
    , "Teste 14" ~: (!!) "computador " 10 ~=? ' '
    , "Teste 15" ~: (!!) [1,5,9,6,3] 2 ~=? 9
    , "Teste 16" ~: (!!) [5,2,5,9,6,8,3,6] 7 ~=? 6
    , "Teste 17" ~: (!!) "funcional" 5 ~=? 'o'
    , "Teste 18" ~: reverse "ola" ~=? "alo"
    , "Teste 19" ~: reverse [1,2,3,4,5] ~=? [5,4,3,2,1]
    , "Teste 20" ~: reverse "computador" ~=? "rodatupmoc"
    , "Teste 21" ~: take 0 "ola" ~=? ""
    , "Teste 22 - teste tricky" ~: take 10 [1] ~=? [1]
    , "Teste 23" ~: take 3 [1,2,3,4,5] ~=? [1,2,3]
    , "Teste 24" ~: take 10 "computador" ~=? "computador"
    , "Teste 25" ~: take 2 "ola" ~=? "ol"
    , "Teste 26" ~: drop 2 "ola" ~=? "a"
    , "Teste 27 - teste tricky" ~: drop 10 "ola" ~=? ""
    , "Teste 28" ~: drop 3 [1,2,3,4,5,6] ~=? [4,5,6]
    , "Teste 29" ~: drop 10 "computador" ~=? ""
    , "Teste 30" ~: drop 5 "computador" ~=? "tador"
    , "Teste 31" ~: zip "ol" [1,2,3] ~=? [('o',1),('l',2)]
    , "Teste 32" ~: zip "olas" [1,2,3] ~=? [('o',1),('l',2),('a',3)]
    , "Teste 33" ~: zip "olas" [1,2,3,4,5] ~=? [('o',1),('l',2),('a',3),('s',4)]
    , "Teste 34" ~: replicate 5 10 ~=? [10,10,10,10,10]
    , "Teste 35" ~: replicate 0 5 ~=? []
    , "Teste 36" ~: intersperse 1 [10,20,30] ~=? [10,1,20,1,30]
    , "Teste 37" ~: intersperse 1 [10,20,30,1] ~=? [10,1,20,1,30,1,1]
    , "Teste 38" ~: intersperse 1 [10] ~=? [10]
    , "Teste 39" ~: group [1,2,2,3,4,4,4,5,4] ~=? [[1],[2,2],[3],[4,4,4],[5],[4]]
    , "Teste 40" ~: group [1,2,2,3,2,6,5,5,5,6,6,9,8,8,2,4,4,4,5,4] ~=? [[1],[2,2],[3],[2],[6],[5,5,5],[6,6],[9],[8,8],[2],[4,4,4],[5],[4]]
    , "Teste 41" ~: group [10] ~=? [[10]]
    , "Teste 42" ~: concat [[1],[2,2],[3],[4,4,4],[5],[4]] ~=? [1,2,2,3,4,4,4,5,4]
    , "Teste 43" ~: concat [[1]] ~=? [1]
    , "Teste 44" ~: concat [[2,2,2,2,2,2,2],[],[],[],[]] ~=? [2,2,2,2,2,2,2]
    , "Teste 45" ~: inits [11,21,13] ~=? [[],[11],[11,21],[11,21,13]]
    , "Teste 46" ~: inits [11] ~=? [[],[11]]
    , "Teste 47" ~: tails [11,21,13] ~=? [[11,21,13],[21,13],[13],[]]
    , "Teste 48" ~: tails [11] ~=? [[11],[]]
    , "Teste 49" ~: tails [1,2,3,4,5,6,7,8,9] ~=? [[1,2,3,4,5,6,7,8,9],[2,3,4,5,6,7,8,9],[3,4,5,6,7,8,9],[4,5,6,7,8,9],[5,6,7,8,9],[6,7,8,9],[7,8,9],[8,9],[9],[]]
    , "Teste 50" ~: heads [[2,3,4],[1,7],[],[8,5,3]] ~=? [2,1,8]
    , "Teste 51" ~: heads ["ola","meus","amigos"] ~=? "oma"
    , "Teste 52" ~: heads [[2,2,2,2,2,2,2],[],[],[],[]] ~=? [2]
    , "Teste 53" ~: heads [[1],[2,2],[3],[4,4,4],[5],[4]] ~=? [1,2,3,4,5,4]
    , "Teste 54" ~: total [[1],[2,2],[3],[4,4,4],[5],[4]] ~=? 9
    , "Teste 55" ~: total [[2,2,2,2,2,2,2],[],[],[],[]] ~=? 7
    , "Teste 56" ~: total [[1],[2,2],[3],[2],[6],[5,5,5],[6,6],[9],[8,8],[2],[4,4,4],[5],[4]] ~=? 20
    ]


