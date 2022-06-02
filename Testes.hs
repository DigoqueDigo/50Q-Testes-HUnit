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
    , "Teste 6" ~: enumFromThenTo 4 2 (-6) ~=? [4,2,0,-2,-4,-6]
    , "Teste 7" ~: enumFromThenTo 6 8 2 ~=? []
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
    , "Teste 57" ~: fun [("rui",3,2), ("maria",5,2), ("ana",43,7)] ~=? [("rui",2), ("maria",2), ("ana",7)] 
    , "Teste 58" ~: fun [("rui",5,2), ("ola", 8,999)] ~=? [("rui",2),("ola",999)] 
    , "Teste 59" ~: fun [("",3,2), ("",5,2), ("",43,7)] ~=? [("",2), ("",2), ("",7)]
    , "Teste 60" ~: cola [("rui",3,2), ("maria",5,2), ("ana",43,7)] ~=? "ruimariaana" 
    , "Teste 61" ~: cola [("   ",3,2), ("maria",5,2), ("",43,7)] ~=? "   maria" 
    , "Teste 62" ~: cola [("r",3,2), ("   ",5,2), ("ana",43,7)]~=? "r   ana" 
    , "Teste 63" ~: idade 2021 26 [("rui",1995), ("maria",2009), ("ana",1947)] ~=? ["rui", "ana"] 
    , "Teste 64" ~: idade 2000 26 [("rui",1995), ("maria",2009), ("  ",1947)]~=? ["  "]
    , "Teste 65" ~: idade 1500 26 [("rui",1995), ("maria",2009), ("ana",1947)]~=? [] 
    , "Teste 66" ~: powerEnumFrom 2 9 ~=? [1,2,4,8,16,32,64,128,256] 
    , "Teste 67" ~: powerEnumFrom 2 (-5) ~=? [] 
    , "Teste 68" ~: powerEnumFrom 3 8 ~=? [1,3,9,27,81,243,729,2187] 
    , "Teste 69" ~: powerEnumFrom 3 0 ~=? [] 
    , "Teste 70" ~: isPrime 877 ~=? True 
    , "Teste 71 - Número muito grande" ~: isPrime 877777 ~=? False
    , "Teste 72" ~: isPrime 99651 ~=? False
    , "Teste 73" ~: isPrime 719333 ~=? True
    , "Teste 74" ~: isPrefixOf [10,20] [10,20,30] ~=? True
    , "Teste 75" ~: isPrefixOf [10,30] [10,20,30] ~=? False
    , "Teste 76" ~: isPrefixOf "cami" "camionete" ~=? True
    , "Teste 77" ~: isPrefixOf [10] [10,50,60] ~=? True
    , "Teste 78" ~: isSuffixOf [10,20] [10,20,30] ~=? False
    , "Teste 79" ~: isSuffixOf [20,20] [10,20,30] ~=? False
    , "Teste 80" ~: isSuffixOf [30] [10,20,30] ~=? True 
    , "Teste 81" ~: isSuffixOf [10] [10,20,30] ~=? False
    , "Teste 82" ~: isSubsequenceOf [10] [1,2,3,5,6,10,1] ~=? True
    , "Teste 83" ~: isSubsequenceOf [10,1,1] [1,2,3,5,6,10,1] ~=? False
    , "Teste 84" ~: isSubsequenceOf [10,1,1] [1,2,3,5,6,10,1,1,10] ~=? True
    , "Teste 85" ~: isSubsequenceOf [2,3,6,10] [1,2,3,5,6,10,1] ~=? True
    , "Teste 86" ~: elemIndicies '0' "computador" ~=? []
    , "Teste 87" ~: elemIndicies 'o' "computador" ~=? [1,8]
    , "Teste 88" ~: elemIndicies '1' "1,2,3,4,5,6,7,8,9,10" ~=? [0,18]
    , "Teste 89" ~: elemIndicies 5 [4,8,9,3,6,2,1] ~=? []
    , "Teste 90 - manter a primeira ocurrência" ~: nub [1,2,3,1,1,2,2,5,3,3,6] ~=? [1,2,3,5,6]
    , "Teste 91 - manter a primeira ocurrência" ~: nub "comunicacao" ~=? "comunia"
    , "Teste 92 - manter a primeira ocurrência" ~: nub "funcional" ~=? "funcioal"
    , "Teste 93 - manter a primeira ocurrência" ~: nub [8,5,8,5,8,5,8,56,6,6]~=? [8,5,56,6]
    , "Teste 94 - manter a primeira ocurrência" ~: nub [1,2,1,2,3,1,2] ~=? [1,2,3]
    , "Teste 95" ~: delete 2 [1,2,1,2,3,1,2] ~=? [1,1,2,3,1,2]
    , "Teste 96" ~: delete '0' "Programacao" ~=? "Programacao"
    , "Teste 97" ~: delete 'o' "Programacao" ~=? "Prgramacao"
    , "Teste 98" ~: delete 5 [1,2,3,4,6,8] ~=? [1,2,3,4,6,8]
    , "Teste 99" ~: delete 'c' "   " ~=? "   "
    , "Teste 100" ~: (\\) [1,2,3,4,5,1] [1,5] ~=? [2,3,4,1]
    , "Teste 101" ~: (\\) "programacao funcional" "acf" ~=? "progrmaao uncional"
    , "Teste 102" ~: (\\) "" "ola" ~=? ""
    , "Teste 103" ~: (\\) [True,True,False,False,True,False] [False,True] ~=? [True,False,True,False]
    , "Teste 104" ~: union [1,1,2,3,4] [1,5] ~=? [1,1,2,3,4,5]
    , "Teste 105" ~: union "programacao" "funcional " ~=? "programacaofunil "
    , "Teste 106" ~: union [] ["ola"] ~=? ["ola"]
    , "Teste 107" ~: union ["comunidade", "academica"] ["academica","futebol","clube"] ~=? ["comunidade","academica","futebol","clube"]
    , "Teste 108" ~: union [5,5,5,5,5,5,6,5,5,5,5] [6] ~=? [5,5,5,5,5,5,6,5,5,5,5]
    , "Teste 109" ~: intersect [1,1,2,3,4] [1,3,5] ~=? [1,1,3]
    , "Teste 110" ~: intersect "programacao " "funcional " ~=? "oaacao "
    , "Teste 111" ~: intersect [] ["ola"] ~=? []
    , "Teste 112" ~: intersect ["comunidade", "academica"] ["academica","futebol","clube"] ~=? ["academica"]
    , "Teste 113" ~: intersect [5,5,5,5,5,5,6,5,5,5,5] [6,5] ~=? [5,5,5,5,5,5,6,5,5,5,5] 
    , "Teste 114" ~: insert 25 [1,20,30,40] ~=? [1,20,25,30,40]
    , "Teste 115" ~: insert 'o' "alo"~=? "aloo"
    , "Teste 116" ~: insert 'c' "abefg" ~=? "abcefg"
    , "Teste 117" ~: insert 0 [] ~=? [0]
    , "Teste 118" ~: unwords ["Programacao", "Funcional"] ~=? "Programacao Funcional"
    , "Teste 119" ~: unwords ["Programacao"] ~=? "Programacao"
    , "Teste 120" ~: unlines ["Programacao", "Funcional"] ~=? "Programacao\nFuncional\n"
    , "Teste 121" ~: unlines ["Programacao"] ~=? "Programacao\n"
    , "Teste 122" ~: pMaior "bbbbzbb" ~=? 4
    , "Teste 123" ~: pMaior [20,5,8,6,3] ~=? 0
    , "Teste 124" ~: pMaior [False,False,False,True,False] ~=? 3
    , "Teste 125" ~: pMaior [1,2,5,99,1,2,3,54,1,2,3,666] ~=? 11
    , "Teste 126" ~: pMaior "computador" ~=? 4
    , "Teste 127" ~: lookup 'a' [('a',1),('b',4),('c',5)] ~=? Just 1
    , "Teste 128" ~: lookup 'z' [('a',1),('b',4),('c',5)] ~=? Nothing
    , "Teste 129" ~: lookup 'c' [('a',1),('b',4),('c',5)] ~=? Just 5
    , "Teste 130" ~: lookup "ola" [("assim",1),("ola",4),("c",5)] ~=? Just 4
    , "Teste 131" ~: preCrescente [3,7,9,6,10,22] ~=? [3,7,9]
    , "Teste 132" ~: preCrescente "computador" ~=? "co"
    , "Teste 133" ~: preCrescente "castanaha" ~=? "c"
    , "Teste 134" ~: preCrescente "abelha" ~=? "abel"
    , "Teste 135" ~: iSort [5,6,9,8,7,1] ~=? [1,5,6,7,8,9]
    , "Teste 136" ~: iSort "ola" ~=? "alo"
    , "Teste 137" ~: iSort "funcional" ~=? "acfilnnou"
    , "Teste 138" ~: iSort [5,5,5,5,5,1] ~=? [1,5,5,5,5,5]
    , "Teste 139" ~: menor "sai" "saiu" ~=? True
    , "Teste 140" ~: menor "sais" "saiu" ~=? True 
    , "Teste 141" ~: menor "sais" "sai" ~=? False
    , "Teste 142" ~: elemMSet 'a' [('b',2), ('a',4), ('c',1)] ~=? True
    , "Teste 143" ~: elemMSet "ola" [("assim",2), ("e",4), ("a vida",1)] ~=? False
    , "Teste 144" ~: elemMSet "a vida" [("assim",2), ("e",4), ("a vida",1)] ~=? True
    , "Teste 145" ~: elemMSet 1 [(4,2), (6,4), (5,1)] ~=? False
    , "Teste 146" ~: convertMSet [('b',2), ('a',4), ('c',1)] ~=? "bbaaaac"
    , "Teste 147" ~: convertMSet [("ola",2), ("a",4), ("azul",1)] ~=? ["ola","ola","a","a","a","a","azul"]
    , "Teste 148" ~: insereMSet 'c' [('b',2), ('a',4), ('c',1)] ~=? [('b',2),('a',4), ('c',2)]
    , "Teste 149" ~: insereMSet 'd' [('b',2), ('a',4), ('c',1)] ~=? [('b',2),('a',4), ('c',1), ('d',1)]
    , "Teste 150" ~: insereMSet 'a' [('b',2), ('a',4), ('c',1)] ~=? [('b',2),('a',5), ('c',1)]
  {-, "Teste 151"
    , "Teste 152"
    , "Teste 153"
    , "Teste 154"
    , "Teste 155"
    , "Teste 156"
    , "Teste 157"
    , "Teste 158"
    , "Teste 159"
    , "Teste 160"
    , "Teste 161"
    , "Teste 162"
    , "Teste 163"
    , "Teste 164"
    , "Teste 165"
    , "Teste 166"
    , "Teste 167"
    , "Teste 168"
    , "Teste 169"
    , "Teste 170"
    , "Teste 171"
    , "Teste 172"
    , "Teste 173"
    , "Teste 174"
    , "Teste 175"
    , "Teste 176"
    , "Teste 177"
    , "Teste 178"
    , "Teste 179"
    , "Teste 180"
    , "Teste 181"
    , "Teste 182"
    , "Teste 183"
    , "Teste 184"
    , "Teste 185"
    , "Teste 186"
    , "Teste 187"
    , "Teste 188"
    , "Teste 189"
    , "Teste 190"
    , "Teste 191"
    , "Teste 192"
    , "Teste 193"
    , "Teste 194"
    , "Teste 195"
    , "Teste 196"
    , "Teste 197"
    , "Teste 198"
    , "Teste 199"-}
    ]