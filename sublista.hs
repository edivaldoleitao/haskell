--Uma lista é uma sublista de outra se os elementos da primeira ocorrem na segunda seguindo a mesma ordem.
-- Por exemplo, "ship" é uma sublista de "fish and chips", mas não de "hippies". Já uma lista é uma subsequência
-- de outra se a primeira ocorre como uma sequência de elementos dentro da outra. Por exemplo,
-- "chip" é uma subsequência de "fish and chips", mas não de "chin up". Defina duas funções, uma para verificar se
-- uma string é sublista de outra (sublista :: String -> String -> Bool), e a segunda para verificar se uma 
--string é uma subsequência de outra (subsequencia :: String -> String -> Bool). 

import Data.List

sublista :: String -> String -> Bool
sublista [] n  = True
sublista m [] = False
sublista m n =
   if head m == head n 
       then sublista (tail m) (tail n)
   else  sublista m (tail n)


sbq = filter(not.null).concatMap inits.tails

subsequencia :: String -> String -> Bool
subsequencia m n = 
   if (length n) > (length m) 
       then elem m (sbq n)
   else  elem n (sbq m)

