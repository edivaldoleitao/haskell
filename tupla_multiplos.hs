{-
Utilizando a linguagem funcional Haskell, defina uma função que recebe uma lista de inteiros e deve retornar uma lista de tuplas-2 onde o primeiro elemento é igual ao número da lista e o segundo é a soma dos dígitos deste número. No entanto, a lista resultante deve conter apenas tuplas nas quais o primeiro elemento é um múltiplo do segundo. 
Exemplos: digitosDeMultiplos [5,12,71,8,25,3,150] -> [(5,5),(12,3),(8,8),(3,3),(150,6)]
digitosDeMultiplos [98,24,81,7773,21,1000] -> [(24,6),(81,9),(21,3),(1000,1)]
-}


digits:: Int -> [Int]

digits n
 | (n<10) = [n]
 | (n<0) = []
 | (n>=10) = digits(div n 10) ++ [mod n 10]


digitosDeMultiplos :: [Int] -> [(Int,Int)]
digitosDeMultiplos [] = []

digitosDeMultiplos (l:ista) = criaTupla l ++ digitosDeMultiplos (ista)

criaTupla :: Int -> [(Int,Int)]
criaTupla n
 |((mod n (sum(digits n))) == 0) = [(n,sum(digits n))]
 |otherwise = []
