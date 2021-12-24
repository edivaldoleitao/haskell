--Utilizando a linguagem funcional Haskell, defina uma função que recebe uma lista de inteiros e deve retornar uma
-- lista de tuplas-2 onde o primeiro elemento é igual ao número da lista e o segundo é a soma dos dígitos deste número.
-- No entanto, a lista resultante deve conter apenas tuplas nas quais o primeiro elemento é um múltiplo do segundo. 
--Exemplos: digitosDeMultiplos [5,12,71,8,25,3,150] -> [(5,5),(12,3),(8,8),(3,3),(150,6)]
--digitosDeMultiplos [98,24,81,7773,21,1000] -> [(24,6),(81,9),(21,3),(1000,1)]

soma :: String -> Int
soma [] = 0
soma l =
    if head l == '-' then soma (tail l)
    else read [head l] + soma (tail l)

isMod :: (Int, Int) -> Bool
isMod (a,b) =
            if mod a b  == 0 then True
            else if a == 0 then False
            else False                  

digitosDeMultiplos :: [Int] -> [(Int, Int)]
digitosDeMultiplos x = filter isMod (map(\(l) -> ((l), soma(show (l)))) x)
