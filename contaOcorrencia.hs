{-
Utilizando a linguagem funcional Haskell, defina uma função que recebe uma lista de listas de elementos de um tipo t (genérico) e retorna uma lista de tuplas-2 onde o primeiro elemento é um valor do tipo t que existe em pelo menos uma das sub-listas da entrada e o segundo é o número de ocorrências desse valor nas sub-listas.

Exemplos: contaOcorr ["haskell","eh","legal"] -> [('h',2),('a',2),('s',1),('k',1),('e',3),('l',4),('g',1)]

                  contaOcorr [[2,45,6,2,1],[7,7,4,3,2]] -> [(2,3),(45,1),(6,1),(1,1),(7,2),(4,1),(3,1)]
-}

-- Função auxiliar

bag ::(Eq a) => [a] -> [(a,Int)]
bag [] = [] -- retorna vazio 
bag (x:xs) = [(x,amount (x,xs))] ++ bag (filter (/= x) xs) -- percorre a lista de forma recursiva e usa filter para retirar os valores repetidos
			where amount (y,ys) = length (filter(== y) (y:ys)) -- amount recebe o valor da cabeça pra comparar usando 
			
-- função auxiliar para unificar as listas e usar como parametro da função bag

unifica :: [[a]] -> [a]
unifica [] = []
unifica lista = head lista ++ unifica(tail lista)

--função principal

contaOcorr ::(Eq a) => [[a]] -> [(a,Int)]
contaOcorr [] = []
contaOcorr lista = bag(unifica(lista))
