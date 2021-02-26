--Utilizando a linguagem funcional Haskell, defina uma função bag que recebe uma lista de elementos e retorna uma lista de pares, 
--onde o primeiro elemento de cada par é um elemento da lista original e o segundo é o número de ocorrências deste elemento.
-- Nesta segunda lista, cada elemento só ocorre uma vez. Por exemplo, bag [a,b,a,c,a,b] = [(a,3),(b,2),(c,1)].

retorno2 :: [Int] -> [Int]
retorno2 (x:xs) = map (\ x -> x*2 ) (x:xs)

-- tem uma lista como entrada e pode ter qualquer tipo como seus elementos
-- (Eq a) é necessário usar esta classe de comparação para tipos genéricos
bag ::(Eq a) => [a] -> [(a,Int)]
bag [] = [] -- retorna vazio 
bag (x:xs) = [(x,amount (x,xs))] ++ bag (filter (/= x) xs) -- percorre a lista de forma recursiva e usa filter para retirar os valores repetidos
			where amount (y,ys) = length (filter(== y) (y:ys)) -- amount recebe o valor da cabeça pra comparar usando 
															   -- filter para criar uma lista de ocorrências e retorna seu tamanho
			      