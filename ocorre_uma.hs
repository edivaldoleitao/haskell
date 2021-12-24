{-
Escreva a funcao ocorreUma :: [Int] -> [Int]  em Haskell,
 que deve retornar uma lista com os números inteiros que aparecem apenas uma vez na lista passada como parâmetro.
 Ex: ocorreUma [4,1,5,4,3,5]  deve retornar [1,3] .
-}

bag ::(Eq a) => [a] -> [(a,Int)]
bag [] = [] -- retorna vazio 
bag (x:xs) = [(x,amount (x,xs))] ++ bag (filter (/= x) xs) -- percorre a lista de forma recursiva e usa filter para retirar os valores repetidos
			where amount (y,ys) = length (filter(== y) (y:ys)) -- amount recebe o valor da cabeça pra comparar usando 
															   -- filter para criar uma lista de ocorrências e retorna seu tamanho
											
funcAux :: [Int] -> [(Int,Int)]
funcAux lista = filter (\(x,y) -> y == 1) (bag lista)										

funcAux2 :: [(Int,Int)] -> [Int]
funcAux2 [] = []
funcAux2 (x:xs) = [(fst x)] ++ funcAux2(xs)


ocorreUma :: [Int] -> [Int]
ocorreUma lista = funcAux2(funcAux lista) 