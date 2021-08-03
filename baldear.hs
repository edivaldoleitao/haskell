{-
Crie uma função em Haskell baldear::[Int]->Int->[[Int]] que recebe uma lista de inteiros e um inteiro n e ela deve retornar uma lista com n listas de forma que
 os elementos nas sublistas são inseridos de forma consecutiva varrendo os elementos da lista principal. Por exemplo: 

baldear [4,23,2,5,6,8,12,57] 3 = [[4,5,12],[23,6,57],[2,8]] 
-}

baldear :: [Int] -> Int -> [[Int]]
baldear l n = functionII l n 0

functionI :: [Int] -> Int -> Int -> [Int]
functionI [] _ _ = []
functionI l n y | ((mod (length l) n) == y) = (functionI (reverse (tail (reverse l))) n y) ++ [last l]
                | otherwise = (functionI (reverse (tail (reverse l))) n y) ++ []
				
functionII :: [Int] -> Int -> Int -> [[Int]]
functionII l n m | (m == n) = []
                 | (m == 0) = (functionII l n (m+1)) ++ [(functionI l n m)]
                 | otherwise = [(functionI l n m)] ++ (functionII l n (m+1))