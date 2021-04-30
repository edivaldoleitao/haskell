--Crie uma função "organizaNomes" em Haskell que recebe uma lista de strings com nomes de pessoas  
--(pode ser só o primeiro nome) e retorna uma lista de tuplas onde o primeiro elemento da tupla é um caractere e o 
--segundo é uma lista com os nomes da lista inicial que começam com este caractere. Por exemplo:

-- -> organizaNomes ["jose", "lucas", "joao", "carlos", "antonio", "alfredo", "zoe"]

-- -> [('a', ["alfredo", "antonio"]), ('c', ["carlos"]), ('j', ["joao", "jose"]), ('l', ["lucas"]), ('z', ["zoe"])]



organizaNomes :: [String] -> [(Char, [String])]
organizaNomes [] = []
organizaNomes (x:xs) = [(head x, [y | y <- (x:xs), (head y) == head x])] ++ (organizaNomes (filtra (head x) xs))



filtra :: Char -> [String] -> [String]
filtra _ [] = []
filtra c (x:xs) | (head x) /= c = [x] ++ (filtra c xs)
				| otherwise = xs		




