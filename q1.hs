--Dada a definição de árvore binária como um tipo algébrico como vimos na nossa aula,
-- crie uma função colapsar:: Int -> ArvoreBin t -> [t]  que transforma uma árvore binária em
-- uma lista de acordo com o inteiro passado como o primeiro parâmetro.
-- Caso o valor seja 0 devemos considerar a navegação Em Ordem,
-- caso seja 1 devemos considerar Pré-Ordem e caso seja 2 retornar a lista em Pós-Ordem. Por exemplo, dada a árvore abaixo:


--	   (2)
--     / \
--	 (7)  (5)
--   / \    \
-- (2)  (6) (9)
--	   /  \   \
--   (5) (11) (4)

		 
-- (Em ordem) 0 -> [2, 7, 5, 6, 11, 2, 5, 4, 9] 

-- (Pré-ordem) 1 -> [2, 7, 2, 6, 5, 11, 5, 9, 4]

-- (Pós-ordem) 2 -> [2, 5, 11, 6, 7, 4, 9, 5, 2]

data ArvoreBin t = No t (ArvoreBin t) (ArvoreBin t) | NilArv deriving (Eq,Show,Ord) 

colapsar :: Int -> ArvoreBin t -> [t]
colapsar _ NilArv = []
colapsar 0 (No t e d) = (colapsar 0 (e)) ++ [t] ++ (colapsar 0 (d))
colapsar 1 (No t e d) = [t] ++ (colapsar 1 (e)) ++ (colapsar 1 (d)) 
colapsar 2 (No t e d) = (colapsar 2 (e)) ++ (colapsar 2 (d)) ++ [t]


-- entrada que representa a árvore da questão e foi usada para testar
--No 2 (No 7 (No 2 (NilArv) (NilArv)) (No 6 (No 5 (NilArv) (NilArv)) (No 11 (NilArv) (NilArv)))) (No 5 (NilArv) (No 9 (NilArv) (No 4 (NilArv) (NilArv))))








