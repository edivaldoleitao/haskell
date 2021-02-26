--Uma empresa deseja fazer uma análise sobre posts de personalidades no Twitter para saber os temas mais abordados.
-- Assim, crie uma função em Haskell que recebe uma lista de tweets (lista de Strings), e devolve uma lista de tuplas,
 --nas quais o primeiro elemento é a palavra e o segundo a quantidade de vezes que esta palavra se repete em todos os tweets.
 --Faça um tratamento nos tweets para remover palavras irrelevantes (artigos, preposições essenciais, conjunções essenciais)
-- e assuma que dentro de cada String as palavras estão separadas por espaço em branco.
-- Na sua resposta cada palavra deve aparecer em somente uma tupla.
import Data.Char 
import Data.List



-- função principal que conta as palavras na lista de tweets que recebe como argumento.
tweetsWords :: [String] -> [(String,Int)]
tweetsWords [] = []
					-- as palavras são separadas pela função slice, depois são convertidas para caixa alta para fazer comparação
					-- com a lista de preposições, e depois são removidas as preposições , artigos  e conjuções , então por fim 
					-- a lista resultante é usada como argumento para função bag.
tweetsWords texts = bag ( textTreat	(capitalize (sliceText (texts)))) 



-- FUNÇÕES AUXILIARES



-- deixa todos os caracters em maiúsculo para serem tratados.
capitalize :: [String] -> [String]
capitalize [] = []
capitalize string = [map (toUpper) $ head string] ++ (capitalize $ tail string)



sliceText :: [String] -> [String] -- fatia as palavras de todas as strings, considerando como separador o espaço em branco.
sliceText [] = []
sliceText text = (words $ head text) ++ (sliceText $ tail text) -- chamada recursiva para fatiar .



textTreat :: [String] -> [String] -- retira as preposições e etc e colocar na bag.
							-- lista de preposições, conjunções essenciais e artigos, (o texto é convertido em caixa alta)
							-- para facilitar a comparação usando capitalize.
textTreat text =  text \\ ["COM", ",", ".","O","A","E","OU","ANTE","APÓS","ATÉ","CONTRA","DE",
							"DESDE","EM","ENTRE","PARA","PER","PERANTE","POR","SEM","SOB","SOBRE","TRÁS","NEM","MAS",
							"PORÉM","TODAVIA","CONTUDO","ENTRETANTO","POIS","PORQUE","PORTANTO","SE","ORA","APESAR","COMO" ] 
								
 
-- conta a quantidade  de elementos.
bag ::(Eq a) => [a] -> [(a,Int)] 
bag [] = [] -- retorna vazio.
bag (x:xs) = [(x,amount (x,xs))] ++ bag (filter (/= x) xs) -- percorre a lista de forma recursiva e usa filter para retirar os valores repetidos.
			where amount (y,ys) = length (filter(== y) (y:ys))