{-
Faça funções em Haskell que recebem uma lista de tuplas (String,Int), onde o primeiro elemento é o nome de um país e o segundo é um inteiro
correspondendo a um ano em que este país ganhou um título na Copa América. Abaixo seguem os comportamentos de cada função:

a.      Uma função que retorne o primeiro país a ganhar um título;

b.     	Uma função que retorne o país que mais ganhou títulos;

c.      Uma função que além, do parâmetro anterior, recebe uma lista com os nomes dos países que já participaram da Copa América e deve retornar
		uma lista com os nomes dos países que nunca ganharam títulos;

OBS: lembre-se que você pode criar funções auxiliares para resolver o problema de cada função.
-}
import Data.List (sortBy)
import Data.Function (on)
--A.------------------------------------------------------------------------------------------
-- retorna o primeiro a receber o título

primeiroVenc :: [(String, Int)] -> (String, Int)
primeiroVenc [] = ("",0)
primeiroVenc lista = head (sortBy (compare `on` snd) lista)

--B.------------------------------------------------------------------------------------------
-- retorna quem tem mais títulos

maisTitulos :: [(String, Int)] -> (String)
maisTitulos [] = ("")
maisTitulos lista =  fst (last (sortBy (compare `on` snd) (bag(listaVenc (lista)))))

-- Função auxiliar de maisTitulos e nuncaVenc

listaVenc :: [(String, Int)] -> [String] -- retorna uma lista dos vencedores
listaVenc [] = []
listaVenc lista = [(first (head lista))] ++ (listaVenc (tail lista))
				  where first (a,_) = a

-- Funcão auxiliar de maisTitulos, é usada para contar quantas vezes o time foi vencedor
bag ::(Eq a) => [a] -> [(a,Int)] 
bag [] = [] -- retorna vazio 
bag (x:xs) = [(x,amount (x,xs))] ++ bag (filter (/= x) xs) -- percorre a lista de forma recursiva e usa filter para retirar os valores repetidos
			where amount (y,ys) = length (filter(== y) (y:ys))
			
--C.------------------------------------------------------------------------------------------


--Função que retorna paises que nunca ganharam a copa améria
nuncaVenc :: [(String,Int)] -> [String] -> [String]
nuncaVenc [] [] = []
nuncaVenc vencedores times =  [x | x <- times, (not (x `elem` (listaVencedores vencedores))) && x `elem` times]
					where listaVencedores vencedores = listaVenc vencedores

			
