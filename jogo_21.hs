{-
 No jogo de cartas 21, os usuários vão pedindo cartas até que decidam parar.
 Caso a pontuação da mão de um jogador ultrapasse 21 ele perde. Caso contrário, ganha o jogador que mais se aproximou de 21.
 Neste jogo as pontuações de cartas seguem a seguinte regra, cartas com números tem a pontuação do respectivo número, cartas de rosto (valete, dama e rei)
 valem 10 cada, e o ás pode valer 1 ou 11 de acordo com o que for mais vantajoso para o usuário. Assim, crie um programa em Haskell com um tipo algébrico 
 Carta para representar as cartas de um baralho com seus naipes (ouro, espada, paus e copas) e representações (Ás, 2, 3, 4, 5, 6, 7, 8, 9, 10, valete, dama e reis).
 Em seguida, crie uma função que dada uma lista de cartas (mão do usuário), retorna a pontuação da mão de acordo com as regras do jogo 21 já apresentadas.
 -}
 
 
 
 
-- estrutura da carta com naipe e valor
data Carta = Ouro String | Espada String | Paus String | Copas String


--Função principal que mostra o valor da mão do jogador
pontuacaoMao :: [Carta] -> Int
pontuacaoMao [] = 0
pontuacaoMao cartas | (qtdAs cartas) == 0 = ( somaCartas cartas )
pontuacaoMao cartas | (qtdAs cartas) >= 2 && (somaCartas cartas) + 10 <= 21 = ((somaCartas cartas) + 10)
pontuacaoMao cartas | (qtdAs cartas) >= 2 && (somaCartas cartas) + 20 <= 21 = ((somaCartas cartas) + 20)
pontuacaoMao cartas | (qtdAs cartas) == 1 && (somaCartas cartas) + 10 <= 21 = ((somaCartas cartas) + 10)
pontuacaoMao cartas | otherwise = somaCartas cartas



-- retorna o valor  da carta na lista, tendo para ÁS  os valor padrão de 1
retornaValor :: Carta -> Int
retornaValor (Ouro valor) | valor == "valete" || valor == "dama" || valor == "rei" = 10
						  | valor == "As" = 1
				          | otherwise = read valor :: Int
retornaValor (Paus valor) | valor == "valete" || valor == "dama" || valor == "rei" = 10
						  | valor == "As" = 1
				          | otherwise = read valor :: Int
retornaValor (Copas valor)| valor == "valete" || valor == "dama" || valor == "rei" = 10
						  | valor == "As" = 1
				          | otherwise = read valor :: Int
retornaValor(Espada valor)| valor == "valete" || valor == "dama" || valor == "rei" = 10
						  | valor == "As" = 1
				          | otherwise = read valor :: Int




-- mostra a quantidade de cartas com Às na lista		  
qtdAs :: [Carta] -> Int
qtdAs cartas = length(filter(\x -> (retornaValor x) == 1) cartas)




-- soma todas as cartas na lista						  
somaCartas :: [Carta] -> Int
somaCartas [] = 0
somaCartas (x:xs) = retornaValor x + (somaCartas xs)