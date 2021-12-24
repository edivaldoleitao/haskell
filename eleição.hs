
{-
	No sistema eleitoral americano quando um candidato a presidência ganha a votação por maioria de um determinado
estado, os votos dos delegados daquele estado são contabilizados para aquele candidato. Ganha a eleição quem
possuir mais votos de delegados, o que não necessariamente representa a maioria dos votos dos eleitores no
país. Crie uma função em Haskell para indicar o vencedor de uma eleição americana. Vamos simplificar
utilizando apenas dez estados, os quais são listados abaixo de acordo com seus números de delegados:
	    
1 - California = 55

2 - Texas = 38

3 - Florida = 29

4 - Nova York = 29

5 - Illinois = 20

6 - Pensilvânia = 20

7 - Ohio = 18

8 - Georgia = 16

9 - Michigan = 16

10 - Carolina do Norte = 15

Assumindo uma eleição com dois candidatos, A e B, crie uma função em Haskell que recebe uma tupla de 10 elementos
 indicando  em cada posição o candidato que venceu em cada estado na ordem listada acima. Por exemplo, se o candidato A
  aparece na posição 1 então ele venceu no estado da Califórnia, já se ele aparece na posição 8, ele ganhou o estado da
   Georgia. Desta forma, calcule a quantidade de votos de delegados que cada candidato terá no colégio eleitoral e
    RETORNE O VENCEDOR DA ELEIÇÃO, OU SEJA, AQUELE QUE CONSEGUIU O MAIOR NÚMERO DE VOTOS DE DELEGADOS.    
-}


-- O código funiona da seguinte forma, o programa soma os delegados de maneira positiva para aparições de 'A' na entrada e soma negativamente delegados na aparição de 'B' na entrada.
-- Se o resultado der positivo o candidato A ganha e se der negativo o candidato B ganha.

{- exemplos de entradas:
contarVotos ('A','B','A','B','A','A','A','B','B','B')
contarVotos ('B','A','B','A','B','B','B','A','A','B')
contarVotos ('A','B','A','B','A','A','A','B','A','B')
contarVotos ('B','B','A','B','A','B','A','B','A','B')
contarVotos ('B','B','A','B','A','A','A','B','B','A')
contarVotos ('B','B','A','B','A','B','A','B','B','B')
-}

type Entrada = (Char,Char,Char,Char,Char,Char,Char,Char,Char,Char)

contarVotos :: Entrada -> (String)
contarVotos x | (pri x) + (sec x) + (tec x) + (qua x) + (qui x) + (sex x) + (set x) + (oit x) + (nov x) + (dez x) > 0 = "O candidato A ganhou as eleicoes." | (pri x) + (sec x) + (tec x) + (qua x) + (qui x) + (sex x) + (set x) + (oit x) + (nov x) + (dez x) < 0 = "O candidato B ganhou as eleicoes." | otherwise = "Deu empate"
	
	 
pri :: Entrada -> Int
pri (a,b,c,d,e,f,g,h,i,j)  | a == 'A' = 55 
                           | a == 'B' = -55
 
sec :: Entrada -> Int
sec (a,b,c,d,e,f,g,h,i,j)  | b == 'A' = 38 
                           | b == 'B' = -38

tec :: Entrada -> Int
tec (a,b,c,d,e,f,g,h,i,j) | c == 'A' = 29 
                          | c == 'B' = -29

qua (a,b,c,d,e,f,g,h,i,j) | d == 'A' = 29 
                          | d == 'B'  = -29                          
                          
qui (a,b,c,d,e,f,g,h,i,j) | e == 'A' = 20 
                          | e == 'B' = -20

sex (a,b,c,d,e,f,g,h,i,j) | f == 'A' = 20 
                          | f == 'B' = -20

set (a,b,c,d,e,f,g,h,i,j) | g == 'A' = 18 
                          | g == 'B' = -18

oit (a,b,c,d,e,f,g,h,i,j) | h == 'A' = 16 
                          | h == 'B' = -16

nov (a,b,c,d,e,f,g,h,i,j) | i == 'A' = 16 
                          | i == 'B' = -16

dez (a,b,c,d,e,f,g,h,i,j) | j == 'A' = 15 
                          | j == 'B' = -15
