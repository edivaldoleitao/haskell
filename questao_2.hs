--Um país deseja criar um protocolo de vacinação que informa a quantidade de meses para uma pessoa tomar a vacina de acordo com 3 fatores:

-- idade.

-- presença de comorbidades

-- se é profissional de saúde.

--As regras são:

--a) caso seja profissional de saúde, o seu mês será 1. 

--b) pessoas acima de 70 anos também são vacinadas no mês 1. 

--c) abaixo de 70 anos, a cada cinco anos aumenta-se a quantidade de meses. Assim, pessoas entre 65 e 70 (incompletos) são o mês 2, entre 60 e 65 são no mês 3, e assim por diante. 

--d) caso a pessoa tenha comorbidade, seu mês de referência é reduzido em uma posição, com o mínimo sendo o mês 1.

--Assim, crie uma função em Haskell que recebe uma tupla de três elementos (Int, Bool, Bool) representando uma pessoa, onde o primeiro elemento é a idade, o segundo é um booleano informando se a pessoa tem comorbidade e o terceiro é um booleano informando se a pessoa é profissional de saúde, e deve retornar a quantidade de meses que a pessoa deverá esperar para ser vacinada.


tempoVacina:: (Int, Bool, Bool) -> Int
tempoVacina (x, y, z) | z == True || x >= 70 = 1
					  |	(x < 70) && (y == True) = ((tempo x) - 1)
					  | otherwise = tempo x
 where tempo x
	| (65 <= x) && (x < 70) = 2
	| (60 <= x) && (x < 65) = 3
	| (55 <= x) && (x < 60) = 4
	| (50 <= x) && (x < 55) = 5
	| (45 <= x) && (x < 50) = 6
	| (40 <= x) && (x < 45) = 7
	| (35 <= x) && (x < 40) = 8
	| (30 <= x) && (x < 35) = 9
	| (25 <= x) && (x < 30) = 10
	| (20 <= x) && (x < 25) = 11
	| (15 <= x) && (x < 20) = 12	  
	| (10 <= x) && (x < 15) = 13
	| (5 <= x) && (x < 10) = 14
	| (0 <= x) && (x < 5) = 15
	