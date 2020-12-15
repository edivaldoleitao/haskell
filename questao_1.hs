--Suponha que precisamos calcular 2 elevado a um número n. Se n é par, por exemplo 2*m, então: 2^n = 2^(2*m) = (2^m)^2

--Se n é ímpar , por exemplo 2*m+1, então: 2^n = 2^(2*m+1) = ((2^m)^2)*2

--Desta forma, crie uma função recursiva "eleva2:: Int -> Int" que computa 2 elevado a um número n usando estas ideias. 




eleva2:: Int -> Int
eleva2 0 = 1
eleva2 1 = 2
eleva2 n | n `mod` 2 == 0 = (eleva2 (n `div` 2))^2
		 | n `mod` 2 == 1 = ((eleva2 (n `div` 2))^2)*2