{-
Crie uma função em Haskell que recebe um inteiro n e um caractere c. Caso o caractere c seja igual a 'p',
 sua função deve retornar a soma de todos os número fibonacci pares até n,
 caso o caractere c seja igual a 'i',sua função deve retornar a soma de todos os número fibonacci ímpares até n, caso contrário, retorna zero.
-}



--fibonacci
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib (n-2)

--fibonacci par
soma_fib :: Int -> Int
soma_fib 0 = 0
soma_fib n | fib n `mod` 2 == 0 = ( fib n ) + soma_fib ( n -1 )
		   | otherwise = 0 + soma_fib ( n -1 )


--fibonacci impar
soma_fib_i :: Int -> Int
soma_fib_i 0 = 0
soma_fib_i n | fib n `mod` 2 == 1 = ( fib n ) + soma_fib_i ( n -1 )
		   | otherwise = 0 + soma_fib_i ( n -1 )


--funcao main
fib_impar_par :: Char -> Int -> Int
fib_impar_par c n | c == 'i' = soma_fib_i n
				  | c == 'p' = soma_fib n
		          | otherwise  = 0
