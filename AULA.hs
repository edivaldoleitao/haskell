square :: Int -> Int
square x = x*x 

allequal :: Int -> Int -> Int ->Bool
allequal n m p = (n == m) && (m == p)

maxi :: Int -> Int -> Int
maxi n m | n >= m = n
         | otherwise = m 
		 


vendas 0 = 1
vendas 1 = 2
vendas 2 = 3
vendas 3 = 4

totalvendas :: Int -> Int 
totalvendas n
 | n == 0 = vendas 0
 | otherwise = totalvendas (n - 1) + vendas n 
 
maxvendas :: Int -> Int 
maxvendas 0 = vendas 0
maxvendas n = maxi (maxvendas (n - 1)) (vendas n) 

fib :: Int ->Int 
fib 0 = 0
fib 1 = 1
fib n = (fib (n - 1)) + (fib (n - 2)) 

fat :: Int ->Int
fat 0 = 1
fat n = (fat (n - 1)) * n 

all4equal :: Int -> Int -> Int -> Int -> Bool
all4equal m n o p = (m == n) && ( n == o) && (o == p) 

equalcount :: Int -> Int -> Int -> Int 
equalcount m n o | (allequal m n o == True) = 3
                 | m == n = 2
				 | n == o = 2
				 | m == o = 2
				 | otherwise = 0

