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



sumSquares :: Int -> Int -> Int
sumSquares x y  = sqX + sqY
  where sqX = x * x 
        sqY = y * y 


sumSquares x y = sq x + sq y 
    where sq z = z * z


sumSquares x y = let sqX = x * x 
                     sqY = y * y 
			           in sqX + sqY 


vendasemana 0 = 10
vendasemana 1 = 30 
vendasemana 2 = 10 
vendasemana 3 = 50 
vendasemana 4 = 7
vendasemana 5 = 1

soma :: Int -> (Int -> Int)
soma x y = x + y
incrementa :: Int -> Int
incrementa = soma 1


vendasPorSemana :: Int -> Int -> Int
vendasPorSemana s n | n > 0 && s == vendasemana n = incrementa (vendasPorSemana s (n - 1)) 
                    | s == vendasemana 0 = incrementa (vendasPorSemana s 0)
                    | s /= vendasemana 0 = 0 
-- stack overflow problem.
                  










