{-
Crie um programa com entrada e saída que primeiro lê um inteiro positivo n, e então lê n inteiros e 
escreve na saída padrão a soma de todos eles. O programa deve explicar ao usuário as entradas a serem 
fornecidas e imprimir a saída esperada. 
-}

iteracao1 = do putStrLn "Digite n: "
               n <- getLine
               return n

iteracao2 = do putStrLn "Digite o próximo numero: "
               numero <- getLine
               return numero

iteracao3 resultado = do putStrLn("O resultado da soma é: " ++ resultado)      

soma x y = x + y

loop 0 x = x
loop i x = soma(x loop((i-1) iteracao2)) 

-----------------------------------
n = iteracao1
iteracao3 loop(n 0)


executa = n