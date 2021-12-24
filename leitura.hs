

escrita :: String -> IO()
escrita x = do putStrLn (reverse x)

leituraEscrita = do 
					x <- getLine
					putStrLn x
					

interacao = do
				putStrLn "coloca teu nome ai porra"
				nome <-  getLine
				putStrLn ("vai se fude " ++ nome)
				
				
				
manipulaArquivo = do 
					writeFile "teste/teste.txt" "teste\n"
					appendFile "teste.txt" "append_teste"
					readFile "teste.txt"
					

