{-
Filamentos de DNA e RNA são compostos por seus nucleotoides. 

Os quatro nucleotoides do DNA são: adenina (A), citosina (C), guanina (G) e timina (T).

Os quatro nucleotoides do RNA são: adenina (A), citosina (C), guanina (G) e uracil (U).

Um filamento de DNA pode ser transcrito para um de RNA substituindo cada nucleotoide pelo seu complemento:

G -> C
C -> G
T -> A
A -> U

Assim, crie um programa em haskell que lê um arquivo "dna.txt" cujas linhas correspondem a filamentos de DNA.
Em seguida, crie um novo arquivo "rna.txt" com as transcrições para RNA dos filamentos fornecidos no arquivo de entrada. Abaixo segue um exemplo.
Dado um arquivo "dna.txt" com o seguinte conteúdo:

ACGTGGTCTTAA

CGTA

Seu programa deve gerar um arquivo "rna.txt" com o seguinte conteúdo:
UGCACCAGAAUU

GCAU

Linhas em branco devem gerar linhas em branco no arquivo de saída. Caso existam caracteres inválidos nas linhas do arquivo de entrada,
 a linha correspondente no arquivo de saída deve informar uma mensagem de erro "Filamento de DNA inválido!". 
-}


import Data.Char
import System.IO



main :: IO()
main = do texto <- readFile "dna.txt"
          let texto2 = rna (show texto)
          writeFile "rna.txt" texto2
          print texto2
          



rna :: String -> String
rna texto = map repl texto



ehValido :: String -> String
ehValido texto | '1' `elem` texto = "Filamento de DNA inválido!"
               | otherwise =  ""

repl :: Char -> Char
repl x | x == 'G' = 'C'     
       | x == 'C' = 'G'
       | x == 'T' = 'A'
       | x == 'A' = 'U'
       | x == '\n' = '\n'
	   | x == '\"' = ' '
       | otherwise = ' '
                  
           







