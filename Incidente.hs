{-
Batman precisa obter as áreas de Gotham City com maior índice de criminalidade e pediu para Robin criar um programa em Haskell para lhe ajudar.
 Robin tem acesso a todos os incidentes da cidade e para tanto pensou em um tipo de dados algébrico INCIDENTE.
 Este tipo pode ser um Roubo, Homicídio ou Agressão. O primeiro tem a informação do valor roubado (ponto flutuante),
 e os últimos dois possuem a quantidade de pessoas envolvidas no incidente. Cada Incidente tem um índice de gravidade calculado de acordo com seu parâmetro:

a. Roubo até $1000,50 – 5

b. Roubo acima de $1000,50 – 10

c. Homicidio = quantidade*20

d. Agressao = quantidade*5

Existem outros tipos de incidentes, mas eles não devem ser considerados na conta.
 Desta forma, crie um programa em Haskell que lê um arquivo contendo os incidentes de um bairro e deve retornar seu índice de criminalidade.
 OBS: A função readFile::String -> IO String lê um arquivo retorna o seu conteúdo.
-}

data INCIDENTE = Roubo Float | Agressao Int | Homicidio Int

--quebra os termos da string
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

--cria os tipos algebricos a partir da string
criaIncidente :: String -> INCIDENTE
criaIncidente texto | head texto2 == "Roubo" = (Roubo valor)
                    | head texto2 == "Homicidio" = (Homicidio valorInt)
					| head texto2 == "Agressao" = (Agressao valorInt)
	where texto2 = (wordsWhen(==' ') texto)
	      valor = read (last (wordsWhen(==' ') texto)) :: Float
	      valorInt = read (last (wordsWhen(==' ') texto)) :: Int
		  
--cria lista de incidentes
criaLista :: String -> [INCIDENTE]
criaLista texto =  map (criaIncidente)(wordsWhen (==',') texto)

--atribui pontos para o indice
ocorrencia :: INCIDENTE -> Float
ocorrencia (Roubo x) | x <= 1000.50 = 5
					 | x > 1000.50 = 10
ocorrencia (Agressao y) = b * 5
  where b = fromIntegral y :: Float
ocorrencia (Homicidio y) = b * 20
  where b = fromIntegral y :: Float


--soma todos os valores de incidente ocorridos
indiceIncidentes :: [INCIDENTE] -> Float
indiceIncidentes [] = 0.00
indiceIncidentes (x:xs) = (ocorrencia x) + (indiceIncidentes xs)

--função main
indiceBairro = do
				 putStrLn "insira o caminho do arquivo:"
				 caminho <- getLine
				 arquivo <- readFile caminho
				 putStrLn (show ("O Indice de criminalidade eh: " ++ (show (indiceIncidentes(criaLista arquivo)))))
				 

--  no Arquivo o formato dos incidentes estão sem parenteses e separados por vígulas
-- Exemplo:
-- Roubo 2, Homicidio, Roubo 2