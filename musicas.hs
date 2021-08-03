{-
Um aplicativo de música deseja computar as músicas e artistas mais ouvidos ao longo do ano dos seus usuários. Assumindo que as informações de registro de músicas ouvidas estão estruturadas em uma lista de tuplas, as quais tem a seguinte estrutura (String, Int, Timestamp), onde o primeiro elemento é o nome da música, o segundo a duração que escutou a música (em segundos) e o terceiro a data/hora que esta música foi escutada, crie as seguintes funções em Haskell:

a. Uma função que dada a lista de registros de músicas ouvidas retorna a música mais ouvida ao longo do ano de 2020, ou seja, mais repetições da mesma música.

b. Uma função que dada a lista de registros de músicas ouvidas retorna a música que o usuário passou mais tempo escutando, ou seja, música cuja soma das durações ao longo do ano de 2020 é a maior.
-}

import Data.Time

data Registro = Registro {nome :: String, -- nome da música
                          duracao :: Int, -- duração da música, em segundos
                          dia :: Maybe Day -- dia que a mesma foi tocada
                         } deriving (Show)

retornarMusicas :: [Registro] -> [String]
retornarMusicas [] = []
retornarMusicas (l:ls) = let ret = (retornarMusicas ls)
                         in if (elem (nome l) ret)
                               then [] ++ ret
                               else [(nome l)] ++ ret

-- letra A
musMaisOuvida :: [Registro] -> String
musMaisOuvida r = last (ordenarMusicas r (retornarMusicas r))
				   
ordenarMusicas :: [Registro] -> [String] -> [String]
ordenarMusicas _ [] = []
ordenarMusicas r (nm:nms) = let menor = (ordenarMusicas r [x | x <- nms, ((contMusica r x) <= (contMusica r nm))])
                                maior = (ordenarMusicas r [x | x <- nms, ((contMusica r x) > (contMusica r nm))])
                            in menor ++ [nm] ++ maior

contMusica :: [Registro] -> String -> Int
contMusica [] _ = 0
contMusica (r:rs) m | (((nome r) == m)) = (1 + (contMusica rs m))
                    | otherwise = (0 + (contMusica rs m))
					
-- letra B
musMaiorDuracao :: [Registro] -> String
musMaiorDuracao r = last (ordenarMusicasII r (retornarMusicas r))

ordenarMusicasII :: [Registro] -> [String] -> [String]
ordenarMusicasII _ [] = []
ordenarMusicasII r (nm:nms) = let menor = (ordenarMusicasII r [x | x <- nms, ((contDuracao r x) <= (contDuracao r nm))])
                                  maior = (ordenarMusicasII r [x | x <- nms, ((contDuracao r x) > (contDuracao r nm))])
                              in menor ++ [nm] ++ maior

contDuracao :: [Registro] -> String -> Int
contDuracao [] _ = 0
contDuracao (r:rs) m | ((nome r) == m) = ((duracao r) + (contDuracao rs m))
                     | otherwise = (0 + (contDuracao rs m))

{-

[(Registro "There! Right There!" 205 (fromGregorianValid 2020 10 05)), (Registro "Faroeste Caboclo" 543 (fromGregorianValid 2020 01 01)), (Registro "There! Right There!" 205 (fromGregorianValid 2020 05 10)), (Registro "There! Right There!" 205 (fromGregorianValid 2021 05 20))]

[(Registro "There! Right There!" 205 (fromGregorianValid 2020 10 05)), (Registro "Faroeste Caboclo" 543 (fromGregorianValid 2020 01 01)), (Registro "Faroeste Caboclo" 543 (fromGregorianValid 2020 01 01))]

-}