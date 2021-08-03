{--
Assumindo que os dados de vacina para o COVID-19 estejam em um arquivo "vacina.txt" e que as linhas representam dados das pessoas que participam do estudo e estão estruturadas da seguinte forma:

[id];[idade];[placebo];[reação]

onde [id] representa um inteiro identificando uma pessoa, [idade] informa a idade da pessoa, [placebo] caso seja "true" indica que a vacina tomada é um placebo, caso seja "false" indica que realmente foi a vacina sendo testada, e [reação] pode ser "nenhuma" caso não tenha tido reação, "leve" caso tenha tido algum sintoma leve, "forte" caso tenha sentido algum sintoma forte.

Crie um programa em Haskell deve ler este arquivo e imprimir na saída padrão as seguintes informações:

- Média de pessoas que tomaram placebo;

- Média de pessoas que não tomaram placebo;

- Média de pessoas que tomaram a vacina (não foi placebo) não tiveram reação;

- Quantidade de pessoas acima de 50 anos que tomaram a vacina (não foi placebo) e tiveram algum tipo de reação;
--}

manipulaArquivo = do 
                     c <- readFile "vacina.txt"
                     return c

dados = manipulaArquivo

imprimir x = do putStrLn x
         putStrLn "Média de pessoas que tomaram placebo: "
         putStrLn "Média de pessoas que não tomaram placebo: "
         putStrLn "Média de pessoas que tomaram a vacina (não foi placebo) não tiveram reação: "
         putStrLn " Quantidade de pessoas acima de 50 anos que tomaram a vacina (não foi placebo) e tiveram algum tipo de reação: "