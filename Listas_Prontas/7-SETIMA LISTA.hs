-- Nome : Euller Henrique Bandeira Oliveira
-- Matricula : 11821BSI210

import Data.Char

-- 1) Nos ítens a seguir, escreva primeiramente uma função recursiva.
--  Em seguida, reescreva essa função utilizando a função de ordem superior map, vista em aula.

-- Uso da função map
-- Definição da função  map

map3 :: (a -> b) -> [a] -> [b]
map3 f [] = []
map3 f (head:tail) = [f head] ++ map3 f (tail)

-- obs : f = função
-- obs : head = valor

-- valor de entrada = uma lista
-- valor de sáida = outra lista de mesmo tamanho

--(a) Função primeiros :: [(a,b)] -> [a] que extrai o primeiro elemento de cada tupla-2 dentro de uma lista. 
-- Dica: utilize a função fst

-- Função recursiva

primeiros :: [(a,b)] -> [a]
primeiros [] = []
primeiros (head:tail) = [fst head] ++ primeiros(tail)

-- Função de alta ordem 

primeiros2 :: [(a,b)] -> [a]
primeiros2 (head:tail) = map fst (head:tail)

-- (b) Função maiusculas :: String -> String que converte uma string para outra string com letras maiusculas. 
-- Dica: utilize a função toUpper da biblioteca Data.Char.

-- Função recursiva 

maiusculas :: String -> String
maiusculas [] = []
maiusculas (head:tail) = [toUpper head] ++ maiusculas(tail) 

-- Função de alta ordem

maiusculas2 :: String -> String
maiusculas2 (head:tail) = map toUpper (head:tail) 

-- (c) Função dobros :: Num a => [a] -> [a] que dobra todos os elementos de uma lista
--Função recursiva

dobros :: Num a => [a] -> [a]
dobros [] = []
dobros (head:tail) = [head * 2] ++ dobros(tail)

--Função de alta ordem

dobros2 :: Num a => [a] -> [a]
dobros2 (head:tail) = map (*2) (head:tail)

-- (d) Função hora_em_seg :: [Float] -> [Float] que converte uma lista de horas em uma lista de segundos.

--Função recursiva

hora_em_Seg :: [Float] -> [Float]
hora_em_Seg [] = []
hora_em_Seg (head:tail) = [head * 3600] ++ hora_em_Seg(tail)

-- Função de alta ordem

hora_em_Seg2 :: [Float] -> [Float]
hora_em_Seg2 (head:tail) = map (*3600) (head:tail)


--2) Escreva uma função em Haskell usando map chamada ellen que considera uma lista de strings e devolve uma lista dos tamanhos de cada string

-- Utiliza a função map

-- Definição da função map : (a -> b) -> [a] -> [b]

-- Valor de entrada = lista de string
-- Valor de saÍda = lista de inteiros 

--Função de alta ordem 

ellen2 :: [String] -> [Int] 
ellen2 (head:tail) = map (length) (head:tail)

--3)Reescreva a função do item anterior usando recursão

--Função recursiva

ellen :: [String] -> [Int]
ellen [] = []
ellen (head:tail) = [length head] ++ ellen(tail)

--4) Escreva uma função map2 que considera uma lista de funções e uma lista de valores (ambas do
-- mesmo tamanho) e devolve a lista resultante da aplicação de cada função da primeira lista sobre o
-- valor correspondente na segunda lista.

-- Utilização da função map e recursividade 
-- Definição da função map : (a -> b) -> [a] -> [b]
-- obs : [(a -> b)] = lista de funções
-- obs : [a] = lista de valores 
-- obs : [b] = lista de resultados

map2 :: [(a -> b)] -> [a] -> [b]
map2 [] [] = []
map2 (head:tail) (cabeca:cauda) 
 |length tail == length cauda = map head [cabeca] ++ map2 tail cauda
 |otherwise = error "As listas não possuem o mesmo tamanho"

-- obs : head = função
-- obs : cabeça = valor


--5) Escreva uma função fmap que considera um valor e uma lista de funções e devolve uma lista de 
--   resultados da aplicação de cada função sobre tal valor.
--   Por exemplo: fmap 3 [((*)2), ((+)2)] resulta  em [6,5].

-- utilização da função map e recursividade
-- Definição da função map : (a -> b) -> [a] -> [b]
-- obs: a = valor
-- obs: [(a -> b)] = lista de funções
-- obs: [b] = lista de resultados

fmap1 :: a -> [(a -> b)] -> [b]
fmap1 x [] = []
fmap1 x (head:tail) = map head [x] ++ fmap1 x (tail)

-- obs : head = função
-- obs : x = valor
-- obs : map head [x] = aplicação da função sobre o valor
-- obs : ++ fmap1 x (tail) == coloca o resultado da aplicação na lista e chama a função de novo para um nova função ser apicada sobre outro valor

--6)6. Qual resultado da execução destas funções? Faça no papel e compare o resultado com a execução no GHCI.

-- obs: sqrt = raiz quadrada

--  > map sqrt [0,1,4,9]
-- => [0.0,1.0,2.0,3.0]

-- succ = sucessor
--  > map succ " HAL "
-- => IBM

--  > map head [" bom "," dia "," turma "]
-- => [b,d,t]

--  > map even[8,10,3,48,5]
-- => [True, True, False, True, False]

--  >  map isDigit " A18 B7"
-- => [False,True,True,False,False,True]

--  > map length ["ci^enca", "da", "computa¸c~ao"]
-- =>[7,2,12] 

--  > map ( sqrt . abs . snd ) [('A' ,100) ,('Z' , -36)]
-- =>[10.0,6.0]
-- obs : é avaliado da esquerda para direita 

--7)Nos itens a seguir, escreva primeiramente uma função recursiva. 
-- Em seguida, reescreva essa função utilizando a função de ordem superior filter, vista em aula.

--Uso da função filter
--Definição da função filter :: (a -> Bool) -> [a] -> [b]

--valor de entrada = uma lista
--valor de saída = uma sub-lista

-- a) Função pares :: [Int] -> [Int] que remove todos os elementos ımpares de uma lista. 
-- Dica: implemente uma função que verifica se um número é par

--Função recursiva

pares :: [Int] -> [Int]
pares [] = []
pares (head:tail)
 |mod head 2 == 0 =  [head] ++ pares(tail)
 |otherwise = pares(tail)


--Função recursiva utilizando even

pares2 :: [Int] -> [Int]
pares2 [] = []
pares2 (head:tail)
 |even head = [head] ++ pares2(tail)
 |otherwise = pares2(tail) 

--Função de alta ordem utilizando uma função criada

par :: Int -> Bool
par x 
 |mod x 2 == 0 = True
 |otherwise = False

pares3 :: [Int] -> [Int]
pares3 (head:tail) = filter par (head:tail) 

-- Função de alta ordem utilizando a função even

pares4 :: [Int] -> [Int]
pares4 (head:tail) = filter even (head:tail)


--b) Função alfa :: String -> String que remove todos os caracteres nao-alfabéticos de uma string.
-- Dica: utilize a função isAlpha da biblioteca Data.Char.

--Função recursiva

alfa :: String -> String
alfa [] = []
alfa (head:tail)
 |isAlpha head = [head] ++ alfa(tail)
 |otherwise = alfa(tail)

-- obs : isApha = se o elemento for caractere, entao [head] ++ alfa(tail)
-- obs : otherwise = não execute, isto é , elimine os caracteres 

-- Função de alta ordem

alfa2 :: String -> String
alfa2 (head:tail) = filter isAlpha (head:tail)

-- obs : filtree imprima todos os elmentos que são caracteres presentes na string

-- (c) Defina a função rm char :: Char -> String -> String que remove todas as ocorrencias de um caractere em uma string.

--Função recursiva 

rmchar :: Char -> String -> String
rmchar x [] = []
rmchar x (head:tail) 
 |x /= head = [head] ++ rmchar x (tail)
 |otherwise = rmchar x (tail)

--Função de alta ordem

rmchar2 :: Char -> String -> String
rmchar2 x (head:tail) = filter (/=x) (head:tail)

-- (d) Defina a função acima :: Int -> [Int] -> [Int] que remove todos os numeros menores ou iguais a um determinado valor

-- Função recursiva

acima :: Int -> [Int] -> [Int]
acima x [] = []
acima x (head:tail) 
 |head <= x = acima x (tail)
 |otherwise = [head] ++ acima x (tail)

-- Função de alta ordem

acima2 :: Int -> [Int] -> [Int]
acima2 x (head:tail) = filter (<=x) (head:tail)

-- (e) Escreva uma função desiguais :: Eq t => [(t,t)] -> [(t,t)] que remove todos os pares (x,y) em que x == y.

-- Função na qual a tupla tem que ter os elementos iguais e pares

desiguaisepares :: (Integral t,Eq t) => [(t,t)] -> [(t,t)]
desiguaisepares [] = []
desiguaisepares (head:tail)
 |(fst head == snd head) && ( even (fst head)  && even (snd head) ) = desiguaisepares(tail)
 |otherwise = [head] ++ desiguaisepares(tail)

-- Função recursiva

desiguais :: Eq t => [(t,t)] -> [(t,t)]
desiguais [] = []
desiguais (head:tail)
 |(fst head == snd head) = desiguais(tail)
 |otherwise = [head] ++ desiguais(tail)

-- Função de alta ordem

desiguais3 :: Eq t => (t, t) -> Bool
desiguais3 (x,y)
 |(x /= y) = True
 |otherwise = False

desiguais2 :: Eq t => [(t, t)] -> [(t, t)]
desiguais2 (head:tail) = filter (desiguais3) (head:tail) 

-- obs : desiguais3 = função condicional

--8) Nos exercícios a seguir, escreva primeiramente uma funcâo recursiva. 
-- Em seguida, reescreva essa função utilizando a função de ordem superior foldr, vista em aula.

-- (a) Função produto :: Num a => [a] -> a que computa o produto dos numeros de uma lista.

-- Função recursiva

produto :: Num a => [a] -> a
produto [] = 1
produto (head:tail) =  head * produto(tail)

-- Função de alta ordem

produto2 :: Num a => [a] -> a
produto2 (head:tail) = foldr (*) (1) (head:tail) 

-- (b) Função e_logico :: [Bool] -> Bool que “e logico” de todos os itens em uma lista

--Função recursiva

e_logico :: [Bool] -> Bool
e_logico [] = True
e_logico (head:tail) 
 |head == True = True && e_logico(tail) 
 |otherwise = False && e_logico(tail)


--Função de alta ordem

e_logico2 :: [Bool] -> Bool
e_logico2 (head:tail) = foldr (&&) (True) (head:tail)

-- (c) Função concatena :: [String] -> String que junta uma lista de strings em uma unica string.

--Função recursiva

concatena :: [String] -> String
concatena [] = []
concatena (head:tail) = head ++ concatena(tail)

--Função de alta ordem

concatena2 :: [String] -> String
concatena2 (head:tail) = foldr (++) ([]) (head:tail)

-- (d) Função maior :: Num a => a -> [a] -> a que considera um numero e o compara com os
-- elementos de uma lista.
-- Caso esse numero seja maior que todos os elementos da lista, ele será retornado pela função.
-- Caso contrário, a função retorna o maior elemento da lista. 

-- Por exemplo:
-- Main *> maior 18 [3 ,6 ,12 ,4 ,55 ,11]
-- 55
-- Main *> maior 111 [3 ,6 ,12 ,4 ,55 ,11]
-- 111

-- Lista de entrada = numeros
-- Lista de saida = um numero

-- Função recursiva

maior :: (Ord a, Num a) => a -> [a] -> a
maior x [] = x
maior x (head:tail)
 |x > maior x (tail) = x
 |head > maior x (tail) = head
 |otherwise = maior x (tail) 

--Função de alta ordem

maior2 :: (Ord a, Num a) => a -> [a] -> a
maior2 x (head:tail) = foldr (max) (x) (head:tail)

-- 9. Para cada uma das seguintes especificações, escreva uma função em Haskell. Use as funções map, filter e foldr

-- (a) Função numof que considera um valor e uma lista e devolve o numero de ocorrências do valor
-- na lista

-- Uso da função filter
-- Definição da função filter: (a -> Bool) -> [a] -> [b]

-- Lista de entrada = valores inteiros
-- Lista de saida = sub lisa de valores inteiros

--Função recursiva

numof :: Int -> [Int] -> Int 
numof x [] = 0
numof x (head:tail)
 | head == x = 1 + numof x (tail)
 | otherwise =  numof x (tail)

--Função de alta ordem

numof2 :: Int -> [Int] -> [Int]
numof2 x (head:tail) = filter (==x) (head:tail)


-- (b) Função ellen que considera uma lista de strings e devolve uma lista dos tamanhos de cada string

-- Uso da função map
-- Definição da função map = (a -> b) -> [a] -> [b]

-- Lista de entrada = lista de string 
-- Lista de saida = lista de inteiros de mesmo tamanho

-- Função recursiva

ellen3 :: [String] -> [Int]
ellen3 [] = []
ellen3 (head:tail) = [length head] ++ ellen3(tail)

--Função de alta ordem

ellen4 :: [String] -> [Int]
ellen4 (head:tail) = map (length) (head:tail) 

-- (c) Função ssp que considera uma lista de inteiros e devolve a soma dos quadrados dos elementos
-- positivos da lista

-- Uso da função map
-- Definição da função map : (a -> b) -> [a] -> [b]

-- Uso da função filter 
-- Definição da função filter: (a -> Bool) -> [a] -> [b]

-- Uso da função foldr 
-- Definição da função foldr: (a -> b -> b) -> b -> [a] -> b

ssp :: [Int] -> Int
ssp (head:tail) = foldr(+) 0 ( map (^2) (filter (>0) (head:tail)))

-- Obs : A filter filtra todos os elementos positivos
-- Obs : A map eleva ao quadrado todos os elementos filtrados
-- Obs : A foldr soma todos os elementos positivos que foram elevados ao quadrado

-- soma dos quadrados dos elementos positivos da lista.

-- 10. Defina a função sqp (soma dos quadrados dos primos) que considera uma lista de inteiros como
-- argumento, remove os que nao são primos, eleva os inteiros restantes ao quadrado e, finalmente,
-- soma tais quadrados. 
-- Por exemplo:
-- *Main > sqp [2, 4, 7, 1, 3]
-- 62

--Função de alta ordem  

divisores :: Int -> [Int]
divisores n = [x | x <-[1..n], n`mod`x==0]

primo :: Int -> Bool
primo n = divisores n == [1,n]

sqp :: [Int] -> Int
sqp (head:tail) = foldr (+) 0 ( map (^2) (filter (primo)(head:tail)))

-- 11. Seja uma lista de strings. Utilizando funções de ordem superior, faça as seguintes funções:

-- (a) Uma funçãao que devolva o valor da soma dos comprimentos de cada string (elemento) da lista.
-- Isto é, a soma total dos comprimentos da lista de entrada;

-- Função recursiva

somatam :: [String] -> Int
somatam [] = 0
somatam (head:tail) =  length head + somatam(tail) 

-- Função de alta ordem

-- Uso da função map
-- Definição da função map : (a -> b) -> [a] -> [b]

-- Uso da função foldr
-- Definição da função foldr: (a -> b -> b) -> b -> [a] -> b

somatam2 :: [String] -> Int
somatam2 (head:tail) = foldr (+) 0 (map (length) (head:tail)) 

-- (b) Uma função que devolva uma lista das strings formadas por palavras que se iniciem por uma letra específica;

-- Função recursiva

letraesp :: Char -> [String] -> [String]
letraesp x [] = []
letraesp x (cabeca:cauda)
 |head cabeca == x = [cabeca] ++ letraesp x (cauda)
 |otherwise = letraesp x (cauda)

-- obs = o otherwise elimina a string que não começa pela palavra especifica

letraesp' :: Char -> [String] -> [String]
letraesp' x (h:t) = (filter (\h -> head (h) == x) (h:t))

-- Filtre todo h, tal que head de h == x 

-- Para todo h, se head h == x, então coloque o h dentro da lista


-- (c) Faça uma função que devolva a mesma lista de entrada, exceto que as letras em cada palavra ficarão
-- invertidas entre maiusculas e minusculas.
-- Por exemplo, onde tem 'e' troque por 'E', onde tem 'E' troque por 'e', e assim por diante.

-- Função recursiva

inverter :: String -> String
inverter [] = []
inverter (head:tail) 
 |head == toUpper head = [toLower head] ++ inverter(tail)
 |head == toLower head = [toUpper head] ++ inverter(tail)

inverter4 :: [String] -> [String]
inverter4 [] = []
inverter4 (head:tail) = [inverter(head)] ++ inverter4(tail) 

-- Função de alta ordem

inverter2 :: String -> String
inverter2 [] = []
inverter2 (head:tail) 
 |head == toUpper head = [toLower head] ++ inverter2(tail)
 |head == toLower head = [toUpper head] ++ inverter2(tail)

inverte :: [String] -> [String]
inverte (head:tail) = map (inverter) (head:tail)

-- 12. As seguintes funçõoes do prelúdio-padrão são de ordem superior. Mostre o resultado das
-- seguintes execuções:

-- Main > all ( <10) [1 ,3 ,5 ,7 ,9]
-- => True

-- Obs : all = devolve todos os elementos que atendem a condição especificada

-- Main > all (==1) [1 ,1 ,0 ,1 ,1]
-- => False

-- Obs : all = devolve todos os elementos que atendem a condição espeficada

-- Main > all even [2 ,4 ,6 ,8 ,10]
-- => True

-- Obs : devolve True se todos os elementos atenderem a condição especificada

-- Main > any (1==) [0 ,1 ,2 ,3 ,4 ,5]
-- => True

-- Obs : devolve True se pelo menos um elemento atender a condição especificada

-- Main > any ( >5) [0 ,1 ,2 ,3 ,4 ,5]
-- => False

-- Obs : devolve False se nenhum pelo elemento atender a condição especificada 

-- Main > any even [1 ,3 ,5 ,7 ,9]
-- => False

-- Obs : devolve False se nenhum pelo elemento atender a condição especificada 

-- Main > takeWhile (<3) [1 ,2 ,3 ,4 ,5]
-- => [1,2]

-- Obs: takeWhile = "pega" os numeros da esquerda pra direita enquanto (condição especificada) for verdadeira

-- Main > takeWhile (>3) [1 ,2 ,3 ,4 ,5]
-- => []

-- Obs : Uma lista vazia é retornada porque o primeiro elemento não é maior que 3, então o laço while nem se inicia

-- Main > takeWhile odd [1 ,3 ,5 ,7 ,9 ,10 ,11 ,13 ,15 ,17]
-- => [1,3,5,7,9]

-- Obs : "pega" os numeros enquanto os numeros forem impares 

-- Main > takeWhile ('w' >) " hello world "
-- => "hello"

-- Obs : "pega" os caracteres que são menores do que 'w' 

-- Main > dropWhile (<3) [1 ,2 ,3 ,4 ,5]
-- => [3,4,5]

--Obs : dropWhile = "pega" os elementos que não antenderam a condição especificada

-- Main > dropWhile even [2 ,4 ,6 ,7 ,9 ,11 ,12 ,13 ,14]
-- => [7,9,11,12,13,14]

--Obs: dropWhile = "pega" os elementos que não antenderam a condição especificada

-- Main > dropWhile ('w'>) "hello , world "
-- => "world"

--Obs : dropWhile = "pega" os elementos que não antenderam a condição especificada