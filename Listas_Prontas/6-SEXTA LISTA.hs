-- Nome : Euller Henrique Bandeira Oliveira
-- Matricula : 11821BSI210

-- jeito errado
-- soma :: [a] -> a
-- soma [head] = head
-- soma (head:tail) = head + soma(tail)

-- jeito certo 
soma :: Num a => [a] -> a
soma [head] = head
soma (head:tail) = head + soma(tail)

-- Num é uma classe, uma restrição.

-- 1. Quais os tipos das seguintes funções? Inclua as devidas restrições de classe, se necessário.

-- a) segundo xs = head (tail xs)

segundo :: [a] -> a 
segundo l = head (tail l)

-- b) trocar (x, y) = (y, x)
trocar :: (a,b) -> (b,a)
trocar (x,y) = (y,x)

-- c) parear x y = (x, y)
parear :: a -> b -> (a,b)
parear x y = (x,y)

-- d) dobro x = x * 2
dobro :: Num a => a -> a
dobro x = x * 2

-- e) palindromos xs = reverse xs == xs
palindromo :: Eq a => [a] -> Bool
palindromo (head:tail) 
 | (head:tail) == reverse (head:tail) = True
 | otherwise = False 

-- 2) Escreva uma função que forneça o terceiro elemento de uma tupla-3. Declare o tipo mais geral para essa função.

terceiro :: (a,b,c) -> c 
terceiro (x,y,z) = z

-- 3) Fornecidos três valores, a, b e c, implemente uma função que retorne quantos desses três são
-- iguais.
-- A reposta deve ser 3, se todos sao iguais; 
-- 2, se dois sao iguais e um é distinto dos demais 
-- ou 0, se todos sao distintos entre si. 

-- Declare o tipo mais geral para essa função.

iguais :: (Eq a, Num a) => a -> a -> a -> a
iguais a b c 
 | a == b && b == c = 3
 | a == b || b == c || a == c = 2
 | otherwise = 0

-- 4. Escreva uma função que receba uma tupla-3 e retorne uma tupla-2 com o maior e o menor elemento dentre os três. 

-- Declare o tipo mais geral para essa função.

maior_3 :: Ord a =>(a, a, a) -> a
maior_3 (x,y,z)
 | x > y && x > z = x 
 | y > z = y
 | otherwise = z
 
menor_3 :: Ord a =>(a, a, a) -> a
menor_3 (x,y,z)  
 | x < y && x < z = x
 | y < z = y
 | otherwise = z
 
maior_menor_3 :: Ord a => (a,a,a) -> (a,a)
maior_menor_3 (x, y, z) = (maior_3(x,y,z), menor_3(x,y,z))
  
-- 5. Implemente as seguintes funções recursivas em Haskell definindo o tipo mais geral de cada função.

-- (a) Determinar o comprimento de uma lista. Exemplo de uso:
-- > comprimento [3 ,14 ,1 ,5 ,9]
-- 5

--Função recursiva

comprimento :: Num b => [a] -> b
comprimento [] = 0
comprimento (head:tail) = 1 + comprimento(tail)

comprimento2 :: [a] -> Int
comprimento2 (head:tail) = length (head:tail)

-- Não precisa de função de alta ordem

-- (b) Determinar o somatorio dos elementos de uma lista. Exemplo de uso:
-- > somatorio [3 ,14 ,1 ,5 ,9]
-- 32

-- Função recursiva

somatorio :: Num a => [a] -> a
somatorio [] = 0
somatorio (head:tail) = head + somatorio (tail)

-- Função de alta ordem

somatorio2 :: Num a => [a] -> a
somatorio2 (head:tail) = foldr (+) 0 (head:tail)

-- (c) Determinar o somatorio dos elementos ímpares de uma lista. Exemplo de uso:
-- > somatorio_impares [3 ,14 ,1 ,5 ,9]
-- 18

-- Função recursiva

somatorio_impares :: Integral a => [a] -> a
somatorio_impares [] = 0
somatorio_impares (head:tail)
 | mod head 2 == 0 = somatorio_impares(tail)
 | mod head 2 /= 0 = head + somatorio_impares(tail)

--Função de alta ordem

somatorio_impares2 :: Integral a => [a] -> a
somatorio_impares2 (head:tail) = foldr (+) 0 (filter (odd) (head:tail))

-- (d) Determinar a soma dos elementos de uma lista que sao múltiplos de 3. Exemplo de uso:
-- > soma_mult_3 [3 ,14 ,1 ,5 ,9]
-- 12

--Função recursiva
soma_mult_3 :: Integral a => [a] -> a
soma_mult_3 [] = 0
soma_mult_3 (head:tail)
 |mod head 3 == 0 = head + soma_mult_3(tail)
 |otherwise = soma_mult_3(tail)


--Função de alta ordem
multiplo3 :: Integral a => a -> Bool
multiplo3 x
 |mod x 3 == 0 = True
 |otherwise = False

soma_mult_3e :: Integral a => [a] -> a
soma_mult_3e (head:tail) = foldr (+) 0 (filter (multiplo3) (head:tail))

-- (e) Determinar o produtorio dos elementos de uma lista. Exemplo de uso:
-- > produtorio [3 ,14 ,1 ,5 ,9]
-- 1890

--Função recursiva

produtorio :: Num a => [a] -> a
produtorio [] = 1
produtorio (head:tail) = head * produtorio(tail)

--Função de alta ordem

produtorio2 :: Num a => [a] -> a
produtorio2 (head:tail) = foldr (*) 1 (head:tail)

-- (f) Determinar o n-esimo elemento de uma lista. Exemplo de uso:
-- > n_esimo 3 [3 ,14 ,1 ,5 ,9]
-- 5
-- Obs.: Considere que o primeiro elemento da lista está na posição 1.

n_esimo :: (Num a, Eq a) => a -> [b] -> b
n_esimo 1 (head:tail) = head 
n_esimo x (head:tail) = n_esimo (x-1) (tail)
 
--(g)Determinar o ultimo elemento de uma lista. 
--   Exemplo de uso:
--   > ultimo [3 ,14 ,1 ,5 ,9]
--   9

-- Função recursiva

ultimo :: [a] -> a 
ultimo [head] = head
ultimo (head:tail) = ultimo(tail) 


-- N precisa de função de alta ordem

ultimo2 :: [a] -> a
ultimo2 (head:tail) = last (head:tail)

-- (h) Substituir todas as ocorrencias de um elemento x em uma lista por outro elemento y. 
-- Exemplo de uso:
-- > substituir_todos 1 2 [3 ,14 ,1 ,5 ,1]
--  [3 ,14 ,2 ,5 ,2]
    
substituir_todos :: Eq a => a -> a -> [a] -> [a] 
substituir_todos x y [] = []
substituir_todos x y (head:tail)
 |head == x = [y] ++ substituir_todos x y (tail)
 |otherwise = [head] ++ substituir_todos x y (tail)

-- (i) Determinar o maior elemento de uma lista. Exemplo de uso:
-- > maior [3 ,1 ,4 ,1 ,5 ,9]
--   14

--Função recursiva

maior :: (Ord a, Num a) => [a] -> a
maior [head] = head 
maior (head:tail)
 | head > maior(tail) = head
 | otherwise = maior(tail)

-- Função de alta ordem 

maior2 :: (Ord a, Num a) => [a] -> a
maior2 (head:tail) = foldr (max) 0 (head:tail) 

-- (j) Escreva uma função que retorna verdadeiro se todos os elementos de uma lista de inteiros forem ímpares, ou falso, caso contrario.

--  Exemplo de uso:
-- > impares [3 ,1 ,5 ,9]
--   True

-- Função recursiva

impares :: (Eq a, Integral a) => [a] -> Bool 
impares [] = True
impares (head:tail)
 |mod head 2 /= 0 = True && impares(tail)
 |mod head 2 == 0 = False && impares(tail)

--Função de alta ordem

impares2 :: (Eq a, Integral a) => [a] -> Bool
impares2 (head:tail) = foldr (&&) (False) (map (even) (head:tail))
 
-- (k) Inserir um elemento ordenadamente em uma lista já ordenada. Exemplo de uso:
-- > insere 4 [1 ,3 ,5]
-- [1 ,3 ,4 ,5]
-- Se o elemento ja existir, pode inseri-lo depois de sua ocorrência.

insere :: (Ord a, Num a) => a -> [a] -> [a]
insere x [] = []
insere x (head:tail)
  | x > head = [head] ++ [x] ++ insere x (tail) 
  | otherwise = [head] ++ insere x (tail)

-- (l) Verificar se um elemente pertece a lista. Exemplo de uso:
-- > pertence 5 [3 ,14 ,1 ,5 ,9]
-- True

-- Função recursiva

pertence :: Eq a => a -> [a] -> Bool
pertence x [] = False
pertence x (head:tail)
 | x == head = True 
 | otherwise = False || pertence x (tail)

--Função de alta ordem

pertence2 ::  Eq a => a -> [a] -> Bool
pertence2 x (head:tail) = foldr (||) False (map (==x) (head:tail))

-- (m) Dada uma lista de tuplas-2, criar uma nova lista com apenas os primeiros elementos de cada tupla. 
-- Exemplo de uso:
-- > primeiros [(3 ,14) ,(1 ,5) ,(9 ,1)]
-- [3 ,1 ,9]

-- Função recursiva

primeiros :: [(a,b)] -> [a]
primeiros [] = []
primeiros (head:tail) = [fst head] ++ primeiros(tail)

-- Função de alta ordem

primeiros2 :: [(a,b)] -> [a]
primeiros2 (head:tail) = map (fst) (head:tail)

-- (n) Dada uma lista de listas, concatenar todas as sub-listas em uma unica lista. Exemplo de uso:
-- > concatenar [[3 ,14 ,1 ,5] ,[9 ,1 ,2]]
-- [3 ,14 ,1 ,5 ,9 ,1 ,2]

-- Função recursiva

concatenar :: [[a]] -> [a]
concatenar [] = []
concatenar (head:tail) = head ++ concatenar(tail) 

-- Função de alta ordem

concatenar2 :: [[a]] -> [a]
concatenar2 (head:tail) = foldr (++) ([]) (head:tail)