-- Nome : Euller Henrique Bandeira Oliveira
-- Matricula : 11821BSI210

import Data.Char


-- 1. Mostre os resultados das seguintes execuções, ou explique porque elas não podem ser executadas
-- primeira versão

separa :: [Int] -> [Int]
separa (p:s:r) = (s:r)

separa1 :: [Int] -> [Int]
separa1 (p:s:r) = [s] ++ r

-- separa (head:head2:tail) = (head2:tail)

--  > separa [1 ,2 ,3 ,4 ,5]
-- => [2,3,4,5]

-- > separa [1 ,2 ,3]
-- => [2,3]

-- > separa [1 ,2]
-- => [2]

--  > separa [1]
-- => Error
-- Justificativa:  Tal lista não possui nenhuma cauda, ou seja,como possui somente um elemento contém somente a cabeça, portando,
-- é impossivel eliminar o head e retornar o resto da lista.


-- segunda versão

-- separab :: [ Int ] -> [ Int ]
-- separab (p:s:r) = (r:s:p)

-- versão correta

separaab :: [Int] -> [Int] 
separaab (p:s:r) = r ++ [s] ++ [p]

-- separab (head: head2: tail) = (tail : head2 : head)

-- > separab [1 ,2 ,3 ,4 ,5]
-- => Error
-- Justificativa: Como o r na função é o tail da lista, não é possivel usar o operador : para construir uma lista, pois o elemento r já é uma lista!! O correto seria utilizar o operador ++ para concatenar todos os elementos.

-- > separab [1 ,2 ,3]
-- => Error
-- Justificativa: Como o r na função é o tail da lista, não é possivel usar o operador : para construir uma lista, pois o elemento r já é uma lista!! O correto seria utilizar o operador ++ para concatenar todos os elementos

-- > separab [1 ,2]
-- => Error
-- Justificativa: Como o r na função é o tail da lista, não é possivel usar o operador : para construir uma lista, pois o elemento r já é uma lista!! O correto seria utilizar o operador ++ para concatenar todos os elementos

-- > separab [1]
-- => Error
-- Justificativa: Como o r na função é o tail da lista, não é possivel usar o operador : para construir uma lista, pois o elemento r já é uma lista!! O correto seria utilizar o operador ++ para concatenar todos os elementos

-- terceira versão
separac :: [ Int ] -> [ Int ]
separac (p:r) = r
-- separac (head : tail) = tail

--  > separac [1 ,2 ,3 ,4 ,5]
-- => [2,3,4,5]
 
-- > separac [1 ,2 ,3]
-- => [2,3]

-- > separac [1 ,2]
-- => [2]

-- > separac [1]
--  => Error.
-- Justificativa:  Tal lista não possui nenhuma cauda, ou seja,como possui somente um elemento contém somente a cabeça, portando,
-- é impossivel eliminar o head e retornar o resto da lista.

-- > separac []
--  => Error
-- Justificativa:  Tal lista não possui nenhuma cauda, ou seja,como possui somente um elemento contém somente a cabeça, portando,
-- é impossivel eliminar o head e retornar o resto da lista.


-- 2. Implemente as seguintes funções em Haskell. Mostre a execução passo a passo dessas funções para os exemplos fornecidos.

-- a) Determinar o comprimento de uma lista, ex:
-- > comprimento [3 ,14 ,1 ,5 ,9]
-- =>5

comprimento :: [Int] -> Int
comprimento [] = 0 
comprimento (head : tail) = 1 + comprimento(tail) 

-- comprimento [3,14,1,5,9]
-- 1 + comrprimento ([14,1,5,9])
-- 1 + ( 1 + comprimento ([1,5,9]))
-- 1 + ( 1 + ( 1 + comprimento ([5,9])))
-- 1 + ( 1 + ( 1 + ( 1 + comprimento ([9]))))
-- 1 + ( 1 + ( 1 + ( 1 + ( 1 + comprimento ([ ])))))
-- 1 + ( 1 + ( 1 + ( 1 + ( 1 + 0))))

--Função polimorfica

comprimento1 :: Num b => [a] -> b
comprimento1 [] = 0
comprimento1 (head:tail) = 1 + comprimento1(tail)

-- b) Determinar o somatorio dos elementos de uma lista, ex:
-- > somatorio [3 ,14 ,1 ,5 ,9]
-- => 32

somatorio :: [Int] -> Int
somatorio [] = 0 
somatorio (head : tail) = head + somatorio (tail) 

-- somatorio [3,14,1,5,9]
-- 3 + somatorio([14,1,5,9]
-- 3 + ( 14 + somatorio([1,5,9]))
-- 3 + ( 14 + ( 1 + somatorio ([ 5,9 ])))
-- 3 + ( 14 + ( 1 + ( 5 + somatorio([9]))))
-- 3 + ( 14 + ( 1 + ( 5 + ( 9 + somatorio ([])))))
-- 3 + ( 14 + ( 1 + ( 5 + ( 9 + 0 ))))
-- 32

--Função polimorfica

somatorio1 :: Num a => [a] -> a 
somatorio1 [] = 0
somatorio1 (head:tail) = head + somatorio1(tail)


-- c) Determinar o somatorio dos elementos ímpares de uma lista, ex:
--  > somatorio_impares [3 ,14 ,1 ,5 ,9]
-- => 18

somatorio_impares :: [Int] -> Int
somatorio_impares [ ] = 0
somatorio_impares (head : tail) 
 |mod head 2 == 0 = somatorio_impares(tail)
 |otherwise = head + somatorio_impares(tail)
 
-- somatorio_impares [3,14,1,5,9]
-- 3 + somatorio_impares( [14,1,5,9])
-- 3 + ( somatorio_impares ([1,5,9]))
-- 3 + ( 1 + somatorio_impares( [5,9]))
-- 3 + ( 1 + ( 5 + somatorio_impares ([9])) 
-- 3 + ( 1 + ( 5 + ( 9 + somatorio_impares ([ ])) 
-- 3 + ( 1 + ( 5 + ( 9 + 0)) 
-- 18 

--Função polimorfica

somatorio_impares1 :: Integral a => [a] -> a
somatorio_impares1 [] = 0
somatorio_impares1 (head:tail)
 |mod head 2 == 0 = somatorio_impares1(tail)
 |otherwise = head + somatorio_impares1(tail)


-- d) Determinar a soma dos quadrados dos elementos de uma lista, ex:

soma_quadrados :: [Int] -> Int
soma_quadrados [ ] = 0
soma_quadrados (head : tail) = head ^ 2 + soma_quadrados (tail)

-- soma_quadrados [3, 14, 1, 5 , 9]
-- 3 ^ 2 + soma_quadrados ([ 14, 1, 5, 9]) 
-- 9 + soma_quadrados ( [14, 1, 5, 9]) 
-- 9 + ( 14 ^ 2 + soma_quadrados ([1, 5, 9])) 
-- 9 + ( 196 + soma_quadrados ([1, 5, 9])) 
-- 9 + ( 196 + ( 1 ^ 2 + soma_quadrado ([5, 9]))) 
-- 9 + ( 196 + ( 1 + soma_quadrados ([5, 9])))
-- 9 + ( 196 + ( 1 + ( 5 ^ 2 + soma_quadrados ([9]))))
-- 9 + ( 196 + ( 1 + ( 25 + soma_quadrados ([9]))))
-- 9 + ( 196 + ( 1 + ( 25 + ( 9 ^ 2 + soma_quadrados ([ ])))))
-- 9 + ( 196 + ( 1 + ( 25 + ( 81 + 0))))
-- 9 + ( 196 + ( 1 + ( 25 + (81))))
-- 9 + ( 196 + ( 1 + (106)))
-- 9 + ( 196 + (107))
-- 9 + (303)
-- 312

--Função polimorfica

soma_quadrados2 :: Num a => [a] -> a
soma_quadrados2 [] = 0
soma_quadrados2 (head:tail) = head ^ 2 + soma_quadrados2(tail)

--e)Determinar a soma dos elementos de uma lista que sao múltiplos de 3, ex:

soma_mult_3 :: [Int] -> Int
soma_mult_3 [ ] = 0 
soma_mult_3 (head : tail) 
 |mod head 3 /= 0 = soma_mult_3(tail)
 |otherwise = head + soma_mult_3(tail)

-- soma_mult_3 [3, 14, 1, 5, 9]
-- 3 + soma_mult_3 ([14,1,5,9])
-- 3 + (soma_mult_3 ([1,5,9]))
-- 3 + (soma_mult_3 ([5,9])
-- 3 + (soma_mult_3 ([9]))
-- 3 + ( 9 + soma_mult_3 ([]))
-- 3 + ( 9 + 0)
-- 12

--Função polimorfica

soma_mult_3a :: Integral a => [a] -> a
soma_mult_3a [] = 0
soma_mult_3a (head:tail)
 |mod head 3 /= 0 = soma_mult_3a(tail)
 |otherwise = head + soma_mult_3a(tail)

-- (f) Determinar o produtorio dos elementos de uma lista, ex: 

-- > produtorio [3 ,14 ,1 ,5 ,9]
--   1890

produtorio :: [Int] -> Int
produtorio  [ ] = 1
produtorio (head:tail) = head * produtorio (tail)

-- produtorio [3, 14, 1, 5 , 9]
-- 3 * produtorio([14, 1, 5, 9])
-- 3 * (14 * produtorio([1,5,9]))
-- 3 * (14 * (1 * produtorio ([5,9])))
-- 3 * (14 * (1 * (5 * produtorio([9]))))
-- 3 * (14 * (1 * (5 * (9 * produtorio ([ ])))))
-- 3 * (14 * (1 * (5 * (9 * 1))))
-- 1890

--Função polimorfica

produtorio1 ::Num a => [a] -> a
produtorio1 [] = 1
produtorio1 (head:tail) = head * produtorio1(tail)

-- (g) Determinar o n-esimo elemento de uma lista, ex:

-- > n_esimo 3 [3 ,14 ,1 ,5 ,9]
-- 5

n_esimo :: Int -> [Int] -> Int
n_esimo 0 (head:tail) = head
n_esimo n (head:tail) = n_esimo (n - 1) (tail)

-- n_esimo 3 [3,14,1,5,9]
-- n_esimo (3 - 1) [14,1,5,9]
-- n_esimo (2) [14,1,5,9]
-- n-esimo (2 -1) [1, 5, 9]
-- n-esimo (1) [1,5,9]
-- n-esimo (1 - 1) [5, 9]
-- n-esimo (0) [5,9]
-- 5

-- Função polimorfica

n_esimo1 :: (Eq a, Num a) => a -> [b] -> b 
n_esimo1 0 (head:tail) = head
n_esimo1 n (head:tail) = n_esimo1 (n - 1) (tail)

--(h) Determinar o ultimo elemento de uma lista, ex:
-- > ultimo [3 ,14 ,1 ,5 ,9]
-- 9

ultimo :: [Int] -> Int
ultimo [head] = head
ultimo (head:tail) = ultimo (tail)

-- ultimo [3,14,1,5,9]
-- ultimo ([14,1,5,9])
-- ultimo ([1,5,9])
-- ultimo ([5,9])
-- ultimo ([9])
-- 9

-- Função polimorfica

ultimo1 :: [a] -> a
ultimo1 [head] = head
ultimo1 (head:tail) = ultimo1(tail)

-- (i) “Duplicar” os elementos de uma lista, ex:
-- > duplica [3 ,14 ,1 ,5 ,9]
-- [3 ,3 ,14 ,14 ,1 ,1 ,5 ,5 ,9 ,9]

duplicar :: [Int] -> [Int]
duplicar [ ] = [ ]
duplicar (head:tail) = [head] ++ [head] ++ duplicar(tail) 

-- duplicar [3,14,1,5,9]
-- [3] ++ [3] ++ duplicar ([14,1,5,9])
-- [3] ++ [3] ++ ([14] ++ [14] ++ duplicar ([1,5,9]))
-- [3] ++ [3] ++ ([14] ++ [14] ++ ([1] ++ [1] ++ duplicar ([5,9])))
-- [3] ++ [3] ++ ([14] ++ [14] ++ ([5] ++ [5] ++ duplicar ([9])))
-- [3] ++ [3] ++ ([14] ++ [14] ++ ([5] ++ [5] ++ ([9] ++ [9] ++ duplicar ([]))))
-- [3,3,14,14,5,5,9,9]


--Função polimorfica

duplicar2 :: [a] -> [a]
duplicar2 [] =[]
duplicar2 (head:tail) = [head] ++ [head] ++ duplicar2(tail)

-- (j) Reverter uma lista, ex:
-- > reverso [3 ,14 ,1 ,5 ,9]
-- [9 ,5 ,1 ,14 ,3]

reverso :: [Int] -> [Int]
reverso [ ] = [ ]
reverso (head : tail) = reverso(tail) ++ [head]

-- reverso [3,14,1,5,9]
-- reverso ([14,1,5,9]) ++ [3]
-- reverso (([1,5,9]) ++ [14]) ++ [3]
-- reverso ((([5,9]) ++ [1]) ++ [14]) ++ [3] 
-- reverso (((([9]) ++ [5]) ++ [1]) ++ [14]) ++ [3]
-- reverso ((((([]) ++ [9]) ++ [5]) ++ [1]) ++ [14]) ++ [3]
--[9,5,1,14,3]

--Função polimorfica 

reverso2 :: [a] -> [a]
reverso2 [] = []
reverso2 (head:tail) = reverso2(tail) ++ [head]

-- (k) Substituir todas as ocorrencias de um elemento x em uma lista por outro elemento y, ex:
-- > substituir_todos 1 2 [3 ,14 ,1 ,5 ,1]
-- [3 ,14 ,2 ,5 ,2]

substituir_todos :: Int -> Int -> [Int] -> [Int]
substituir_todos x y [] = [] 
substituir_todos x y (head:tail) 
 |head == x = [y] ++ substituir_todos x y (tail) 
 |otherwise = [head] ++ substituir_todos x y (tail) 
 
-- substituir_todos 1 2 [3, 14, 1, 5, 1]
-- [3] ++ substituir_todos 1 2 ([14, 1, 5, 1])
-- [3] ++ ([14] ++ substituir_todos 1 2 ([1,5,1]))
-- [3] ++ ([14] ++ ([2] ++ substituir_todos 1 2 ([5,1])))
-- [3] ++ ([14] ++ ([2] ++ ([5] ++ substituir_todos 1 2 ([1]))))
-- [3] ++ ([14] ++ ([2] ++ ([5] ++ ([2] ++ substituir_todos 1 2 ([ ])))))
-- [3] ++ ([14] ++ ([2] ++ ([5] ++ ([2] ++ []))))
-- [3,14,2,5,2]

-- Função polimorfica

substituir_todos2 :: Eq a => a -> a -> [a] -> [a] 
substituir_todos2 x y [] = []
substituir_todos2 x y (head:tail)
 |head == x = [y] ++ substituir_todos2 x y (tail)
 |otherwise = [head] ++ substituir_todos2 x y (tail)

-- (l) Substituir a primeira ocorrencia de um elemento x em uma lista por outro elemento y, ex:
-- > substituir_primeiro 1 2 [3 ,14 ,1 ,5 ,1]
--   [3 ,14 ,2 ,5 ,1]

substituir_primeiro :: Int -> Int -> [Int] -> [Int]
substituir_primeiro x y [ ] = [ ]
substituir_primeiro x y  (head : tail)
 |head == x = [y] ++ tail  
 |otherwise = [head] ++ substituir_primeiro x y (tail)

-- substituir_primeiro 1 2 [2,1,2,1,2,1]
-- [2] ++ substituir_primeiro 1 2 ([1,2,1,2,1])
-- [2] ++ ([2] ++ [2,1,2,1])
-- [2] ++ ([2] ++ ( [2] ++ substituir_primeiro 1 2 ([1,2,1])))
-- [2] ++ ([2] ++ ( [2] ++ ( [1] ++ substituir_primeiro 1 2 ([2,1]))))
-- [2] ++ ([2] ++ ( [2] ++ ( [1] ++ ( [2] ++ substituir_primeiro([1]))))
-- [2] ++ ([2] +  ( [2] ++ ( [1] ++ ( [2] ++ ( [1] ++ substituir_primeiro([]))))))
-- [2,2,2,1,2,1]

-- Função polimorfica

substituir_primeiro2 :: Eq a => a -> a -> [a] -> [a]
substituir_primeiro2 x y [] = []
substituir_primeiro2 x y (head:tail)
 |head == x = [y] ++ tail
 |otherwise = [head] ++ substituir_primeiro2 x y (tail)

-- (m) Determinar o produto interno de dois vetores representados por listas, ex:
-- > produto_interno [3 ,14 ,1] [5 ,9 ,26]
-- 167

produto_interno :: [Int] -> [Int] -> Int
produto_interno [ ] [ ] = 0
produto_interno [ ] (cabeca:cauda) = 0
produto_interno (head:tail) [] = 0
produto_interno (head:tail) (cabeca:cauda) = head * cabeca + produto_interno (tail) (cauda)

-- produto_interno [3 ,14 ,1] [5 ,9 ,26] 
-- 3  * 5 + produto_interno ([14,1]) ([9,26])
-- 3  * 5 + ( 14 * 9 + produto_interno([1]) ([26]))
-- 3  * 5 + ( 14 * 9 + ( 1 * 26 + produto_interno ([]) ([])))
-- 3  * 5 + ( 14 * 9 + ( 1 * 26 + 0)
-- 167

-- Função polimorfica

produto_interno2 :: Num a => [a] -> [a] -> a
produto_interno2 [] [] = 0
produto_interno2 [] (cabeca:cauda) = 0
produto_interno2 (head:tail) [] = 0
produto_interno2 (head:tail) (cabeca:cauda) = head * cabeca + produto_interno2 (tail) (cauda)

-- (n) Determinar o maior elemento de uma lista, ex:
-- > maior [3 ,1 ,4 ,1 ,5 ,14]

maior :: [Int] -> Int
maior [head] = head
maior (head:tail)
 |head > maior(tail) = head
 |otherwise = maior(tail) 
 
--  > maior [3, 1, 4, 5, 9]

-- => [3] > maior([1,4,5,9]) = True
-- => [3] > maior([4,5,9])  = False
-- => maior ([4,5,9])
-- => [4] > maior([5,9]) = False
-- => maior([5,9])
-- => [5] > maior([9]) = False
-- => maior ([9]) 
-- => 9 > maior ([]) = True 
-- => 9 > 0 = True
-- => 9 (Maior número) 

-- Função polimorfica

maior2 :: (Ord a, Num a) => [a] -> a
maior2 [head] = head
maior2 (head:tail)
 |head > maior2(tail) = head
 |otherwise = maior2(tail)

-- (o) “Desduplicar” os elementos de uma lista, ex:
-- > desduplicar [3, 3, 14, 14, 1, 1, 5, 5, 9, 9]
-- [3, 14, 1, 5, 9]

desduplicar :: [Int] -> [Int]
desduplicar [ ] = [ ]
desduplicar (head : taill) = [head] ++ desduplicar (tail taill)

-- desduplicar [3,3,14,14,1,1,5,5,9,9]
-- [3] ++ desduplicar ([3,14,1,1,5,5,9,9] [14,1,1,5,5,9,9])
-- [3] ++ ([14] ++ desduplicar ([1,1,5,5,9,9] [1,5,5,9,9]))
-- [3] ++ ([14] ++ ([1] ++ desduplicar ([5,5,9,9] [5,9,9])))
-- [3] ++ ([14] ++ ([1] ++ ([5] ++ desduplicar ([9,9] [9]))))
-- [3] ++ ([14] ++ ([1] ++ ([5] ++ ([9] ++ desduplicar ([ ] [ ])))))
-- [3] ++ ([14] ++ ([1] ++ ([5] ++ ([9] ++ [ ]))))
-- [3,14,1,5,9]

--Função polimorfica

desduplicar2 :: [a] -> [a]
desduplicar2 [] = []
desduplicar2 (head:taill) = [head] ++ desduplicar2 (tail taill)

-- (p) Escreva uma função que retorna verdadeiro se todos os elementos de uma lista de inteiros forem ímpares, ou falso, caso contrario, ex:
-- > impares [3 ,1 ,5 ,9]
-- True

impares :: [Int] -> Bool
impares [ ] = True
impares (head:tail) 
 |mod head 2 == 0 = False 
 |otherwise  = True  && impares (tail) 
 
-- impares [3,1,5,9]
-- True && impares ([1,5,9])
-- True && ( True && impares ([5,9]))
-- True && ( True && ( True && impares ([9])))
-- True && ( True && ( True && ( True && impares ([]))))
-- True && ( True && ( True && ( True && True)))
-- True

-- Função polimorfica

impares2 :: Integral a => [a] -> Bool
impares2 [] = True
impares2 (head:tail)
 |mod head 2 == 0 = False
 |otherwise = True && impares2 (tail)


--q) Inserir um elemento ordenadamente em uma lista já ordenada, ex:
-- > insere 4 [1 ,3 ,5]
-- [1 ,3 ,4 ,5]

insere :: Int -> [Int] -> [Int]
insere n [ ] = [n]
insere n (head:tail) 
 |n > head = [head] ++  insere n (tail) 
 |otherwise = [n] ++ (head:tail)
  
-- insere 4 [1 ,3 ,5]
-- [1] ++ insere 4 ([3,5])
-- [1] ++ ([3] ++ insere ([5]))
-- [1] ++ ([3] ++ ( [4] ++ ([5])))
-- [1,3,4,5]

--Função polimorfica

insere2 :: Ord a => a -> [a] -> [a]
insere2 n [] = [n]
insere2 n (head:tail)
 |n > head = [head] ++ insere2 n (tail)
 |otherwise = [n] ++ (head:tail)


-- (r) Calcular o quadrado de cada elemento da lista, ex:
-- > quadrado [3 ,14 ,1 ,5 ,9]
-- [9 ,196 ,1 ,25 ,81]

quadrado :: [Int] -> [Int]
quadrado [ ] = [ ]
quadrado (head:tail) = [head ^ 2] ++ quadrado(tail)

-- quadrado [3,14,1,5,9]
-- [3 ^ 2] ++ quadrado([14,1,5,9]
-- [3 ^ 2] ++ ([14 ^ 2] ++ quadrado([1,5,9])
-- [3 ^ 2] ++ ([14 ^ 2] ++ ([1 ^ 2] ++ quadrado ([5,9])))
-- [3 ^ 2] ++ ([14 ^ 2] ++ ([1 ^ 2] ++ ([5 ^ 2] ++ quadrado([9]))))
-- [3 ^ 2] ++ ([14 ^ 2] ++ ([1 ^ 2] ++ ([5 ^ 2] ++ ([9 ^ 2] ++ quadrado([])))))
-- [9] ++ ([196] ++ ([1] ++ ([25] ++ ([81 ++ []))))
-- [9,196,1,25,81]

-- Função polimorfica

quadrado2 :: Num a => [a] -> [a]
quadrado2 [] = []
quadrado2 (head:tail) = [head ^ 2] ++ quadrado2(tail)

-- (s) Verificar se um elemente pertece a lista, ex: 
-- > pertence 5 [3 ,14 ,1 ,5 ,9]
-- True

pertence :: Int -> [Int] -> Bool
pertence n [ ] = False
pertence n (head:tail)
 |head == n = True
 |otherwise = False || pertence n (tail)
 
-- pertence 5 [3,14,1,5,9]
-- pertence 5 ([14,1,5,9])
-- pertence 5 ([1,5,9])
-- pertence 5 ([5,9])
-- True

--Função polimorfica

pertence2  :: Eq a => a -> [a] -> Bool
pertence2 n [] = False
pertence2 n (head:tail)
 |head == n = True
 |otherwise = False || pertence2 n (tail)

-- (t) Remover todos os elementos k de uma lista, ex:

-- > remover_todos 1 [3 ,14 ,1 ,5 ,9 ,1]
-- [3 ,14 ,5 ,9]

remover_todos :: Int -> [Int] -> [Int]
remover_todos n [ ] = [ ]
remover_todos n (head:tail)
 |head == n = remover_todos n (tail)
 |otherwise = [head] ++ remover_todos n (tail)
 
-- remover_todos 1 [3,14,1,5,9,1]
-- [3] ++ remover_todos 1 ([14,1,5,9,1]
-- [3] ++ ([14] ++ remover_todos 1 ([1,5,9,1]))
-- [3] ++ ([14] ++ (remover_todos 1 ([5,9,1])))
-- [3] ++ ([14] ++ ([5] ++ remover_todos 1 ([9,1])))
-- [3] ++ ([14] ++ ([5] ++ ([9] ++ remover_todos 1 ([1]))))
-- [3] ++ ([14] ++ ([5] ++ ([9] ++ (remover_todos 1 ([])))))
-- [3] ++ ([14] ++ ([5] ++ ([9] ++ [])))
-- [3,14,5,9]

-- Função polimorfica

remover_todos2 :: Eq a => a -> [a] -> [a]
remover_todos2 n [] = []
remover_todos2 n (head:tail)
 |head == n = remover_todos2 n (tail)
 |otherwise = [head] ++ remover_todos2 n (tail)

-- (u) Dada uma lista de tuplas-2, criar uma nova lista com apenas os primeiros elementos de cada tupla, ex:
-- > primeiros [(3 ,14) ,(1 ,5) ,(9 ,1)]
-- [3 ,1 ,9]

primeiros :: [(Int, Int)] -> [Int]
primeiros [ ] = [ ] 
primeiros (head : tail) = [fst head] ++ primeiros(tail) 
 
-- primeiros [(3 ,14) ,(1 ,5) ,(9 ,1)]
-- [3] ++ primeiros([(1,5),(9,1)])
-- [3] ++ ( [1] ++ ([(9,1)])
-- [3] ++ ( [1] ++ ( [9] ++ primeiros ([]))
-- [3] ++ ( [1] ++ ( [9] ++ []))
-- [3,1,9]  

--Função polimorfica

primeiros2 :: [(a,b)] -> [a]
primeiros2 [] = []
primeiros2 (head:tail) = [fst head] ++ primeiros2(tail)

-- (v) Dada uma lista de listas, concatenar todas as sub-listas em uma unica lista, ex:
-- > concatenar [[3 ,14 ,1 ,5],[9 ,1 ,2]]
-- [3 ,14 ,1 ,5 ,9 ,1 ,2]

-- concatenção de duas listas 
concatenar2L :: [Int] -> [Int] -> [Int]
concatenar2L (head:tail) (cabeça:cauda) = concatenar_1(head:tail) ++ concatenar_2(cabeça:cauda)
concatenar2L _ _ = [ ]
 where
concatenar_1 :: [Int] -> [Int]
concatenar_1 [ ] = [ ] 
concatenar_1 (head:tail) = [head] ++ concatenar_1(tail)
 
concatenar_2 :: [Int] -> [Int]
concatenar_2 [ ] = [ ] 
concatenar_2 (cabeça:cauda) = [cabeça] ++ concatenar_2(cauda)

-- Concatenação de duas listas

concatenar_3  :: [Int] -> [Int] -> [Int]
concatenar_3 [] (cabeça:cauda) = (cabeça:cauda)
concatenar_3 (head:tail) (cabeça:cauda) = [head] ++ concatenar_3 (tail) (cabeça:cauda)

-- concatenar_3 [1,2,3] [1,2,3]
-- [1] ++ concatenar_3([2,3] [1,2,3])
-- [1] ++ ( [2] ++ concatenar_3([3] [1,2,3])
-- [1] ++ ( [2] ++ ( [3] ++ concatenar_3([] [1,2,3])))
-- [1] ++ ( [2] ++ ( [3] ++ ([1,2,3])))
-- [1,2,3,1,2,3] 


--Função polimorfica

concatenar_3p :: [a] -> [a] -> [a]
concatenar_3p [] (cabeca:cauda) = (cabeca:cauda)
concatenar_3p (head:tail) (cabeca:cauda) = [head] ++ concatenar_3p(tail) (cabeca:cauda)

-- concatenação de sub-listas

concatenar :: [[Int]] -> [Int]
concatenar [ ] = [ ]
concatenar (head:tail) =  head ++ concatenar(tail)

-- Função polimorfica

concatenar2 :: [[a]] -> [a]
concatenar2 [] = []
concatenar2 (head:tail) = head ++ concatenar2(tail)

-- concatenar [[3 ,14 ,1 ,5] ,[9 ,1 ,2]]
-- [3,14,1,5] ++ concatenar ([9,1,2])
-- [3,14,1,5] ++ ( [9,1,2] ++ concatenar([]))
-- [3,14,1,5] ++ ( [9,1,2] ++ [ ])
-- [3,14,1,5,9,1,2]

-- (w) Dadas duas listas de mesmo tamanho, obter uma terceira lista, representando a diferença absoluta entre as listas dadas, ex:
-- > diferenca [1, 3, 2, 8] [2, 5, 6, 8]
-- [1, 2, 4, 0]

diferenca :: [Int] -> [Int] -> [Int]
diferenca [] [] = []
diferenca (head:tail) (cabeça:cauda) = [cabeça - head] ++ diferenca (tail) (cauda) 

-- diferenca [1,3,2,8] [2,5,6,8]
-- [2 - 1] ++ diferenca ([3,2,8]) ([5,6,8])
-- [2 - 1] ++ ([5 - 3] ++ diferenca ([2,8]) ([6,8]))
-- [2 - 1] ++ ([5 - 3] ++ ([6 - 2] ++ diferenca ([8] ([8])))) 
-- [2 - 1] ++ ([5 - 3] ++ ([6 - 2 ] ++ ([8 - 8] ++ diferenca ([ ]) ([ ]))))
-- [1] ++ ([2] ++ ([4] ++ ([0] ++ [ ])))
-- [1,2,4,0]

diferenca2 :: Num a => [a] -> [a] -> [a]
diferenca2 [] [] = []
diferenca2 (head:tail) (cabeca:cauda) = [cabeca - head] ++ diferenca2 (tail) (cauda)

-- (x) Dadas duas listas, verificar se ambas são exatamente iguais, ou seja, se possuem o mesmo tamanho e os mesmos elementos, ex:
-- > iguais [1, 3, 2, 8] [1, 3, 2, 8]
-- True

iguais :: [Int] -> [Int] -> Bool
iguais [ ] [ ] = True
iguais (head:tail) (cabeça:cauda)
 |length (head:tail) /= length(cabeça:cauda) = False
 |head /= cabeça = False
 |head == cabeça = True && iguais (tail) (cauda)

-- iguais [1, 3, 2, 8] [1, 3, 2, 8] 
-- True && iguais ([3,2,8]) ([3,2,8])
-- True && ( True && iguais([2,8]) ([2,8]))
-- True && ( True &&( True && iguais([8]) ([8]))
-- True && ( True &&( True && ( True && iguais ([]) ([])
-- True && ( True &&( True && ( True && True)))
-- True


-- Função polimorfica

iguais2 :: Eq a => [a] -> [a] -> Bool
iguais2 [] [] = True
iguais2 (head:tail) (cabeca:cauda)
 |length (head:tail) /= length (cabeca:cauda) = False
 |head /= cabeca = False
 |head == cabeca = True && iguais2 (tail) (cauda)

-- (y) Dada uma lista de numeros reais, calcular a média aritmética dos elementos, ex:
-- > media [3.0 , 1.4 , 1.0]
-- 1.8

media :: [Float] -> Float
media [] = 0.0
media (head:tail) = (soma (head:tail)) / (fromIntegral  (length (head:tail)))
 where
soma :: [Float] -> Float
soma [ ] = 0.0
soma (head:tail) = head + soma(tail)
 
-- 3.0 + soma (1.4,1.0])
-- 3.0 + ([1.4] + soma([1.0]))
-- 3.0 + ([1.4] + ([1.0] + soma([])))
-- 3.0 + ([1.4] + ([1.0] + 0))
-- 5.4

-- media [3.0 , 1.4 , 1.0]
-- (5.4) / 3.0
-- 1.8


-- Função polimorfica

media2 :: [Float] -> Float
media2 [] = 0.0
media2 (head:tail) = (soma (head:tail))/ (fromIntegral (length(head:tail)))
 where
soma2 :: Fractional a => [a] -> a
soma2 [] = 0.0
soma2 (head:tail) = head + soma2 (tail)


-- (z) Dado um inteiro n, devolva a lista de todos os numeros inteiros ímpares entre 1 e n,ex:

lista_impares :: Int -> [Int]
lista_impares 0 = [ ]
lista_impares n
 |mod n 2 == 0 = lista_impares (n-1)
 |otherwise =  lista_impares (n-1) ++ [n]
 
--  lista_impares 9
--  lista_impares (9 -1 ) ++ [9]
--  (lista_impares(8)) ++ [9]
--  (lista_impares(8 - 1)) ++ [9]
--  (lista_impares (7)) ++ [9]
--  (lista_impares(7 - 1 ) ++ [7]) ++ [9]
--  ((lista_impares (6)) ++ [7]) ++ [9]
--  ((lista_impares(6-1)) ++ [7]) ++ [9]
--  ((lista_impares(5)) ++ [7]) ++ [9]
--  ((lista_impares(5 - 1) ++ [5]) ++ [7]) ++ [9]
--  (((lista_impares(4)) ++ [5]) ++ [7]) ++ [9]
--  (((lista_impares(4 - 1)) ++ [5]) ++ [7]) ++ [9]
--  (((lista_impares(3)) ++ [5]) ++ [7]) ++ [9]
--  (((lista_impares(3 - 1) ++ [3]) ++ [5]) ++ [7]) ++ [9]
--  ((((lista_impares(2)) ++ [3]) ++ [5]) ++ [7]) ++ [9]
--  (((lista_impares(2 - 1) ++ [3]) ++ [5]) ++ [7]) ++ [9]
--  ((((lista_impares(1)) ++ [3]) ++ [5]) ++ [7]) ++ [9]
--  ((((lista_impares(1 - 0)) ++ [1]) ++ [5]) ++ [7]) ++ [9]
--  ((((lista_impares(0)) ++ [1]) ++ [5]) ++ [7]) ++ [9]
--  ((([ ] ++ [1]) ++ [5]) ++ [7]) ++ [9]
--  [1,5,7,9]

--Função polimorfica

lista_impares2 :: Integral a => a -> [a]
lista_impares2 0 = []
lista_impares2 n
 |mod n 2 == 0 = lista_impares2 (n -1)
 |otherwise = lista_impares2 (n - 1) ++ [n]


-- 3. Implemente, usando recursão, as seguintes operações sobre conjuntos, lembrando que um conjunto é uma sequência ordenada de elementos não-repetidos:

--(a) pertence: define se um dado elemento pertence a um conjunto
pertence' :: Int -> [Int] -> Bool
pertence' n [ ] = False
pertence' n (head:tail)
 |head == n = True 
 |otherwise = False || pertence' n (tail)

--(b) uniao: dados dois conjuntos, fornece a uniao deles
uniao :: [Int] -> [Int] -> [Int]
uniao [] [] = []
uniao [] y = []
uniao x [] = []
uniao (x:xs) (y:ys)
  |x < y = [x] ++ uniao xs (y:ys)
  |x > y = [y] ++ uniao (x:xs) ys
  |otherwise = [x] ++ uniao xs ys

--(c)inter: dados dois conjuntos, fornece a intersecção deles
inter :: [Int] -> [Int] -> [Int]
inter [] y = []
inter x [] = []
inter (x:xs) (y:ys)
  |x > y = inter (x:xs) ys
  |y > x = inter xs (y:ys)
  |otherwise = [x] ++ inter xs ys

-- (d) diff: dados dois conjuntos, fornece a diferença deles
menor :: [Int] -> Int
menor [x] = x
menor (x:xs)
  |x < menor xs = x
  |otherwise = menor xs

ordena :: [Int] -> [Int]
ordena [x] = [x]
ordena x = menor x:ordena (remover_todos (menor x) x)

diff :: [Int] -> [Int] -> [Int]
diff x y = ordena (diff' z w)
  where
    z = inter x y
    w = concatenar_3 x y
    diff' :: [Int] -> [Int] -> [Int]
    diff' [x] y = remover_todos x y
    diff' (x:xs) y = diff' xs (remover_todos x y)

--(e)subc: dados dois conjuntos, diz se o primeiro é subconjunto do segundo
sub_conjunto :: [Int] -> [Int] -> Bool
sub_conjunto (x:xs) y
   |(xs == []) && (pertence' x y) = True
   |(xs == []) = False
   |pertence' x y = sub_conjunto xs y
   |otherwise = False

--(f)interc: intercalar dois conjuntos de mesmo tamanho em um terceiro conjunto
intercala :: [Int] -> [Int] -> [Int]
intercala [] y = y
intercala x [] = x
intercala (x:xs) (y:ys)
  |x /= y = ordena ([x] ++ [y] ++ intercala xs ys)
  |otherwise = ordena ([x] ++ intercala xs ys)

--4) No modulo Char encontramos a função toUpper que converte uma letra minuscula na sua correspondente maiuscula.

--(a)Crie uma função recursiva maius que converte todas as letras de uma palavra em maiusculas.

maius :: [Char] -> [Char]
maius [] = []
maius (head:tail) = [toUpper head] ++ maius(tail)

--(b)Usando a função isAlpha, tambem do módulo Char, refaça a função maius para descartar símbolos e numeros.

descarta :: [Char] -> [Char]
descarta [] = []
descarta (head:tail) 
 |isAlpha head = [toUpper head] ++ descarta(tail)
 |otherwise = descarta(tail)

--(c)Faça uma nova função que recebe uma palavra e devolve em tupla a palavra original e a sua correspondente escrita em maiuscula.

palavraMaiuscula :: [Char] -> ([Char],[Char])
palavraMaiuscula [] = ([],[])
palavraMaiuscula (head:tail)  
 |isAlpha head = ((head:tail), (maius (head:tail))) 
 |otherwise = palavraMaiuscula(tail)

--5)Considere uma lista de numeros inteiros como entrada de uma função. A função retorna verdadeiro se a lista é alternante ou falso, caso contrário.
--  Uma lista é alternante se seus dígitos se alternam entre par e ımpar.

alternante :: [Int] -> Bool
alternante [] = False
alternante (head:[]) = False
alternante (head:tail:[]) = if (((mod (head) 2 == 0) && (mod (tail) 2 == 1)) || ((mod (head) 2 == 1)  && (mod (tail) 2 == 0))) then True else False
alternante (head:tail:f) = alternante (tail:f)

--6)A representação binária de um número consiste em realizar sucessivas divisoes deste numero por 2 e imprimir do último para o primeiro, todos os restos das divisões.
converte :: Int -> [Int]
converte 0 = [0]
converte num 
 | div num 2 == 0 = [mod num 2]
 | otherwise = converte (div num 2) ++ [mod num 2]

--7) Escreva uma função que dada uma lista com 0s e 1s, representando um número binário, calcule seu correspondente na forma decimal.
decimal :: [Int] -> Int
decimal [] = 0
decimal (head:tail) = head * 2 ^ (comprimento (head:tail) - 1) + decimal(tail)

--8)Implemente uma função que tem como entrada um número inteiro e que retorna uma lista com cada dígito do numero separadamente. Dica: parte inteira e resto da divisão por 10.
separa_inteiro :: Int -> [Int]
separa_inteiro x
 | x < 10 = [x]
 |otherwise = separa_inteiro (div x 10) ++ [mod x 10]

