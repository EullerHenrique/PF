-- Lista de exercícios resolvidos correspondente à atividade prática de 06/09/2018
-- Nome: Euller Henrique Bandeira Oliveira

-- 1. Considere as duas listas a seguir: 
-- ls1 = [3 ,1 ,4 ,1 ,5]  
-- ls2 = [1 ,6 ,1 ,8 ,0]  

--Mostre a saída das seguintes expressões, quando corretamente executadas. 
--Explique o que acontece quando houver erro.  

--a)ls1 > (tail ls2)  
--  =>[3.1,4,1,5] > [6,1,8,0] 
--  =>False 
-- 3.1 não é maior do que 6 

--b)(tail ls1) ++ ls2  
-- =>[1,4,1,5] ++ [1,6,1,8,0] 
-- =>[1,4,1,5,1,6,1,8,0] 
 
--c) head (tail(tail ls1)) 
-- =>head(tail [1,4,1,5]) 
-- =>head [4,1,5] 
-- =>4 

-- d)fst (head ls1 , tail ls2) 
-- => fst ( 3, [6,1,8,0]) 
-- => 3 

-- e)3 ++ ls1 
-- Erro, listas somente podem ser somadas com outras listas. 

-- f)snd (head ls1 , tail ls2) 
-- =>snd  ( 3, [6,1,8,0] 
-- =>[6,1,8,0] 

-- g)"1,2,3" ++ ls1 
-- Erro, listas somente podem ser somadas com outras listas. 

-- h)(head (tail ls2), tail ls1) 
-- =>(head [6,1,8,0], [1,4,1,5]) 
-- =>(6, [1,4,1,5]) 

-- i)head ls1 ++ ls1 
-- => 3 ++ ls1 
-- => Erro, listas somente podem ser somadas com outras listas. 

-- j)ls2 ++ [1..7] 
-- => [1,6,1,8,0] ++ [ 1,2,3,4,5,6,7] 
-- =>[1,6,1,8,0,1,2,3,4,5,6,7] 

-- k)[9] ++ ls1 ++ ls2 
-- =>[9] ++ [3,1,4,1,5] ++ [1,6,1,8,0] 
-- =>[9,3,1,4,1,5,1,6,1,8,0] 

-- 2. Usando as funções head e tail, defina a função terceiro que devolve o terceiro elemento de uma lista de inteiros.  
terceiro :: [Int] -> Int 
terceiro l = head (tail(tail l)) 

-- ex: > terceiro [1,2,3,4,5] 
--    => head (tail(tail l)
--    => head (tail([2,3,4,5])
--    => head ([3,4,5])
--    => 3

-- 3. Considere a função reverse do preludio-padrão: 
-- > reverse [1 ,2 ,3]  
-- => [3 ,2 ,1] 
-- Utilizando essa função (além das funções head e tail, crie as seguintes funções:  

-- a) Função último, que devolve o último elemento de uma string.  
ultimo :: [Char] -> Char 
ultimo h =  head (reverse h)

ultimo_2 :: String -> Char
ultimo_2 h = head (reverse h)
 
 -- Exemplo:
--  > ultimo " haskell "
-- => "llekash"
-- => ’l’

--(b) Função inicio, que devolve todos os elementos da string, exceto o ultimo.
inicio :: [Char] -> [Char] 
inicio h =  reverse (tail (reverse h)) 

inicio_2 :: String -> String
inicio_2 h = reverse (tail (reverse h))

-- Exemplo:
--  > inicio " haskell "
-- => "llekash"
-- => "lekash"
-- => "haskel"

-- 4.Implemente uma função que receba o primeiro e o último nome de alguém e retorne suas iniciais em uma tupla.
iniciais :: ([Char], [Char])  -> (Char, Char) 
iniciais (a, b) = (head a, head b) 

-- ex: iniciais ("Euller", "Henrique") = ('E','H')
    
-- 5. O operador : chamado construtor, permite construir uma lista a partir de um conjunto de elementos.  
-- Execute as seguintes expressões e tente entender o que está sendo feito. 

-- > 1:[2 ,3 ,4] 
-- => [1,2,3,4] 

-- > 1:2:3:4:[] 
-- =>[1,2,3,4] 

-- > [1 ,2 ,3]:[4..7] 
-- Erro, não pode-se usar o construtor para construir uma lista a partir de duas listas.

-- > 1:[ 'a','b'] 
-- Erro, listas só podem ter tipos iguais.

-- > "a":" bCc " 
-- Erro, não pode-se usar o construtor para construir uma lista a partir de duas listas.

-- > 'a' : 'b' 
-- Erro, não é póssivel contruir uma lista, pois não há nenhuma lista presente.

-- > 'a' :"b" 
-- "aa"

--  > [1, 4, 7] ++ 4:[5:[]]  
-- [1,4,7] ++ 4:[[5]]
-- [1,4,7] ++ [4,[5]]
-- Erro,  listas só podem ter tipos iguais.

-- > [True , True :[]] 
-- [True,[True]]
-- Erro,  listas só podem ter tipos iguais.

-- > True :[ True , False ] 
-- => [True, True, False]
