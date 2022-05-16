-- Lista de exercícios resolvidos correspondente à atividade prática de 12/09/2018
-- Nome: Euller Henrique Bandeira Oliveira

-- 1.Fornecidos três valores, a, b e c em uma tupla, implemente uma função que retorne quantos desses três são iguais.
-- A reposta deve ser 3, se todos são iguais;
-- 2, se dois são iguais e um e distinto dos demais ou 0, se todos são distintos entre si.

quantos_iguais :: (Int, Int, Int) -> Int  
quantos_iguais (a,b,c)  
    |a==b && b==c = 3  
    |a==b || b==c || a==c = 2  
    |otherwise = 0 

-- 2.Faça uma função que recebe dois valores reais em uma tupla e devolve o menor.  
menor_2 ::(Float, Float) -> Float 
menor_2 (n1,n2) 
  | n1 < n2 = n1 
  | otherwise = n2 

-- 3.Faça uma função que recebe três valores reais em uma tupla e devolve o menor.  
menor2 :: Float -> Float -> Float 
menor2 n1 n2 
  | n1 < n2  = n1 
  | otherwise = n2 
  
menor3 ::(Float, Float, Float) -> Float
menor3 (x, y, z) = menor2 (menor2 x y) z

menorr_3 :: (Float, Float, Float) -> Float
menorr_3(x, y, z) =
 let 
  menor_2 n1 n2 
   | n1 < n2  = n1 
   | otherwise = n2 
 in
  menor2 (menor2 x y) z
 
-- 4. Escreva uma função que receba uma tupla-3 de três inteiros e retorne uma tupla-2 com o maior e o menor elemento dentre os três.  

menor_3 :: (Int, Int, Int) -> Int 
menor_3 (n1, n2, n3) 
  |(n1<n2) && (n1<n3) = n1 
  |(n2<n3) = n2 
  |otherwise = n3 

maior_3 :: (Int, Int, Int) -> Int 
maior_3 (n1,n2,n3) 
  |(n1>n2) && (n1>n3) = n1 
  |(n2>n3) = n2 
  |otherwise = n3 
  
maior_menor_3 :: (Int, Int, Int) -> (Int, Int) 
maior_menor_3 (x, y, z) = (maior_3 (x, y, z), menor_3 (x, y, z)) 

maior_menorr_3 :: (Int, Int, Int) -> (Int, Int) 
maior_menorr_3 (x, y, z) =
 let
 maiorr_3 n1 n2 n3 
  |n1>n2 && n1>n3 = n1 
  |n2>n3 = n2 
  |otherwise = n3 
 menorr_3 n1 n2 n3 
  |n1<n2 && n1<n3 = n1 
  |n2<n3 = n2 
  |otherwise = n3 
  in
  (maiorr_3 x y z, menorr_3 x y z)
   
-- 5. Seja a seguinte equação do segundo grau: ax2 + bx + c = 0 sendo que a, b e c sao numeros reais e a≠0.  
-- Essa equação tem: 
-- • duas raízes reais, se b2 > 4ac;  
-- • uma raiz real, se b2 = 4ac;  
-- • nenhuma raiz real, se b2 < 4ac. 
-- Faça uma função que, dados três coeficientes a, b, e c, informe quantas raízes a equação possui.  

segundo_grau_1 ::(Float, Float, Float) -> Float 
segundo_grau_1 (a,b,c) 
 |b^2 > 4 * a * c = 2 
 |b^2 == 4 * a * c = 1 
 |otherwise = 0 

segundo_grau_2 ::(Float, Float, Float) -> String 
segundo_grau_2 (a,b,c) 
 |b^2  > 4 * a *c = "duas raizes " 
 |b^2 == 4 * a *c = " uma raiz "
 |otherwise =  " nenhuma raiz "
 
segundo_grau_3 ::(Float, Float, Float) -> String 
segundo_grau_3 (a, b, c) 
 |b1 > c1 = " duas raizes " 
 |b1 == c1 = " uma raiz " 
 |otherwise = " nenhuma raiz "
 where  
 b1= b^2 
 c1= 4 * a  * c 

segundo_grau_4 ::(Float, Float, Float) -> String 
segundo_grau_4 (a, b, c) = 
 let 
  x = b ^2 
  y = 4 * a * c  
  comparacao a1 b1  
   |a1 > b1 = "duas raizes" 
   |a1 < b1 = "uma raiz" 
   |otherwise = "nehuma raiz real" 
 in  
   comparacao x y  
   
-- 6. Faça uma função que, dado duas datas como entrada, determine qual delas ocorreu cronologicamente antes em relação a outra. 
-- Cada data é composta por um tupla de 3 números inteiros: ano, mês e dia.  
-- Saídas possíveis: ”Primeira data ocorreu antes da segunda”ou ”Segunda data ocorreu antes da Primeira”.  
cronologia_de_datas :: (Int,Int,Int) -> (Int,Int,Int) -> String 
cronologia_de_datas (a1,m1,d1) (a2,m2,d2)  
 | a1 > a2 = "Segunda data ocorreu antes da primeira" 
 | a1 < a2 = "Primeira data ocorreu antes da segunda" 
 | m1 > m2 = "Segunda data ocorreu antes da primeira" 
 | m1 < m2 = "Primeira data ocorreu antes da segunda" 
 | d1 > d2 = "Segunda data ocorreu antes da primeira" 
 | d1 < d2 = "Primeira data ocorreu antes da segunda" 
 |otherwise = "As duas datas ocorreram ao mesmo tempo"  
      
-- 7. Faça uma função chamada ordena_2 :: Int -> Int -> (Int, Int) que aceita dois valores inteiros como argumentos e retorna-os como um par ordenado. 
-- Por exemplo, ordena_2 5 3 é igual a (3,5). 
-- Defina essa função utilizando Guardas.   

ordena_2 :: Int -> Int -> (Int, Int) 
ordena_2 x y 
 | x > y = (y,x) 
 | otherwise = (x,y) 

-- 8. Faça a função par que recebe um número inteiro e devolve verdadeiro se o número for par e falso, caso contrário. Não se esqueça das definições de tipos.  
par :: Int -> Bool 
par x 
 |(mod x 2) == 0 = True 
 | otherwise = False 
 
-- 9. Utilizando a função do item anterior, faça a função impar que recebe um número inteiro e devolve verdadeiro se o número for ímpar e falso, caso contrario. 
impar :: Int -> Bool 
impar y  
 | par y = False
 | otherwise = True
 
 
-- 10. Defina uma função para calcular as raízes reais do polinômio ax ^ 2 + bx + c.
-- Faça uso de uma definição local com where para calcular o discriminante. Teste suas funções no GHCi.  
-- Dica: Use a função error::String->a do prelúdio, que exibe uma mensagem de erro e termina o programa, para exibir uma mensagem quando não houver raízes reais. 

raizes :: Float -> Float -> Float -> (Float, Float)  
raizes a b c   
 | d > 0 = ((((-b) + sqrt d) /  2 * a),(((-b) - sqrt d)/ 2 * a))
 | d == 0 = (((-b)/ 2 * a), ((-b)/ 2 * a)) 
 | otherwise = error "não tem raizes reais" 
 where  
  d = (b^2) - 4 * a* c  
  
  
raizes_1 :: Float -> Float -> Float -> (Float, Float)
raizes_1 a b c =
 let
  d = (b^2) - 4 * a * c  
  zero = 0
  comparacao a1 b1
   | a1 > b1 = ((((-b) + sqrt d) /  2 * a),(((-b) - sqrt d)/ 2 * a))
   | a1 == b1 = (((-b)/ 2 * a), ((-b)/ 2 * a)) 
   | otherwise = error "não tem raizes reais" 
  in
  comparacao d zero