-- Nome: Euller Henrique Bandeira Oliveira
-- Matricula: 11821BSI210

-- 1. Implemente o cálculo do mínimo múltiplo comum (MMC) de dois números:
-- Main > mmc (2, 3)
-- 6
-- Utilize, para isso, a função mdc vista em aula. Sabe-se que:
-- mmc (a,b)= a.b/mdc(a,b)

mdc ::(Int, Int) -> Int
mdc (a,b)
 |b == 0 = a
 |otherwise = mdc (b, (mod a b))

-- mdc (2,3)
-- mdc (b,(mod a b))
-- mdc (3,(mod 2 3))
-- mdc (3,(2))
-- mdc (b,(mod a b))
-- mdc (2,(mod 3 2))
-- mdc (2,1))
-- mdc (b,(mod a b))
-- mdc (1,(mod 2 1))
-- mdc (1, 0)
-- 1
 
mmc :: (Int, Int) -> Int
mmc (a,b) = div (a * b) (mdc(a,b))

-- mmc (2,3)
-- div (2*3) (1)
-- div (6) (1)
-- 6

-- 2. Pesquise e implemente um metodo recursivo para calcular o MDC de três números. Utilize a função mdc vista em aula. Sabe-se que:
-- mdc(a,b,c) = mdc(mdc(a,b),c)

mdc_1 ::(Int, Int) -> Int
mdc_1 (a,b)
 |b == 0 = a
 |otherwise = mdc_1 (b, (mod a b))

-- mdc_1 (2,3)
-- mdc_1 (b,(mod a b))
-- mdc_1 (3,(mod 2 3))
-- mdc_1 (3,(2))
-- mdc_1 (b,(mod a b))
-- mdc_1 (2,(mod 3 2))
-- mdc_1 (2,1))
-- mdc_1 (b,(mod a b))
-- mdc_1 (1,(mod 2 1))
-- mdc_1 (1, 0)
-- 1
 
-- mdc_1 (1,4)
-- mdc_1 (b, (mod a b))
-- mdc_1 (4, (mod 1 4))
-- mdc_1 (4,1)
-- mdc_1 (b,(mod a b))
-- mdc_1 (1, (mod 4 1))
-- mdc_1 (1, 0)
-- 1

mdc3 ::(Int, Int, Int) -> Int
mdc3 (a,b,c) = mdc_1(mdc_1(a,b),c)

-- mdc3 (2,3,4)
-- mdc_1(mdc_1(a,b),c)
-- mdc_1(mdc_1(2,3),4)
-- mdc_1(1,4)
--1 

-- 3. Implemente uma função recursiva somaN :: Int ­> Int que computa a soma dos números de 1 a n.
somaN :: Int -> Int
somaN n
 |n == 0 = 0
 |otherwise =  n + somaN (n-1)

-- somaN 6
-- 6 + somaN(6-1)
-- 6 + somaN (5)
-- 6 + ( 5 + somaN (5-1))
-- 6 + ( 5 + somaN (4))
-- 6 + ( 5 + ( 4 + somaN(4 - 1)))
-- 6 + ( 5 + ( 4 + somaN(3)))
-- 6 + ( 5 + ( 4 + ( 3 + somaN(3 -1))))
-- 6 + ( 5 + ( 4 + ( 3 + somaN(2))))
-- 6 + ( 5 + ( 4 + ( 3 + ( 2 + somaN(2 - 1)))))
-- 6 + ( 5 + ( 4 + ( 3 + ( 2 + somaN(1)))))
-- 6 + ( 5 + ( 4 + ( 3 + ( 2 + ( 1 + somaN( 1-0))))))
-- 6 + ( 5 + ( 4 + ( 3 + ( 2 + ( 1 + somaN(0))))))
-- 6 + ( 5 + ( 4 + ( 3 + ( 2 + ( 1 + 0)))))
-- 21

-- 4. Implemente uma função recursiva para calcular a soma entre dois números n1 e n2 incluindo os limites
-- Em seguida reimplemente essa função para excluir os limites.

-- COM LIMITES

soma_n1_n2 :: (Int, Int) -> Int
soma_n1_n2 (n1, n2)
 |n1 > n2 = 0
 |otherwise = n1 + soma_n1_n2 ((n1 + 1), n2)
 
-- soma_n1_n2 (4,6)
-- n1 + soma_n1_n2 (( n1 + 1), n2)
-- 4 + soma_n1_n2 (( 4 + 1), 6))
-- 4 + soma_n1_n2 (5,6))
-- 4 + ( 5 + soma_n1_n2((5 + 1),6))
-- 4 + ( 5 + soma_n1_n2 (6,6))
-- 4 + ( 5 + ( 6 + soma_n1_n2 ((6 + 1),6)
-- 4 + ( 5 + ( 6 + soma_n1_n2((7,6))
-- 4 + ( 5 + ( 6 + 0))
-- 15 
 
-- SEM LIMITES 
soma_n1_n2s :: (Int, Int) -> Int
soma_n1_n2s (n1, n2) = soma_n1_n2(n1,n2) - n1 - n2
 

-- 5. Implemente uma função recursiva que, dados dois números n1 e n2, 
--    encontra os multiplos de um terceiro numero n3 que se encontram nesse intervalo. 

multiplos_n3 :: Int -> Int -> Int -> [Int]
multiplos_n3 n1 n2 n3 
 |n1 > n2 = [ ] 
 |n3 > n2 = error " n3 não pertence a este intervalo"
 |mod n1 n3 == 0 = [n1] ++ multiplos_n3 (n1 + 1) n2 n3
 |otherwise = multiplos_n3 (n1 + 1) n2 n3

-- 6. Implemente uma função recursiva que calcule o número de grupos distintos com k pessoas 
--    que  podem ser formados a partir de um conjunto de n pessoas 
--   (ou seja, a combinação de n pessoas em grupos de k)

--   comb = número de grupos 
--   n = número total de pessoas
--   k = grupo com k pessoas   

-- A definição abaixo da função comb(n, k) define as regras:
--   comb(n , k )= n se k=1
--   comb(n, k )= 1 se k=n
--   comb(n , k )= comb(n−1, k−1)+comb(n−1, k ) se 1<k<n

comb :: (Int, Int) -> Int
comb (n, k) 
 |k == 1 = n
 |k == n = 1
 |1 < k && k < n = comb(n-1, k -1) + comb(n-1, k)
 |otherwise = error " N deve ser maior que K"

-- 7)7. Seja a função exp x definida por uma serie de Taylor:
-- Implemente o calculo recursivo da soma da série para n = 10 termos e teste para varios valores de x.
-- Compare os resultados obtidos com o valor dado pela função exp x do preludio-padrão.

fatorial :: Int -> Int
fatorial n
 |n < 0 = error "n deve ser maior do que 0"
 |n == 0 = 1
 |n > 0 = n * fatorial(n - 1)
 
taylor :: Float -> Int -> Float
taylor x n 
 |n == 0 = 1.0
 |otherwise = (x ^ n) / (fromIntegral (fatorial n)) + (taylor x (n - 1)) 
 
-- 8. Escreva uma função recursiva conta_digitos que recebe um numero inteiro n e retorna sua quantidade de dígitos. Exemplo: se n = 132, conta_digitos n retorna 3.
conta_digitos :: Int -> Int
conta_digitos n
 |n < 10 = 1 
 |otherwise = 1 + conta_digitos (div n 10)
 
--conta_digitos 180
-- 1 + conta_digitos(div 180 10)
-- 1 + conta_digitos(18)
-- 1 + ( 1 + conta_digitos(div 18 10))
-- 1 + ( 1 + conta_digitos(1))
-- 1 + ( 1 + ( 1 + conta_digitos ( 1 / 10)
-- 1 + ( 1 + ( 1 + conta_digitos (0)))
-- 1 + ( 1 + ( 1 + 0))
-- 3

-- 9. Escreva uma função recursiva soma_digitos que recebe um numero inteiro n e retorna a soma de seus dígitos. Exemplo: se n = 132, soma_digitos n retorna 6.
soma_digitos :: Int -> Int
soma_digitos n
 |n < 10 = 1
 |otherwise = mod n 10 + soma_digitos(div n 10)

-- soma_digitos 24
-- (mod n 10) + soma_digitos (div n 10)
-- (mod 24 10) + soma_digitos(div 24 10)
-- 4 + soma_digitos(2)
-- 4 + ((mod 2 10) + soma_digitos(div 2 10))
-- 4 + (2 + soma_digitos(0))
-- 4 + ( 2 + 0)
-- 6

-- 10. Implemente a função recursiva potencia (b, e) :: (Int, Int) ­> Int que eleva a base b ao expoente e.
potencia :: (Int, Int) -> Int
potencia (b,e) 
 |e <= 0 = 1 
 |otherwise = b * potencia (b, ( e - 1))
 
--potencia(2,4)
-- b * potencia(b, e - 1)
-- 2 * potencia(2, 4 - 1)
-- 2 * potencia(2, 3)
-- 2 * ( 2 * potencia(2,3-1))
-- 2 * ( 2 * potencia(2,2))
-- 2 * ( 2 * ( 2 * potencia (2,2-1)))
-- 2 * ( 2 * ( 2 * potencia (2,1)))
-- 2 * ( 2 * ( 2 * ( 2 * potencia (2, 1-1))))
-- 2 * ( 2 * ( 2 * ( 2 * potencia (2,0))))
-- 16


-- 11. Implemente a função de Ackermann, a qual é definida por:
-- A(m ,n)= n+1 se m=0
-- A(m ,n)= A(m−1,1) se m>0 e n=0
-- A(m,n)= A(m,n−1) se m>0 e n>0

-- Observação: Teste essa função com valores pequenos (em torno de 0 a 3).

ackermann :: (Integer, Integer) -> Integer
ackermann (m,n)
 |m == 0 = n + 1
 |m > 0 && n == 0 = ackermann (m - 1, 1)
 |m > 0 && n > 0  = ackermann (m, n - 1)
 


-- 12. Desconsidere o conhecimento da função sqrt na linguagem Haskell. Uma forma de se obter a
-- raiz quadrada de um numero qualquer x seria através de busca binária:
-- • Assuma que a raiz quadrada de x esta entre 1 (início) e x (fim), se x ≥ 1;
-- • Assuma que a raiz quadrada de x esta entre x (início) e 1 (fim), se x < 1;
-- • Se o número for negativo, retorne 0.
-- Para sabermos se um palpite y = (inicio+fim)/2 é a raiz quadrada de x, basta testar
-- se y * y é próximo o suficiente de x ou, em outras palavras, se o módulo da diferença entre x e y
-- * y está dentro de uma tolerância definida. Caso contrário, podemos restringir a busca entre início
-- e y ou entre y e fim. Escreva a função que implemente este algoritmo, considerando 10−6 como
-- tolerância para o cálculo do resultado

raiz :: Float -> Float -> Float -> Float -> Float
raiz x inicio fim tol
  | abs(y * y - x) <= tol = y
  | (y > inicio) && (y < fim) = raiz x y fim tol
  | otherwise = raiz x inicio y tol
 where 
 tol = 0.000001
 y = (inicio + fim) / 2 


raiz_quadrada :: Float -> Float 
raiz_quadrada x
 | x < 0 = 0
 | x >= 1 = raiz x 1 x tol 
 where 
 tol = 0.000001



















