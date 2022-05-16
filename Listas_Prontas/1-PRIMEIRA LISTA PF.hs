-- Lista de exercícios resolvidos correspondente à atividade prática de 29/08/2018
-- Nome: Euller Henrique Bandeira Oliveira

-- 1. Mostre o resultado das seguintes execuções:  

-- > 7 `div` 2 
-- => 3 

-- > 7 `div` 2 == div 7 2 
-- => 3==3 
-- =>True 

-- > 3 > 5 
-- =>False 

-- > False == False 
-- =>True 

-- > 'a' > 'b' 
-- =>False 

-- > "abc" == "abc" 
-- => True 

-- > "elegante" < "elefante" 
-- => False 

-- > (*) ((+) 7 2) 7 
-- => (*) (7+2) 7 
-- =>(*)(9) 7 
-- =>9*7 
-- =>63 

-- > (+1) 3 
-- =>1+3 
-- =>4  

-- > (False && not True) || (True && not False) 
-- =>(False)||(True) 
-- => True 

-- 2. Defina os parênteses para as seguintes expressões  

-- > 2 ^ 3 * 4  
-- >(2^3) *4 
-- => (8) * 4 
-- =>32 

-- > 2 * 3 + 4 * 5 
-- >(2*3) + (4*5) 
-- =>(6) + (20) 
-- =>(26) 

-- > 2 + 3 * 4 ^ 5 
-- >2+(3*(4^5)) 
-- =>2+(3*(1024)) 
-- =>2+(3072) 
-- =>3074 

-- > not True || False  
-- >(not True) || (False) 
-- =>False 

-- > False || True && False 
-- >(False) || (True && False) 
-- =>(False) || (False) 
-- => False 

-- > True || False && True  
-- >(True) || (False && True) 
-- =>(True) || (False) 
-- =>True 

-- 3. Qual a diferença entre os tipos Char e String? As expressões ˜'a' e "a" representam o mesmo valor? Justifique. 

-- O tipo Char representa caracteres simples, ex: 'a', 'b', 'c' etc. Já o tipo String representa uma sequência de caracteres, ex: "Fortaleza", "Uberlândia", "Natal" etc. 
-- A expressão 'a' é um caractere, já a expressão "a" é uma String de um caractere. Portanto, representam valores diferentes. 

-- 4. Dadas as seguintes funções:  

--inc :: Int -> Int  
--inc x = x + 1 
 
--square :: Int -> Int  
--square x = x * x
 
--average :: Float -> Float -> Float  
--average a b = ( a + b ) / 2.0 

-- Mostre qual será o valor das seguintes execuções:   
-- Justifique. 

-- a)  
-- > inc (square 5)  
-- => inc ( 5*5) 
-- => 25 + 1 
-- => 26 

-- b) 
--  > square (inc 5)  
-- => square (5+1) 
-- => 6*6 
-- => 36 

-- c) 
-- average (inc 3)(inc 5) 
-- Erro 
-- Justificativa: Ocorreu um erro devido ao fato da função inc ser do tipo Int e a função average ser do tipo Float. Para a função average funcionar deve-se alterar o tipo dela para Int ou deve-se alterar o tipo da função inc para Float. 

-- 5. Quais dos seguintes nomes de funções estão corretos?  
-- Justifique.  

-- a) square_1   
-- Correta, tal função segue todas as regras para se escrever o nome de uma função. 

-- b) 1square  
-- Incorreta, pois toda função deve começar com uma letra. 

-- c) Square 
-- Incorreta, pois não se deve usar letra maiúscula no começo de uma função. 

-- d) Square!  
-- Incorreta, pois não se deve usar sinais de pontuação em uma função. 
 
-- e) square’ 
-- Incorreta, pois não se deve usar apóstrofo em uma função.  

-- 6. Faça uma função que determine a área de um retângulo. 
area_retangulo :: Float -> Float -> Float 
area_retangulo b h = b * h 

-- 7. Faça uma função que determine a área de um quadrado.  
area_quadrado :: Float -> Float 
area_quadrado l = l * l 

-- 8. Faça uma função que determine a área de um triangulo. 
area_triangulo :: Float -> Float -> Float 
area_triangulo b h = (b * h) / 2 

-- 9. Faça uma função que determine a área de um trapézio. 
area_trapezio :: Int -> Int -> Int -> Int
area_trapezio bma bme h = div ((bma + bme)* h) 2 

-- 10. Faça uma função que determine a área de um círculo. 
area_circulo :: Float -> Float 
area_circulo r = pi * r ^ 2 

-- 11. Faça uma função que determine a área da coroa circular.  
area_coroa_circular :: Float -> Float -> Float 
area_coroa_circular rma rme = pi * (rma ^ 2 - rme ^ 2) 

-- 12. Faça uma função que determine o volume de um cubo. 
volume_cubo :: Float -> Float 
volume_cubo l = l * l * l 

-- 13. Faça uma função que determine o volume de um paralelepípedo. 
volume_paralelepipedo :: Float -> Float -> Float -> Float 
volume_paralelepipedo c l h = c * l * h 

-- 14. Faça uma função que determine o volume de uma pirâmide regular.  
volume_piramide :: Float -> Float -> Float 
volume_piramide a h = (ab * h) / 3 
 where
 ab = (3 * a ^ 2 * sqrt 3) / 2 
  
volume_piramide1 :: Float -> Float -> Float
volume_piramide1 a h = 
 let 
 ab = (3 * a ^ 2 * sqrt 3) / 2 
 in 
 (ab * h) / 3 
 
-- 15. Faça uma função que determine o volume de uma esfera.  

volume_esfera :: Float -> Float 
volume_esfera r = (pi * 4 * r ^ 3) / 3 

-- 16. Faça uma função que determine o valor da hipotenusa de um triângulo retângulo, onde são fornecidos os seus outros lados.  

hipotenusa :: Float -> Float -> Float 
hipotenusa b c = sqrt ((b*b) + ( c*c)) 

-- 17. Dado um ponto (x, y), implemente uma função que determine a distância desse ponto à origem (0, 0). 

d_AB1 :: Float -> Float -> Float  
d_AB1 xb yb = sqrt (( xb - 0) ^ 2 + (yb - 0) ^ 2) 

-- 18. Dados dois pontos (xa, ya) e (xb, yb), implemente uma função que determine a distância entre esses pontos. 

d_AB2 :: Float -> Float -> Float -> Float -> Float 
d_AB2 xa ya xb yb = sqrt (( xb - xa) ^ 2 + (yb - ya) ^ 2) 

-- 19. Faça uma função que determine o quadrado de um número.  

quadrado_de_um_numero :: Int -> Int 
quadrado_de_um_numero x = x ^ 2 

--20. Faça uma função que determine o cubo de um número.  

cubo_de_um_numero :: Int -> Int 
cubo_de_um_numero x = x ^ 3 

-- 21. Faça uma função que determine a quarta potência de um número, usando a função que determina o quadrado de um número. 
quadrado_de_um_numero_1 :: Int -> Int 
quadrado_de_um_numero_1 x = (x ^ 2) 

quarta_potencia_de_um_numero_1 :: Int -> Int 
quarta_potencia_de_um_numero_1 y = quadrado_de_um_numero_1 ( quadrado_de_um_numero_1 y)


quarta_potencia_de_um_numero_2 :: Int -> Int 
quarta_potencia_de_um_numero_2 x = quadrado_de_um_numero_2 * quadrado_de_um_numero_2
 where
 quadrado_de_um_numero_2 = (x ^ 2) 

 
quarta_potencia_de_um_numero_3 :: Int -> Int
quarta_potencia_de_um_numero_3 x =
 let
 quadrado_de_um_numero_2 = (x ^ 2)
 in
 quadrado_de_um_numero_2 * quadrado_de_um_numero_2

-- 22. Faça uma função que, dado um total de segundos, calcule o total de horas. 
horas :: Int -> Int 
horas s = div s 3600

-- 23. Faça uma função que, dado um total de segundos, calcule o total de minutos. 
minutos :: Int -> Int 
minutos s = div s 60 

-- 24.Dada uma temperatura em graus Fahrenheit, converta-a em graus Celsius.  
celcius_1 :: Float -> Float 
celcius_1 f = (f - 32) / 1.8 

-- 25. Dada uma temperatura em graus Kelvin, converta-a em graus Celsius  
celcius_2 :: Float -> Float 
celcius_2 k = k - 273 

-- 26.Dada uma temperatura em graus Fahrenheit, converta-a em graus Kelvin. 
celcius_3 :: Float-> Float  
celcius_3 f = (f - 32) / 1.8  

kelvin_1 :: Float -> Float 
kelvin_1 f = celcius_3 f + 273 

kelvin_2 :: Float -> Float 
kelvin_2 f = celcius_4 + 273 
 where
 celcius_4 = (f-32)/1.8
 
kelvin_3 :: Float-> Float  
kelvin_3 f =
 let  
 celcius_5 = (f-32)/ 1.8
 in
 celcius_5 + 273

-- 27. Faça uma função que, dada uma velocidade em quilômetros por hora, converta-a em  metros por segundo. 
ms:: Float -> Float 
ms kmh = kmh / 3.6 

-- 28. Dados dois valores lógicos, faça uma função que implemente a fórmula:  (p∨q)∧¬(p∧q). 
f1 :: Bool -> Bool -> Bool 
f1 p q = (p || q) && not(p && q) 

-- 29. Dados três valores lógicos, faça uma função que implemente a fórmula: (p ∨ q) ∧ r 
f2 :: Bool -> Bool -> Bool -> Bool 
f2 p q r = (p || q) && r 

-- 30. Dados três valores lógicos, faça uma função que que implemente a fórmula: (p∧q)∨¬(p∧r). 
f3 :: Bool -> Bool -> Bool -> Bool 
f3 p q r = (p && q) || not (p && r) 

-- 31. Dados quatro valores lógicos, faça uma função que implemente a fórmula: p∨(q∧r)∨¬s.  
f4 :: Bool -> Bool -> Bool -> Bool -> Bool 
f4 p q r s = (p || (q && r)) || not s 

-- 32. Dados quatro valores lógicos, faça uma função que implemente a fórmula: ¬(p∨q)∧(r ∨ s) ∧ ¬r. 
f5 :: Bool -> Bool -> Bool -> Bool -> Bool 
f5 p q r s = (not(p || q) && (r || s)) && not r 

