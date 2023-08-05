--1.1 dado el lado de un cuadrado devolver el area 
areaCuadrado (lado) = lado * lado
--1.2 y 1.3 daod la base y altura de un rectangulo devolvemos el area y perimetro 
areaYPerimetroRectangulo :: Num b => (b, b) -> (b, b)
areaYPerimetroRectangulo (base, altura) = (base * altura, 2 * altura + 2 * base)
--1.4 dado 2 numeros devolver si es verdad que el primero es mayor que el segundo
mayorElPrimero :: (Int, Int) -> Bool 
mayorElPrimero (num1, num2) = if (num1 > num2) then True else False
--1.5 dado un numero que devuelva si es par o no
multiploDe2 :: (Int) -> Bool
multiploDe2 (num) = if (num `mod` 2 == 0) then True else False
--1.6 numero si es multiplo de 2 y 3 al mismo tiempo
multiploDe2y3 :: (Int) -> Bool
multiploDe2y3 (num) = if (num `mod` 2 == 0 && num `mod` 3 == 0) then True else False
--1.7 ^ cualquier num a una potencia entera no negativa, ^^ fraccion a cualquier potencia entera, ** flotantes
elevarA3 (num) = num ^ 3
--1.8 elevar a cierta potencia
elevarA4 (num) = num ^ 4
elevarA8 (num) = num ^ 8
elevarA10 (num) = num ^ 10
elevarA32 (num) = num ^ 32
--1.9 que 2 numeros obedescan una orden de una funcion
fun9 num1 num2 f = f num1 num2 
-- 2.1 el mayor de 2 numeros
mayorDe2 a b = if (a > b) then a else b
-- 2.2 mayor de 3 numeros
mayorDe3 num1 num2 num3 = if num1 > num2 && num1 > num3 then num1 else if num2 > num3 then num2 else num3
--2.3 mayor de 4 numeros
mayorDe4 n1 n2 n3 n4 = if n1 > n2 && n1 > n3 && n1 > n4 then n1 else if n2 > n3 && n2 > n3 then n2 else if n3 > n4 then n3 else n4
--2.10 dado 4 notas determinar si aprobo abandono o reprobo
nota :: Int -> Int -> Int -> Int -> String
nota pp sp ef si = if ((pp + sp) `div` 2) > 50 then "Aprobado" else if ef > 50 then "Aprobado" else if si > 50 then "Aprobado" else if pp == 0 && sp == 0 && ef == 0 && si == 0 then "Abandono" else "Reprobado"
--2.11 recibo 2 fechas y devuelvo el mayor "en listas"
fechaMayor :: [Int] -> [Int] -> [Int]
fechaMayor f1 f2 = if last f1 > last f2 then f1 else if last f2 > last f1 then f2 else if f1 !! 1 > f2 !! 1 then f1 else if f1 !! 1 < f2 !! 1 then f2 else if head f1 > head f2 then f1 else if head f1 < head f2 then f2 else f1
--3.1.2 4 numeros y devuelve el mayor por distincion de casos
devolverMayor a b c d   |a > mayorDe3 b c d = a |b > mayorDe2 c d = b |c > d = c |d > c = d  
--3.1.1 4 numeros y devuelve el mayor por combinacion
devolverMayorCombinacion a b c d = mayorDe2 (mayorDe2 a b) (mayorDe2 c d) 
--3.2 dada una nota decir si aprobo o no
fun3_2 a | a > 50 = "Aprobado"
         | otherwise = "Reprobado"
--3.3 dada una nota determinar si esta excelente, bien, regular, mal
fun3_3 :: Int -> String 
fun3_3 a | a > 89 = "Excelente" | a >= 70 = "Bien" | a >= 51 = "Regular" | otherwise = "Mal"
--3.4 dada notas definir el estado de su nota
fun3_4 :: String -> String -> String -> String -> String
fun3_4 pp sp ef si |(((read pp ::Int) + (read sp ::Int)) `div` 2) >= 51 = "Aprobado" | (read ef ::Int) >= 51 = "Aprobado" | (read si ::Int) >= 51 = "Aprobado" | otherwise = "Reprobado"  
--3.5 reciba 16 numeros y retorne el mayor
fun3_5 a b c d e f g h i j k l m n o p = mayorDe4 (mayorDe4 a b c d)(mayorDe4 e f g h )(mayorDe4 i j k l)(mayorDe4 m n o p) 
--3.6 dado un quebrado decir si es mayor a uno o no
fun3_6 :: Int -> Int -> Bool 
fun3_6 a b | a > b = True |otherwise = False
--3.7 2 fechas devuelve el mayor "tuplas"
fun3_7 :: (Int,Int,Int) -> (Int,Int,Int) -> (Int,Int,Int)
fun3_7 (d1, m1, a1) (d2, m2, a2) | a1 > a2 = (d1, m1, a1) | a2 > a1 = (d2, m2, a2) |a1 == a2 && m1 > m2 = (d1, m1, a1) |a1 == a2 && m2 > m1 = (d2, m2, a2) |a1 == a2 && m1 == m2 && d1 > d2 = (d1, m1, a1) |a1 == a2 && m1 == m2 && d1 < d2 = (d2, m2, a2) |otherwise = (d2, m2, a2) 
--fun3.8 2 fechas devuelve los años trancurridos 
fun3_8 :: (Int,Int,Int) -> (Int,Int,Int) -> Int
fun3_8 (d1, m1, a1) (d2, m2, a2) | a1 > a2 = a1 - a2 | a2 > a1 = a2 - a1
--fun3.9 2 fechas devuelve los meses transcurridos
fun3_9 :: (Int,Int,Int) -> (Int,Int,Int) -> Int
fun3_9 (d1, m1, a1) (d2, m2, a2) | fun3_7 (d1, m1, a1) (d2, m2, a2) == (d1, m1, a1) = (fun3_8 (d1, m1, a1) (d2, m2, a2)) * 12 + (m1 - m2) | fun3_7 (d1, m1, a1) (d2, m2, a2) == (d2, m2, a2) = (fun3_8 (d1, m1, a1) (d2, m2, a2)) * 12 + (m2 - m1)
--fun3.10 2 fechas devuelve los dias transcurridos 
fun3_10 :: (Int,Int,Int) -> (Int,Int,Int) -> Int
fun3_10 (d1, m1, a1) (d2, m2, a2) | fun3_7 (d1, m1, a1) (d2, m2, a2) == (d1, m1, a1) = (fun3_9 (d1, m1, a1) (d2, m2, a2))* 30 + (d1 - d2) | fun3_7 (d1, m1, a1) (d2, m2, a2) == (d2, m2, a2) = (fun3_9 (d1, m1, a1) (d2, m2, a2))* 30 + (d2 - d1)
--fun3.11 2 fechas devuelve los dias, meses y años trancurridos
fun3_11 :: (Int,Int,Int) -> (Int,Int,Int) -> (Int,Int,Int)
fun3_11 (d1, m1, a1) (d2, m2, a2) |fun3_7 (d1, m1, a1) (d2, m2, a2) == (d1, m1, a1) = (d1-d2, m1 - m2, a1 - a2)|fun3_7 (d1, m1, a1) (d2, m2, a2) == (d2, m2, a2) = (d2-d1, m2 - m1, a2 - a1)  
--fun3.12 dado un tiempo aumentamos un segundo
fun3_12 :: ((Int,Int,Int),Int) -> ((Int,Int,Int),Int,Int,Int)
fun3_12 ((dia,mes,año),hora) = ((dia,mes,año),hora, 0 , 1)
--fun4.1 dada una vocal devolvera la siguiente 
fun4_1 :: Char -> Char
fun4_1 v = 
 case v of 
 'a' -> 'e' 
 'e' -> 'i' 
 'i' -> 'o' 
 'o' -> 'u' 
 'u' -> 'a'
 _ -> '?'
--4.2 dado un digito devolvemos el digito en String
fun4_2 :: Int -> String 
fun4_2 num = 
 case num of
 num -> read $ show num :: String 
--4.3 dado 2 valores logicos determinar su salida por la compuerta and
fun4_3 :: Bool -> Bool -> Bool
fun4_3 v1 v2 = 
 case v1 of 
 False -> False
 True -> case v2 of 
         False -> False
         True -> True
--4.4 dado 2 valores su salida por compuerta or 
fun4_4 :: Bool -> Bool -> Bool 
fun4_4 v1 v2 = case v1 of  
               True -> True 
               False -> case v2 of 
                        False -> False 
                        True -> True
--4.5 dado 2 valores su salida para xor
fun4_5 :: Bool -> Bool -> Bool 
fun4_5 v1 v2 = case v1 of
               False -> case v2 of 
                        False -> False
                        True -> True 
               True -> case v2 of 
                       True -> False 
                       False -> True
--4.6 dado 2 valores pero que reciba la operacion como argumento
fun4_6 :: Bool -> Bool -> String -> Bool 
fun4_6 v1 v2 oper = case oper of 
                         "and" -> fun4_3 v1 v2
                         "or" -> fun4_4 v1 v2
                         "xor" -> fun4_5 v1 v2
--4.7 funcion que reciba 2 digitos y devuelva su literal
--fun4_7 :: Int -> String 
--fun4_7 num =
--fun4_8 funcion que reciba 3 digitos y devuelva su literal 
--4.9 dado 2 nnumeros devolver el menor 
fun4_9 n1 n2 = 
 case (n1 >= n2) of 
 True -> n2
 false -> n1
--4.10 dado 6 numeros devuelva el menor 
fun4_10 n1 n2 n3 n4 n5 n6 = 
 case (fun4_9 n1 n2 >= fun4_9 n3 n4) of
 True -> case (fun4_9 n3 n4 >= fun4_9 n5 n6) of
         True -> fun4_9 n5 n6
         False -> fun4_9 n3 n4
 False -> case (fun4_9 n1 n2 >= fun4_9 n5 n6) of
          True -> fun4_9 n5 n6
          False -> fun4_9 n1 n2
--4.11 dado 3 numeros si su suma es mayor a 20 devolver sumatoria mayor y si no sumatoria menor y si es menor a 10 vacio
fun4_11 :: Int -> Int -> Int -> String
fun4_11 n1 n2 n3 = case (n1 + n2 + n3 < 20) of
                   True -> "Sumatoria mayor"
                   False -> case (n1 + n2 + n3 < 10) of
                            True -> "Sumatoria menor"
                            _ -> "Vacio"
--4.12 dado 3 notas devolver si es excelente, bien, regular o mal
fun4_12 :: Int -> Int -> Int -> String
fun4_12 n1 n2 n3 = case ((n1 + n2 + n3) `div` 3) >= 90  of
                   True -> "Excelente"
                   False -> case ((n1 + n2 + n3) `div` 3) >= 70 of
                            True -> "Bien"
                            False -> case ((n1 + n2 + n3) `div` 3) >= 51 of
                                     True -> "Regular"
                                     _ -> "Mal"
--Ejercicio propuesto de clase
notasArgumento :: Int -> Int -> Int -> Int -> String
notasArgumento e1 e2 e3 e4 | (( e1 +  e2  +  e3  + e4 ) `div` 4) >= 90 = "Excelente" | (( e1 +  e2  +  e3  + e4 ) `div` 4) >= 80 = "Muy Bien" | (( e1 +  e2  +  e3  + e4 ) `div` 4) >= 70 = "Bien" | (( e1 +  e2  +  e3  + e4 ) `div` 4) >= 51 = "Regular" | (( e1 +  e2  +  e3  + e4 ) `div` 4) >= 0 = "Mal" | otherwise = "Notas invalidas"
--6.13 recibe 6 numeros y devuelve el menor hice mayor por accidente
fun6_13 n1 n2 n3 n4 n5 n6 = 
 let 
   maDe4 = mayorDe4 n1 n2 n3 n4
 in  
   if n5 > maDe4 && n5 > n6 then n5 else if n6 > maDe4 then n6 else maDe4 
--6.14 recibe 3 numeros y devuelva "sumatoria mayor" si la sumatoria de los numeros es menor que 20
--, "Sumatoria menor" si es menor que 10 y "Vacio" en otro caso 
fun6_14 :: Int -> Int -> Int -> String
fun6_14 n1 n2 n3 = 
 let
   sumatoria = n1 + n2 + n3
 in 
   if sumatoria < 20 then "Sumatoria menor" else if sumatoria < 10 then "Sumatoria menor" else "Vacio"
--6.15 funcion que reciba 3 notas y devuelva "Excelete" 90 - 100
--"Bien" 70 - 89, "Regular" 51 - 69 y "Mal" 0 - 50
fun6_15 :: Int -> Int -> Int -> String
fun6_15 n1 n2 n3 = 
 let
   promedio = (n1 + n2 + n3) `div` 3
   estaEn li ls = promedio >= li && promedio <= ls
 in 
   if estaEn 90 100 == True then "Excelente" 
    else if estaEn 70 89 == True then "Bien" 
     else if estaEn 51 69 == True then "Regular" 
      else "Mal"
--6.16 inventar 3 ejercicios con el uso de definicioens locales 
--Ejercicio propuesto en clase fecha mayor de 2
maFecha :: (Int,Int,Int) -> (Int,Int,Int) -> (Int,Int,Int)
maFecha (d1, m1, a1) (d2, m2, a2) | a1 > a2 = (d1, m1, a1) 
                                  | a1 == a2 && m1 > m2 = (d1,m1,a1)
                                  | a1 == a2 && m1 == m2 && d1 > d2 = (d1,m1,a1)
                                  |otherwise = (d2,m2,a2)
--Ejercicio propuesto en clase mayor de 4 fechas								  
maFechaDe4v1 :: (Int,Int,Int) -> (Int,Int,Int) -> (Int,Int,Int) -> (Int,Int,Int) -> (Int,Int,Int)
maFechaDe4v1 (d1,m1,a1)(d2,m2,a2)(d3,m3,a3)(d4,m4,a4) = maFecha (maFecha (d1,m1,a1)(d2,m2,a2)) (maFecha (d3,m3,a3)(d4,m4,a4))

maFechaDe4v2 :: (Int,Int,Int) -> (Int,Int,Int) -> (Int,Int,Int) -> (Int,Int,Int) -> (Int,Int,Int)
maFechaDe4v2 (d1,m1,a1)(d2,m2,a2)(d3,m3,a3)(d4,m4,a4) =
 let 
  f1 = maFecha (d1,m1,a1)(d2,m2,a2)
  f2 = maFecha (d3,m3,a3)(d3,m3,a3)
  in
   if maFecha f1 f2 == (d1,m1,a1) then (d1,m1,a1) else
   if maFecha f1 f2 == (d2,m2,a2) then (d2,m2,a2) else
   if maFecha f1 f2 == (d3,m3,a3) then (d3,m3,a3) else
   (d4,m4,a4)
--7.1 una fecha y devuelva el dia

fun7_1 (d,_,_) = d 
--7.2 una fecha y devuelva el mes

fun7_2 (_,m,_) = m 
--7.3 una fecha y devuelva el mes

fun7_3 (_,_,a) = a
--7.4 recibe 2 quebrados y devuelva el mayor
fun7_4 :: (Int,Int) -> (Int,Int) -> (Int,Int)
fun7_4 (num1,den1) (num2,den2) = if num1 `div` den1 > num2 `div` den2 then (num1,den1)
                                  else if num1 `div` den1 < num2 `div` den2 then (num2,den2)
                                   else if (num1 `mod` den1) `div` 2 > (num2 `mod` den2) `div` 2 then (num2,den2)
                                    else if (num1 `mod` den1) `div` 2 < (num2 `mod` den2) `div` 2 then (num1,den1)
                                     else if (num1 `mod` den1) `div` 2 == (num2 `mod` den2) `div` 2  then (num2,den2)
                                      else if (num1 `mod` den1) `mod` 2 > (num2 `mod` den2) `mod` 2 then (num1,den1)
                                       else (num2,den2)
--7.5 recibe 1 quebrado y lo devuelva reducido 
multiplo :: Int -> Int -> Bool
multiplo v1 v2 = if v1 `mod` v2 == 0 then True else False
fun7_5 :: (Int,Int) -> (Int,Int)
fun7_5 (num1,den1) = if multiplo num1 den1 == True || multiplo den1 num1 == True then ( num1 `div`(fun4_9 num1 den1), den1 `div`(fun4_9 num1 den1)) else (num1,den1)
--7.6 recibe un quebrado y devuelve su signo como caracter
fun7_6 :: (Int,Int) -> Char
fun7_6 (numerador, denominador) = if numerador < 0 || denominador < 0 || (numerador < 0 && denominador < 0) then '+' else '-'
--7.7 recibe 2 fechas y devuelve la menor 
fun7_7 :: (Int,Int,Int) -> (Int,Int,Int) -> (Int,Int,Int)
fun7_7 (d1,m1,a1)(d2,m2,a2) | a1 > a2 = (d2,m2,a2)
                            | a1 == a2 && m1 > m2 = (d2,m2,a2)
                            | a1 == a2 && m1 == m2 && d1 > d2 = (d2,m2,a2)
                            | otherwise = (d1,m1,a1)
--7.8 recibe 2 horas devuelve el mayor 
fun7_8 :: (Int,Int,Int,Int) -> (Int,Int,Int,Int) -> Int
fun7_8 (_,_,_,h1) (_,_,_,h2) = if h1 > h2 then h1 else h2 
--7.9 2 instantes y devuelve el mas reciente instante = fecha,hora,min,segundo
fun7_9 :: ((Int,Int,Int),Int,Int,Int) -> ((Int,Int,Int),Int,Int,Int) -> ((Int,Int,Int),Int,Int,Int)
fun7_9 (f1,h1,m1,s1) (f2,h2,m2,s2) = if f1 /= f2 then if maFecha f1 f2 == f1 then (f1,h1,m1,s1) else (f2,h2,m2,s2) else if h1 > h2 then (f1,h1,m1,s1) else if m1 > m2 then (f1,h1,m1,s1) else if s1 > s2 then (f1,h1,m1,s1) else (f2,h2,m2,s2) 
--fun7.10 recibe numero natural y devuelva el siguiente
fun7_10 :: Int -> Int
fun7_10 num = num + 1
--fun7.11 recibe un quebrado de quebrado 
fun7_11 :: ((Int,Int),(Int,Int)) -> (Int,Int)
fun7_11 ((a,b),(c,d)) = 
 let
  arribaSimp = fun7_5 (a,b)
  abajoSimp = fun7_5 (c,d)
 in  
  (a*d,b*c)
--Calculo lamda
l1_1 :: Int -> Int  
l1_1 = (\lado -> lado*lado)
--
