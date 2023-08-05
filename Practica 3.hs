--PARTE 2 UTILIZANDO LAS FUNCIONES SOBRE LISTAS,DEFINIR LAS SIGUIENTES 
--FUNCIONES 
--1. Definir una función que reciba una lista de elementos 
--y devuelva el tercero
f1:: [a] -> a
f1 xs = last(take 3 xs)
--2. Definir una función que reciba una lista de elementos 
--y devuelva el segundo
f2:: [a] -> a
f2 xs = (last.take 2) xs
--3. Definir una función que reciba una lista de listas de funciones 
--y un elemento y aplique la 1ra función de la primera lista al elemento
elevar2 num = num ^ 2
f3:: [[(e -> tr)]] -> e -> tr
f3 xs ele = head (head xs) ele
--4. Definir una función que reciba una lista de listas 
--y devuelva el 5to. Elemento de la 3ra. lista.
f4:: [[a]] -> a
f4 xs = last(take 5(last(take 3 xs)))
--5. Definir una función que reciba una lista de listas de listas 
--y devuelva el 3er. elemento de la 4ta. Lista de la 2da. lista
f5:: [[[a]]] -> a
f5 xs = (last.take 3.last.take 4.last.take 2) xs
--6 Definir una función que verifique si una lista esta ordenada 
--de acuerdo a una función de orden.

--7 Definir una función que compare 2 listas 
--y devuelva True si las listas son iguales
f7:: [Int] -> [Int] -> Bool
f7 xs ys = and (zipWith (==) xs ys)
--8 Definir una función que verifique si una 
--lista de listas podría ser considerada una matriz
f8 :: [[a]] -> Bool
f8 xss = 
 let 
  tamaño = length xss
  tamañoDeListas = map (length) xss
  cumple = map (== tamaño) tamañoDeListas
 in
  if and cumple then True else False
--9 Definir una función que reciba un número y una lista y devuelva
--el elemento de la lista que esta en la posición n

--Ejercicios propuestos 
--Def 1 función que reciba una lista de números y devuelva 
--la sumatoria de los números pares
sumPares:: [Int] -> Int
sumPares xs = 
 let 
  lisPares = filter even xs
 in
  sum lisPares 
--Def 1 función que reciba un lista de números xs y un número n 
--y devuelva verdad si n es mayor a todos los elementos de xs
nEsMayor:: [Int]  -> Int -> Bool
nEsMayor xs num =
 let 
  listaDeResp = map (<num) xs
 in
  and listaDeResp 
-- Def 1 función que reciba una matriz (lista de listas, donde 
--cada lista es una fila) y devuelva la primera columna
primColum:: [[a]] -> [a]
primColum xss = map (!! 0) xss 
--Def 1 función que reciba una matriz y una entero c 
--y devuelva la columna c de la matriz
columN :: [[a]] -> Int -> [a]
columN xss c = map (!! (c - 1)) xss
--Def 1 función que reciba una matriz y devuelva su transpuesta
transpuesta:: [[Int]] -> [[Int]]
transpuesta xss = map (\c -> columN xss c) [1..(length (head xss))]
    