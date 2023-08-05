--Practica 5
--Fliter por listas de comprension
mifilter f xs =[x | x <- xs , f x]
--Map
miMap f xs = [f x |x <- xs]
--concat
miConcat xss = [foldr (++)[] x | x <- [xss]] !! 0
--length
miLength xs = sum [1 | x <- xs]
--Practicando 
--procesaMats:: [(String,Int,Int)] -> (String,Int,Int) -> [(String,Int,Int)]
--procesaMats  lista@[(nombreDeLaMat, promedio, alumnos)] mat@(nombreDeLaMat2, promedio2, alumnos2) = foldr z y x
-- where 
-- x =  lista
-- y = [mat]
-- z x@(nombreDeLaMat2, promedio2, alumnos2) rs@[(nombreDeLaMat, promedio, alumnos)] = if ((last rs) alumnos) >= alumnos2 then x : rs
--                                                                                     else (last rs)(++) x (++)(init rs)
fun :: ((Int,Int,Char)->Int->Int)->((Bool,Int)->Bool)->(Int -> (Bool,Int))
fun f g n1 = if g t == True then (True, n1)
             else if f s n1 > 50 then (True, n1*2)
             else (False, 0)
 where 
 s = (n1, n1-3, 'V')
 t = (even n1, n1 * 4)
 --- Examen 3
getDiaMes xs year = foldr f a xs
 where 
  a = []
  f x@(d,m,a) rs = g x rs year  
  g x@(d,m,a) rs year = if a == year then (d,m) : rs
                         else rs

fs= [(2,3,17),(12,5,18), (4,6,17),(9,2,17),(8,8,16),(5,7,17)]



{-type ci = Int
type cs = [Int]
type pos = Int 
buscar:: ci -> cs -> pos

buscar ci cs = esta ci cs 0
esta c [] pos = -1
esta c (x:xs) pos 
      |c == x = pos
      |otherwise = esta c xs (p+1)

data Dia = Lun|Mar|Mie|Jue|Vie|Sab|Dom
diaSgte:: Dia -> Dia
diaSgte Lun = Mar
diaSgte Mar = Mie 
diaSgte Mie = Jue 
diaSgte Jue = Vie 
diaSgte Vie = Sab
diaSgte Sab = Dom 
diaSgte Dom = Lun

esDescanso:: Dia -> Bool
esDescanso Dom = True
esDescanso _ = False-}

data TipoUsuario = Administrador | Lead | Comun
permisos:: TipoUsuario -> String
permisos Administrador = "Modulo 1,2,3"
permisos Lead = "Modulo 4,5"
permisos Comun = "Modulo 6,7"


data TipoMovilidad = Vagoneta | Camioneta | Camion | Auto
type Placa = String
funA:: [(Placa,TipoMovilidad)] -> TipoMovilidad
funA xs = contar xs 0 0 0 0
contar [] v c ca a = if v >= c && v >= ca && v >= a then Vagoneta
                     else if c >= ca && c >= a then Camioneta 
                     else if ca >= a then Camion
                     else Auto
contar (x:xs) v c ca a = if esVago x == True then contar xs (v+1) c ca a
                         else if esCamioneta x == True then contar xs v (c+1) ca a
                         else if esCamio x == True then contar xs v c (ca+1) a
                         else contar xs v c ca (a+1)

esVago:: (Placa,TipoMovilidad) -> Bool
esVago (_,Vagoneta) = True
esVago (_,_) = False
esCamioneta:: (Placa,TipoMovilidad) -> Bool
esCamioneta (_,Camioneta) = True
esCamioneta (_,_) = False
esCamio:: (Placa,TipoMovilidad) -> Bool
esCamio (_,Camion) = True
esCamio (_,_) = False


data Resp = Valido Bool| NoValido String
  deriving Show
data Nota = Numeral Int| Literal Char
reprobado:: Nota -> Resp 
reprobado (Literal x) | x == 'A' || x == 'B' || x == 'C' = Valido False
                      | x == 'D' = Valido True
                      | otherwise = NoValido "Nota invalida"
reprobado (Numeral x) = if x <= 100 && x > 50 then Valido False
                        else if x >= 0 && x < 51 then Valido True
                        else NoValido "Nota Invalida"

--definir una funcion int2Natural que reciba un entero y devuelva
-- el equivalente en Natural 

data Natural = Cero | Suc Natural
 deriving Show 
data Respuesta = Positivo (Char,Natural) | Negativo (Char,Natural) | Natural Natural| Error String
 deriving Show 
int2Natural:: Int -> Natural
int2Natural 0 = Cero
int2Natural n = Suc (int2Natural (n-1))

natural2Int :: Natural -> Int
natural2Int Cero = 0
natural2Int (Suc n)= 1+(natural2Int n)

-- 2. def una f que reciba dos naturales y devuelva true si el 
--primero es mayor que el segundo, falso en otro caso

mayorNatural:: Natural -> Natural -> Bool
mayorNatural Cero Cero = False 
mayorNatural Cero n = True
mayorNatural n Cero = False 
mayorNatural (Suc n1) (Suc n2) = mayorNatural n1 n2 

suma:: Natural -> Natural -> Natural
suma Cero n2 = n2
suma n1 Cero = n1
suma (n1) (Suc n2) = suma (Suc n1) (n2)

resta:: Natural -> Natural -> Respuesta
resta Cero n2 = Negativo ('-',n2)
resta n1 Cero = Positivo ('+',n1)
resta (Suc n1) (Suc n2) = resta n1 n2

resta2:: Natural -> Natural -> Natural
resta2 Cero n2 = Cero
resta2 n1 Cero = n1
resta2 (Suc n1) (Suc n2) = resta2 n1 n2

multi:: Natural -> Natural -> Natural
multi Cero n2 = Cero
multi n1 Cero = Cero
multi n1 (Suc n2) = suma (n1) (multi (n1) (n2))

divi:: Natural -> Natural -> Natural 
--divi n1 Cero = Error "No es posible la division entre 0"
divi Cero n2 = Cero
divi n1 n2 = suma (Suc Cero) (divi (resta2 n1 n2) (n2))

data Arbol a = Hoja a| Rama (Arbol a)(Arbol a)
  deriving Show
  
arb1= Rama (Hoja 5)
           (Rama (Rama (Hoja 2)
                       (Hoja 10)
                 )
                 (Hoja 30)
           )
  
--Función que reciba un árbol y devuelva el elemento mayor del mismo
mayorDelArbol:: Arbol Integer -> Integer
mayorDelArbol (Hoja x) = x
mayorDelArbol (Rama (ar1) (ar2)) = if (mayorDelArbol ar1) > (mayorDelArbol ar2) then (mayorDelArbol ar1) else (mayorDelArbol ar2)

-- Función que reciba un árbol y devuelva sus elementos en una lista
elementos:: Arbol a -> [a]
elementos (Hoja x) = [x]
elementos (Rama (ar1) (ar2)) = (elementos ar1) ++ (elementos ar2)

--Función que reciba un árbol y devuelva sus elementos en una lista ordenada ascendentemente
ordenarAscendentemente:: Arbol Integer -> [Integer]
ordenarAscendentemente (Hoja n) = [n]
ordenarAscendentemente (Rama (ar1) (ar2)) = ordenar ((elementos ar1) ++ (elementos ar2))
 where 
  ordenar:: [Integer] -> [Integer]
  ordenar xs = if length xs > 0 then [minimum xs] ++ ordenar (filter (/= (minimum xs)) xs) else []  

data ArbolBus a = HojaB | SubArbolB (ArbolBus a)(a)(ArbolBus a)
  deriving Show

mayorDelArbolBus:: ArbolBus a -> a
mayorDelArbolBus (SubArbolB _ (hoja) HojaB) = hoja
mayorDelArbolBus (SubArbolB (ari) (hoja) (ard)) = mayorDelArbolBus ard

arbus1:: ArbolBus Integer
arbus1 = SubArbolB (SubArbolB (HojaB) (1) (HojaB))
                   (2)
                   (SubArbolB (SubArbolB (HojaB) (3) (HojaB)) (4) (SubArbolB (HojaB) (5) (SubArbolB (HojaB ) (6) (SubArbolB (HojaB) (7) (HojaB)))))

--profundidad
profundidad:: ArbolBus a -> Integer
profundidad (HojaB) = 0
profundidad (SubArbolB (ari) (hoja) (ard)) = if (profundidad ari) >= (profundidad ard) then 1 + (profundidad ari) else 1 + (profundidad ard)

--balanceado
balanceado:: ArbolBus a -> Bool
balanceado (HojaB) = True
balanceado (SubArbolB (ari) (hoja) (ard)) = if (abs (profundidad ari - profundidad ard)) >= 2 then False else balanceado ari && balanceado ard

--insertar
insertar :: Int -> ArbolBus Int -> ArbolBus Int
insertar x (HojaB) = SubArbolB HojaB x HojaB
insertar x (SubArbolB izq y der)
           | x < y = SubArbolB (insertar x izq) y der
           | x > y = SubArbolB izq y (insertar x der)
           | otherwise = SubArbolB izq x der

divisor n = posDiv n 1
  where
    posDiv n m = if (n `mod` m)== 0 then m:(posDiv n (m+1)) else if n < m then [] else posDiv n (m+1)

buscar:: Integer -> [Integer] -> Integer
buscar n xs = buscarEle n xs 0
 where
  buscarEle n [] m = (-1)
  buscarEle n xs m = if (head xs) == n then m else buscarEle n (tail xs) (m+1)

data Lista a = Vacia | Add a (Lista a )
  deriving Show

instance Eq Natural where
 (==) (Cero) (Cero) = True
 (==) (Suc x) (Suc y) = x == y
 (==) _ _ = False

instance Eq a => Eq (Lista a) where
 (==) Vacia Vacia = True
 (==) (Add x xs) (Add y ys) = x==y && xs == ys
 (==) _ _ = False

instance Eq a => Eq (Arbol a) where
 (==) (Hoja x) (Hoja y) = x == y
 (==) (Rama ai ad) (Rama ai2 ad2) = ai == ai2 && ad == ad2
 (==) _ _ = False

data Rosa a = Nodo a [Rosa a]
  deriving Show

toLis [] = Vacia
toLis (x:xs) = Add x (toLis xs)

prueba = map rosa2lista [Nodo 4 [], Nodo 5 []]
 where
  rosa2lista (Nodo x []) = Add x Vacia
--               lo que sale del map
prueba2 = toLis([Add 4 Vacia, Add 5 Vacia])
--      despues de aplica toLis a lo que sale del map
aux = Add (Add 4 Vacia) (Add (Add 5 Vacia) Vacia)
-- error de tipos ya que el tipo de aux es Lista (Lista Integer),
-- y nosotros le estamos dando Add 3 lo cual hace que tenga
-- que ser Lista Integer
--prueba3 = Add 3 (Add (Add 4 Vacia) (Add (Add 5 Vacia) Vacia))

aux3 = [Add 4 (Add 3 Vacia)]
prueba2_0 = toLis aux3

tarea = (Nodo 3 [Nodo 4[], Nodo 5 []])

rosa2lista (Nodo x []) = Add x Vacia
rosa2Lista (Nodo x xs) = Add x (toListaa (map rosa2Lista xs))

toListaa [] = Vacia
toListaa (x:xs) = Add (getNum x) (toListaa xs)
 where
  getNum:: Lista a -> a
  getNum (Add n xs) = n

type Nombre = String
type HorasLab = Int
type SueldoHoras = Float
type Salario = Float
type Puesto = String
type Materia = String
data Empleado = Docente Nombre HorasLab SueldoHoras Materia | Administrativo Nombre Salario Puesto
 deriving Show
obtenerNom::Empleado -> Nombre
obtenerNom (Docente nom hlab sldhr mat) = nom

empleado:: Empleado
empleado = Docente "Delgadillo" 25 40.0 "Est"