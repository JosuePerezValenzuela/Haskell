f1:: Bool -> (Int -> (Int -> Int)) 
((f1 x) y) z = if x then y+10 else z

f2:: Int -> (Int -> (Char -> Int))
((f2 x) y) z = if z=='s' then 2*x else y

f3:: (Int -> Bool) -> ((Char -> Bool) -> ((Bool -> Bool) -> Bool))
((f3 x) y) z = ( x 2) && (y 'a') && (z True)

f4:: (Int -> Bool) -> (t -> (Int -> (Int -> Int)))
f4=(\x -> \y -> \z -> \w -> if x 2 then z else w+10 )

f5:: ((Int -> Int), Int) -> (((Int -> Int), Int) -> Int)
f5=(\(x,y) -> \(z ,w) -> (x y )+ (z w))

f6:: t -> t
f6 x = x

f7:: (Int -> t) -> Int -> t
f7 x y = x y

f8:: (Bool -> Int) -> (Int -> Bool) -> Int -> Int  
f8 x y z = x (y z)

f9:: (Int -> (Char -> Bool)) -> Int -> Char -> Bool 
f9 x y z = (x y) z

f10:: (Int -> Char) -> (Bool -> Int) -> (String -> Bool) -> String -> Char 
f10 x y z w= x (y (z w))

f11:: (ty -> (tz -> (tw -> tr))) -> ty -> tz -> tw -> tr  
(((f11 x) y) z) w= ((x y) z) w

f12:: (ty -> (tz -> tr)) -> ty -> (tw -> tz) -> tw -> tr
f12 x y z w= ((x y) (z w))

f13:: Int -> Int -> t -> Int
f13 x y z = x*2+y

f14:: Int -> Int -> t -> Int
f14 x y z = x*2+y

f16:: (Bool -> Char -> Int) -> Bool -> Char -> Int
f16 x y z = x y z

f17:: Bool -> Bool -> Bool -> Bool
f17 x y z | x = y
          | y = z

f18:: (Int -> Int -> Int) -> (Int -> Int) -> Int -> Int
f18 x y z = x (y z)(y z)

c19:: (Int -> Int -> Int) -> (Int,Int) -> Int
c19 f (x, y) = f x y

u20:: ((Int,Int) -> Int) -> Int -> Int -> Int
u20 f x y = f(x, y)

f21:: Num ty => tx -> ty -> (ty -> ty) -> ty
f21 x y z = s + y
    where s = z y

f22:: Num tx => (Int -> Int -> tx) -> Int -> Int -> tx
f22 x y z = r1 + r2
  where r1 = x 5 y
        r2 = x y z

c23:: ((tx,ty) -> tr) -> tx -> ty -> tr 
c23 f = g
    where g x y = f (x, y)

u24:: (tx -> ty -> tr) -> (tx,ty) -> tr
u24 f = g
    where g (x,y) = f x y

c25:: ((tx,ty,tz) -> tr) -> tx -> ty -> tz -> tr
c25 f = g
    where g x y z = f(x, y, z)

--f26:: tz -> Bool -> (Bool -> Bool) -> tz -> (tz -> tz) -> tz 
--f26 e x y z w = if x&&(y x) then z else w z
--            where w a |a = e
--                      |otherwise = z

--PARTE 2
--Escribir los tipos de las siguientes expresiones
suma1:: Int -> Int -> Int 
suma1 x y = x + y
suma2:: (Int,Int) -> Int
suma2 (x,y) = x + y

--1curry1:: ((tx,ty) -> tr) -> tx -> ty -> tr
--2curry2:: ((tx,ty) -> tr) -> tx -> ty -> tr

--3uncurry1:: (tx -> ty -> tr) -> (tx,ty) -> tr
--4uncurry2:: (tx -> ty -> tr) -> (tx,ty) -> tr

--5curry1 suma1:: no puede hacerce 
--6curry1 suma2:: tx -> ty -> tr
--7uncurry1 suma1:: (tx,ty) -> tr
--8curry1 suma2 3 5 
-- curry1 suma2:: Int -> Int -> Int 
-- curry1 suma2 3 5 = 8
--9curry1 suma2 7
-- curry1 suma2:: Int -> Int -> Int
-- curry1 suma2 7 = Error espera 2 int
--10curry1 uncurry1 suma1 2 3
--  curry1 uncurry1 suma1
--  curry1 uncurry1:: Error
--11uncurry1 (curry1 suma2) (2,3)
-- curry1 suma2:: Int -> Int -> Int
-- uncurry1 Int -> Int -> Int:: (Int,Int) -> Int
-- uncurry1 (curry1 suma2) (2,3) = 5
--12uncurry1 (curry1 suma2 (2,3))
--curry1 suma2:: Int -> Int -> Int 
--curry1 suma2 (2,3) = Error

--PARTE 3
--Para las siguientes definiciones escribir una funcion que empareje

f1_3::Int->Int->Int->Bool->Int->Bool->Int
f1_3 num1 num2 num3 boo num4 boo2 = if boo && boo2 then num1 * num2
                                    else if boo == True then num3 * num4
                                    else num1 * num2 * num3 * num4
faux:: Int -> Int
faux num1 = num1 * num1
f2_3::(Int->Int)->Int->Bool->Int->Bool->Int
f2_3 faux num1 boo num2 boo2 = if boo && boo2 then faux num1 
                               else if boo2 == True then faux num2
                               else faux num1 * faux num2

faux2 :: Int -> Bool
faux2 num = if num `mod` 2 == 0 then True else False
f3_3::(Int->Int)->(Int->Bool)->Int->Bool->Int
f3_3 faux faux2 num1 boo = if faux2 num1 && boo then num1^3
                           else faux num1

faux3:: (Int -> Int -> Int)
faux3 num1 num2 = num1 * num2
f4_3::(Int->Int->Int)->Bool->Int->Bool->Int
f4_3 faux3 boo num1 boo2 = if boo && boo2 then faux3 num1 num1
                           else num1

fau4:: (Int -> Int) -> Int -> Bool 
fau4 f num = if f num == num * num then True  else False  
f5_3::((Int->Int)->Int->Bool)->Int->Bool->Int
f5_3 f num boo = if boo && fau4 faux num then num * num else num