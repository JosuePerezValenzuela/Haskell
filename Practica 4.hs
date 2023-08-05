--Practica 4 
--1 Una función que reciba una lista y devuelva 
--la productoria de sus elementos
f1 :: [Int] -> Int
f1 xs = foldr (*) 1 xs
--2 map
miMap fun xs = foldr (\x -> \lisR -> (fun x): lisR) [] xs
--3 filter
miFilter fun xs = foldr (\x -> \lisR -> if fun x == True then x : lisR else lisR) [] xs

data Natural = Cero | Suc Natural
 deriving Show

instance Eq a => Eq Natural where
(==) (Cero) (Cero) = True
(==) (Suc x) (Suc y) = x == y
(==) _ _ = False