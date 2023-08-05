data Lista a = Sinelementos | Aumentar a (Lista a)
  deriving Show

data Natural = Nada | Unomas Natural
  deriving Show

instance Eq Natural 
 where
 (==) Nada Nada = True
 (==) (Unomas x) (Unomas y) = x == y
 (==) _ _ = False

type Cadena = Lista Char
type PosicionInicial = Natural
type PosicionFinal = Natural

subCadena2:: Cadena -> PosicionInicial -> PosicionFinal -> Cadena
subCadena2 xs de hasta = if de /= hasta then Aumentar ((get) xs de) (subCadena2 xs (Unomas de) hasta) else Aumentar ((get) xs hasta) (Sinelementos) 
 where
  get (Aumentar x _) Nada = x
  get (Aumentar x xs) (Unomas n) = get xs n

x = (Aumentar 'H' (Aumentar 'o' (Aumentar 'l' (Aumentar 'a' Sinelementos))))
from = (Unomas (Nada))
to = (Unomas (Unomas (Nada)))