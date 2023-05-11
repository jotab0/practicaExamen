import Text.Show.Functions ()

--A. Concediendo deseos
--1

data Chico = Chico {
    nombre :: String,
    edad :: Int,
    habilidades :: [String],
    deseos :: [Chico -> Chico]
} deriving Show

roman :: Chico
roman = Chico "Roman" 10 ["teletransportacion"] [serMayor]
claudio :: Chico
claudio = Chico "Claudio" 5 ["Decir 100 palabras por segundo"] [serMayor, aprenderHabilidades ["volar","memorizar"]]

--aprenderHabilidades habilidades unChico : agrega una lista de habilidadesnuevas a las que ya tiene el chico.
aprenderHabilidades :: [String] -> Chico -> Chico
aprenderHabilidades habilidadesNuevas unChico = unChico { habilidades = habilidades unChico ++ habilidadesNuevas }

--serGrosoEnNeedForSpeed unChico: dado un chico, le agrega las habilidades
--de jugar a todas las versiones pasadas y futuras del Need For Speed, que
--son: “jugar need for speed 1”, “jugar need for speed 2”, etc.

serGrosoEnNeedForSpeed :: Chico -> Chico
serGrosoEnNeedForSpeed = aprenderHabilidades (listaNeedForSpeed 1)

listaNeedForSpeed :: (Ord t, Num t, Show t) => t -> [[Char]]
listaNeedForSpeed n
    | n < 10 = ("Saber jugar Need for speed " ++ numeroToString n) : listaNeedForSpeed (n+1)
    | otherwise = []

numeroToString :: Show a => a -> String
numeroToString = show

--serMayor: chico pase a ser mayor de 18
serMayor :: Chico -> Chico
serMayor chico = chico {edad = 18}

--2

concederDeseos :: Chico -> Chico
concederDeseos chico = last (mapearFunciones (deseos chico) chico)

concederUnicoDeseo :: Chico -> Chico
concederUnicoDeseo chico = head (mapearFunciones (deseos chico) chico)

mapearFunciones :: [t -> a] -> t -> [a]
mapearFunciones (f:fs) x = f x : mapearFunciones fs x
mapearFunciones [] _ = []

alterarEdad :: Int -> Chico -> Chico
alterarEdad n chico = chico {edad = edad chico + n }

--wanda: dado un chico, wanda le cumple el primer deseo y lo hace madurar (crecer un año de edad).
wanda :: Chico -> Chico
wanda chico = alterarEdad 1 (concederUnicoDeseo chico)

wanda2 :: Chico -> Chico
wanda2 (Chico nombre ed hab d) = (alterarEdad 1.concederDeseos) (Chico nombre ed hab [head d]) -- no le suma los 2 años porque luego de sumar aplica el deseo y lo deja con 18

--cosmo: dado un chico, lo hace “des”madurar, quedando con la mitad de años de edad. Como es olvidadizo, no le concede ningún deseo.
cosmo :: Chico -> Chico
cosmo chico = chico {edad = div (edad chico) 2}

--muffinMagico: dado un chico le concede todos sus deseos.
muffinMagico :: Chico -> Chico
muffinMagico = concederDeseos

--B. En busqueda de pareja

data Chica = Chica {
    nombrec :: String,
    condicion :: Chico -> Bool
}

--1.
--a. tieneHabilidad unaHabilidad unChico: Dado un chico y una habilidad, dice
--si la posee.

tieneHabilidad habilidad chico = habilidad `elem` habilidades chico

--b. esSuperMaduro: Dado un chico dice si es mayor de edad (es decir, tiene más
--de 18 años) y además sabe manejar.

esSuperMaduro chico = edad chico > 18 && elem "manejar" (habilidades chico)

--2.
--Para Trixie la única condición es que el chico no sea Timmy,
--ya que nunca saldría con él
--trixie = Chica “Trixie Tang” noEsTimmy
--vicky = Chica “Vicky” (tieneHabilidad “ser un supermodelo noruego”)

noEsTimmy :: Chico -> Bool
noEsTimmy chico = nombre chico /= "Timmy" 
trixie :: Chica
trixie = Chica "Trixie" noEsTimmy