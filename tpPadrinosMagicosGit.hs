
import Text.Show.Functions ()
--1

data Chico = Chico {
    nombre :: String,
    edad :: Int,
    habilidades :: [String],
    deseos :: [Chico -> Chico]
}

roman :: Chico
roman = Chico "Roman" 10 ["teletransportacion"] [serMayor]

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

serMayor :: Chico -> Chico
serMayor chico = chico {edad = 18}

--2
concederDeseo :: Chico -> Chico
concederDeseo chico = last (mapearFunciones (deseos chico) chico)
concederUnicoDeseo :: Chico -> Chico
concederUnicoDeseo chico = head (mapearFunciones (deseos chico) chico)
mapearFunciones :: [t -> a] -> t -> [a]
mapearFunciones (f:fs) x = f x : mapearFunciones fs x
mapearFunciones [] _ = []
aumentarEdad :: Chico -> Int -> Chico
aumentarEdad chico n = chico {edad = edad chico + n } 
--wanda: dado un chico, wanda le cumple el primer deseo y lo hace madurar (crecer un año de edad).
wanda :: Chico -> Chico
wanda chico = aumentarEdad (concederUnicoDeseo chico) 1

wanda2 :: Chico -> Chico
wanda2 (Chico nombre ed hab d) = concederDeseo (Chico nombre (ed+2) hab [head d]) -- no le suma los 2 años porque luego de sumar aplica el deseo y lo deja con 18
