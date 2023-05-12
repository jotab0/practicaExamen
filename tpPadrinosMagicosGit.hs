
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
chad :: Chico
chad = Chico "Chad" 12 ["ser un supermodelo noruego", "enamorar"] [serMayor]
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

nfs :: String
nfs = "Saber jugar Need for speed "

lista2NeedForSpeed :: Show a => [a] -> [[Char]]
lista2NeedForSpeed n = map (\a -> nfs ++ numeroToString a) n

numeroToString :: Show a => a -> String
numeroToString = show

--serMayor: chico pase a ser mayor de 18
serMayor :: Chico -> Chico
serMayor chico = chico {edad = 18}

--2

-- ESTAN MAL PORQUE NO ACUMULAN LOS DESEOS EN EL CHICO
concederDeseosR :: Chico -> Chico
concederDeseosR chico = last (mapearFunciones (deseos chico) chico)

concederDeseosNR :: Chico -> Chico
concederDeseosNR chico = last (map (\f -> f chico) (deseos chico))

concederDeseosNR2 :: Chico -> Chico
concederDeseosNR2 chico = last (map (mapearFuncionesNR chico) (deseos chico))

concederUnicoDeseo :: Chico -> Chico
concederUnicoDeseo chico = head (mapearFunciones (deseos chico) chico)

mapearFunciones :: [t -> a] -> t -> [a]
mapearFunciones (f:fs) x = f x : mapearFunciones fs x
mapearFunciones [] _ = []

mapearFuncionesNR :: t1 -> (t1 -> t2) -> t2
mapearFuncionesNR x f = f x
--

aplicarFuncAcum :: [t -> t] -> t -> [t]
aplicarFuncAcum (f:fs) x = f x : mapearFunciones fs (f x)
aplicarFuncAcum [] _ = []

concederDeseoBienR :: Chico -> Chico
concederDeseoBienR chico = last (aplicarFuncAcum (deseos chico) chico)

concederUnicoDeseoBienR :: Chico -> Chico
concederUnicoDeseoBienR chico = last (aplicarFuncAcum (deseos chico) chico)

alterarEdad :: Int -> Chico -> Chico
alterarEdad n chico = chico {edad = edad chico + n }

--wanda: dado un chico, wanda le cumple el primer deseo y lo hace madurar (crecer un año de edad).
wanda :: Chico -> Chico
wanda chico = alterarEdad 1 (concederUnicoDeseo chico)

wanda2 :: Chico -> Chico
wanda2 (Chico nombre ed hab d) = (alterarEdad 1.concederDeseosR) (Chico nombre ed hab [head d]) -- no le suma los 2 años porque luego de sumar aplica el deseo y lo deja con 18

--cosmo: dado un chico, lo hace “des”madurar, quedando con la mitad de años de edad. Como es olvidadizo, no le concede ningún deseo.
cosmo :: Chico -> Chico
cosmo chico = chico {edad = div (edad chico) 2}

--muffinMagico: dado un chico le concede todos sus deseos.
muffinMagico :: Chico -> Chico
muffinMagico = concederDeseoBienR

--B. En busqueda de pareja

data Chica = Chica {
    nombrec :: String,
    condicion :: Chico -> Bool
}

--1.
--a. tieneHabilidad unaHabilidad unChico: Dado un chico y una habilidad, dice
--si la posee.

tieneHabilidad :: String -> Chico -> Bool
tieneHabilidad habilidad chico = habilidad `elem` habilidades chico

--b. esSuperMaduro: Dado un chico dice si es mayor de edad (es decir, tiene más
--de 18 años) y además sabe manejar.

esSuperMaduro :: Chico -> Bool
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
vicky :: Chica
vicky = Chica "Vicky" (tieneHabilidad "ser un supermodelo noruego")

--quienConquistaA unaChica losPretendientes: Dada una chica y una lista
--de pretendientes, devuelve al que se queda con la chica, es decir, el primero
--que cumpla con la condición que ella quiere. Si no hay ninguno que la cumpla,
--devuelve el último pretendiente (una chica nunca se queda sola). (Sólo en este
--punto se puede usar recursividad)
--b. Dar un ejemplo de consulta para una nueva chica, cuya condición para elegir a
--un chico es que este sepa cocinar.

pretendientes :: [Chico]
pretendientes = [roman, claudio, chad]

quienConquistaA :: Chica -> [Chico] -> Chico
quienConquistaA chica lista
    |   any (condicion chica) lista = head (filter (condicion chica) lista)
    |   otherwise = last lista

catalina :: Chica
catalina = Chica "Catalina" (tieneHabilidad "saber cocinar")

{-C. Da Rules
Como no todo está permitido en el mundo mágico, Jorgen
Von Strángulo está encargado de controlar que no se
viole lo establecido en “da Rules”:
1. infractoresDeDaRules : Dada una lista de
chicos, devuelve la lista de los nombres de
aquellos que tienen deseos prohibidos. Un deseo
está prohibido si, al aplicarlo, entre las
cinco primeras habilidades, hay alguna prohibida.
En tanto, son habilidades prohibidas enamorar,
matar y dominar el mundo.-}

habilidadesProhibidas :: [String]
habilidadesProhibidas = ["enamorar","matar","dominar el mundo"]

esInfractor :: Chico -> Bool
esInfractor chico = any (algunaHabProh (take 5 (habilidades chico))) habilidadesProhibidas
algunaHabProh :: (Foldable t, Eq a) => t a -> a -> Bool
algunaHabProh lista a = a `elem` lista
infractoresDeDaRules :: [Chico] -> [String]
infractoresDeDaRules lista = map nombre (filter esInfractor lista)

{-
D. Justificaciones
Indicar donde se utilizó y para qué:
● la función composición
● funciones de orden superior (creadas)
● aplicación parcial
● listas infinitas: dar ejemplos de consultas que funcionen y que no, donde aparezcan
estas listas
-}

