import Text.Show.Functions ()

{-Lisa Simpson se propuso desarrollar un programa que le permita ayudar a su hermano a 
vencer a su vecino Todd en un torneo de minigolf. Para hacerlo más interesante, 
los padres de los niños hicieron una apuesta: el padre del niño que no gane deberá 
cortar el césped del otro usando un vestido de su esposa.

De los participantes nos interesará el nombre del jugador, 
el de su padre y sus habilidades (fuerza y precisión). 
-}
-- Modelo inicial
data Jugador = Jugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Float,
  precisionJugador :: Float
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart :: Jugador
bart = Jugador "Bart" "Homero" (Habilidad 25 60)
todd :: Jugador
todd = Jugador "Todd" "Ned" (Habilidad 15 80)
rafa :: Jugador
rafa = Jugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = Tiro {
  velocidad :: Float,
  precision ::  Float,
  altura :: Float,
  superaObstaculo :: Bool
} deriving (Eq, Show)

type Puntos = Int

-- Funciones útiles
between :: (Eq a, Enum a) => a -> a -> a -> Bool
between n m x = x `elem` [n .. m]

maximoSegun :: (Foldable t, Ord a1, Num a2) => (a2 -> a1) -> t a2 -> a2
maximoSegun f lista = foldl (mayorSegunF f) 0 lista -- (0 (mayorSegun f) 1) (mayorSegun f) 2 (mayorSegun f) 3
-- En este caso no hace falta flip porque no importa como ingresa a y b,
-- porque son argumentos, el tema es cuando hay funciones

mayorSegunF :: Ord a => (t -> a) -> t -> t -> t
mayorSegunF f a b
  | f a > f b = a
  | otherwise = b

-- maximoSegun (+1) [1,2,3] = 3
-- Notar que cuando está la función fold es para aplicarla a listas

{-
También necesitaremos modelar los palos de golf que pueden usarse y los 
obstáculos que deben enfrentar para ganar el juego.

Sabemos que cada palo genera un efecto diferente, por lo tanto elegir 
el palo correcto puede ser la diferencia entre ganar o perder el torneo.
Modelar los palos usados en el juego que a partir de una determinada habilidad 
generan un tiro que se compone por velocidad, precisión y altura.
El putter genera un tiro con velocidad igual a 10, el doble de la precisión recibida y altura 0.
La madera genera uno de velocidad igual a 100, altura igual a 5 y la mitad de la precisión.
Los hierros, que varían del 1 al 10 (número al que denominaremos n), generan un tiro de 
velocidad igual a la fuerza multiplicada por n, la precisión dividida por n 
y una altura de n-3 (con mínimo 0). Modelarlos de la forma más genérica posible.
-}

-- PARA MÍ VA A SER MEJOR CON TYPE

data Palo1 = Palo1 {
    habilidadRecibida :: Habilidad,
    tiro :: Tiro
}

putter1 :: Habilidad -> Palo1
putter1 hab = Palo1 hab (Tiro 10 (precisionJugador hab * 2) 0 True)

madera1 :: Habilidad -> Palo1
madera1 hab = Palo1 hab (Tiro 100 (precisionJugador hab / 2) 5 True)
--
-- CON TYPE

type Palo = (Habilidad -> Tiro)

putter :: Palo
putter hab = Tiro 10 (precisionJugador hab * 2) 0 True

madera :: Palo
madera hab = Tiro 100 (precisionJugador hab / 2) 5 True

hierro :: Float -> Palo
hierro n hab = Tiro (fuerzaJugador hab * n) (precisionJugador hab / n) (n-3) True

{-
Definir una constante palos que sea una lista 
con todos los palos que se pueden usar en el juego.
-}

palos :: [Palo]
palos = [putter, madera, hierro 1, hierro 2, hierro 3, hierro 4, hierro 5,
         hierro 6, hierro 7, hierro 8, hierro 9, hierro 10]

{-
Definir la función golpe que dados una persona y un palo, obtiene el tiro resultante 
de usar ese palo con las habilidades de la persona.
Por ejemplo: 
Si Bart usa un putter, se genera un tiro de velocidad = 10, precisión = 120 y altura = 0.
-}

golpe :: Jugador -> Palo -> Tiro
golpe persona palo = palo (habilidad persona)

{-
Lo que nos interesa de los distintos obstáculos es si un tiro puede superarlo, 
y en el caso de poder superarlo, cómo se ve afectado dicho tiro por el obstáculo. 
En principio necesitamos representar los siguientes obstáculos:
Un túnel con rampita sólo es superado si la precisión es mayor a 90 yendo al ras del suelo, 
independientemente de la velocidad del tiro. Al salir del túnel la velocidad del tiro se duplica, 
la precisión pasa a ser 100 y la altura 0.
Una laguna es superada si la velocidad del tiro es mayor a 80 y 
tiene una altura de entre 1 y 5 metros. Luego de superar una laguna el tiro 
llega con la misma velocidad y precisión, pero una altura equivalente a la altura 
original dividida por el largo de la laguna.
Un hoyo se supera si la velocidad del tiro está entre 5 y 20 m/s yendo al ras del suelo 
con una precisión mayor a 95. Al superar el hoyo, el tiro se detiene, quedando con todos 
sus componentes en 0.
Se desea saber cómo queda un tiro luego de intentar superar un obstáculo, teniendo en 
cuenta que en caso de no superarlo, se detiene, quedando con todos sus componentes en 0.
-}

{-
Un túnel con rampita sólo es superado si la precisión es mayor a 90 yendo al ras del suelo, 
independientemente de la velocidad del tiro. Al salir del túnel la velocidad del tiro se duplica, 
la precisión pasa a ser 100 y la altura 0
-}

ponerA0 :: Tiro -> Tiro
ponerA0 tiro = tiro {velocidad = 0, precision = 0, altura = 0, superaObstaculo = False}

tiroSuperaRampita :: Tiro -> Bool
tiroSuperaRampita tiro = precision tiro > 90 && altura tiro == 0

tunelConRampita :: Tiro -> Tiro
tunelConRampita tiro
    | tiroSuperaRampita tiro && superaObstaculo tiro = tiro {velocidad = velocidad tiro * 2, precision = 100}
    | otherwise = ponerA0 tiro

{-
Una laguna es superada si la velocidad del tiro es mayor a 80 y 
tiene una altura de entre 1 y 5 metros. Luego de superar una laguna el tiro 
llega con la misma velocidad y precisión, pero una altura equivalente a la altura 
original dividida por el largo de la laguna.
-}

tiroSuperaLaguna :: Tiro -> Bool
tiroSuperaLaguna tiro = velocidad tiro > 80 &&  (altura tiro < 5 && altura tiro > 1)

laguna :: Float -> Tiro -> Tiro
laguna largoLaguna tiro
    | tiroSuperaLaguna tiro && superaObstaculo tiro = tiro {altura = altura tiro / largoLaguna}
    | otherwise = ponerA0 tiro

{-
Un hoyo se supera si la velocidad del tiro está entre 5 y 20 m/s yendo al ras del suelo 
con una precisión mayor a 95. Al superar el hoyo, el tiro se detiene, quedando con todos 
sus componentes en 0.
-}

tiroSuperaHoyo :: Tiro -> Bool
tiroSuperaHoyo tiro = (velocidad tiro > 5 && velocidad tiro < 20) &&  altura tiro == 0 && precision tiro > 95

hoyo :: Tiro -> Tiro
hoyo tiro
    | tiroSuperaHoyo tiro && superaObstaculo tiro = tiro {velocidad = 0, precision = 0, altura = 0, superaObstaculo = True}
    | otherwise = ponerA0 tiro

{-
Definir palosUtiles que dada una persona y un obstáculo, 
permita determinar qué palos le sirven para superarlo.
Saber, a partir de un conjunto de obstáculos y un tiro, 
cuántos obstáculos consecutivos se pueden superar.
Por ejemplo, para un tiro de velocidad = 10, precisión = 95 y altura = 0, 
y una lista con dos túneles con rampita seguidos de un hoyo, el resultado 
sería 2 ya que la velocidad al salir del segundo túnel es de 40, por ende no supera el hoyo.
(BONUS: resolver este problema sin recursividad, teniendo en cuenta que 
existe una función takeWhile :: (a -> Bool) -> [a] -> [a] que podría ser de utilidad.)
Definir paloMasUtil que recibe una persona y una lista de obstáculos 
y determina cuál es el palo que le permite superar más obstáculos con un solo tiro.
-}

{-
Definir palosUtiles que dada una persona y un obstáculo, 
permita determinar qué palos le sirven para superarlo.
-}

palosUtiles :: Jugador -> (Tiro -> Bool) -> [Palo]
palosUtiles persona obstaculo = filter (obstaculo.golpe persona) palos

{-
Saber, a partir de un conjunto de obstáculos y un tiro, 
cuántos obstáculos consecutivos se pueden superar.
-}

obstaculosConsecutivos :: t -> [t -> Tiro] -> Int
obstaculosConsecutivos tiro obstaculos = length (recorrerHastaFalso tiro obstaculos)

recorrerHastaFalso :: t -> [t -> Tiro] -> [Tiro]
recorrerHastaFalso _ [] = []
recorrerHastaFalso tiro (x:xs)
  | superaObstaculo (x tiro) = x tiro : recorrerHastaFalso tiro xs
  | otherwise = []

aplicacInv :: t1 -> (t1 -> t2) -> t2
aplicacInv x f  = f x

{-
ghci> obstaculosConsecutivos (Tiro 10 95 0) [tiroSuperaRampita, tiroSuperaRampita, tiroSuperaHoyo]
ghci> 2
-}

{-
Definir paloMasUtil que recibe una persona y una lista de obstáculos 
y determina cuál es el palo que le permite superar más obstáculos con un solo tiro.
-}

maxTirosConsec :: Jugador -> [Tiro -> Tiro] -> Int
maxTirosConsec persona obstaculos = maximum (map ($ obstaculos) (map obstaculosConsecutivos (map (golpe persona) palos)))

paloMasUtil :: Jugador -> [Tiro -> Tiro] -> [Palo]
paloMasUtil persona obstaculos = filter ((maxTirosConsec persona obstaculos <=).flip obstaculosConsecutivos obstaculos.golpe persona) palos

{-
Dada una lista de tipo [(Jugador, Puntos)] que tiene la 
información de cuántos puntos ganó cada niño al finalizar el torneo, 
se pide retornar la lista de padres que pierden la apuesta por ser el 
“padre del niño que no ganó”. Se dice que un niño ganó el torneo si tiene más puntos que los otros niños.
-}

tuplasConMenosPuntos :: Ord b => [(Jugador, b)] -> [String]
tuplasConMenosPuntos listaConTuplas = map (padre.fst) (filter ((sacarMinimo listaConTuplas ==).snd) listaConTuplas)


sacarMinimo :: Ord a1 => [(a2, a1)] -> a1
sacarMinimo listaConTuplas = minimum (map snd listaConTuplas)

tuplasConMenosPuntos2 :: Ord b => [(Jugador, b)] -> [String]
tuplasConMenosPuntos2 listaConTuplas = map (padre.fst) (filter (filtrado (sacarMinimo listaConTuplas)) listaConTuplas)

filtrado :: Eq a1 => a1 -> (a2, a1) -> Bool
filtrado n tupla = n == snd tupla

--tuplasConMenosPuntos2


