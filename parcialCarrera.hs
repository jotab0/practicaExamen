-- 1
import Text.Show.Functions ()

data Auto = Auto {
    velocidad :: Int,
    distancia :: Int,
    color :: String
} deriving (Eq, Show)

autoAmarillo :: Auto
autoAmarillo = Auto 180 200 "amarillo"
autoNegro :: Auto
autoNegro = Auto 220 300 "negro"
autoAzul :: Auto
autoAzul = Auto 150 225 "azul"
autoBlanco:: Auto
autoBlanco = Auto 300 230 "blanco"

type Carrera = [Auto]
carrera :: Carrera
carrera = [autoAmarillo, autoAzul, autoBlanco, autoNegro]

estaCerca :: Auto -> Auto -> Bool
estaCerca auto1 auto2 = (color auto1 /= color auto2) && 100 > abs (distancia auto1 - distancia auto2)

distanciaEsMayor :: Auto -> Auto -> Bool
distanciaEsMayor auto1 auto2 = distancia auto1 > distancia auto2

segundoArgumentoVaAdelante :: Auto -> Auto -> Bool
segundoArgumentoVaAdelante auto1 auto2
  | distancia auto1 < distancia auto2 = True
  | distancia auto1 == distancia auto2 && velocidad auto1 /= velocidad auto2 = segundoArgumentoVaAdelante (transformarVelocEnPosic auto1) (transformarVelocEnPosic auto2)
  | otherwise = False

transformarVelocEnPosic :: Auto -> Auto
transformarVelocEnPosic auto = auto {distancia = velocidad auto}

noEsMimsoAuto :: Auto -> Auto -> Bool
noEsMimsoAuto auto1 auto2 = color auto1 /= color auto2

vaTranquilo :: Auto -> Bool
vaTranquilo auto
  = and (map (not . estaCerca auto) carrera) && and (map (distanciaEsMayor auto) (filter (noEsMimsoAuto auto) carrera))

puesto :: String -> [Auto] -> Int
puesto col lista
  = 1 + length (filter (segundoArgumentoVaAdelante (head (filter (\auto -> color auto == col) lista))) lista)

autoDeColor :: String -> Auto -> Bool
autoDeColor col auto = color auto == col

filtrarAutoDeColor :: String -> [Auto] -> Auto
filtrarAutoDeColor col = head.filter  (autoDeColor col)

filtrarAutoDeColor2 :: String -> [Auto] -> [Auto] -- Para cuando quiera filtrar auto por color pero dejarlo en lista (sirve para powerups)
filtrarAutoDeColor2 col = filter  (autoDeColor col)

puesto2 :: String -> [Auto] -> Int
puesto2 col lista = length lista - length (filter (distanciaEsMayor (filtrarAutoDeColor col lista)) lista)

--2
correrAuto :: Int -> Auto -> Auto
correrAuto n auto = auto {distancia = distancia auto + (n * velocidad auto)}

modificadorVelocidad :: Auto -> Int -> Auto
modificadorVelocidad auto n = auto {velocidad = velocidad auto + n}

bajarVelocidad :: Int -> Auto -> Auto
bajarVelocidad n auto
    | velocidad (modificadorVelocidad auto (-n)) > 0 = modificadorVelocidad auto (-n)
    | otherwise = auto {velocidad = 0}

--3 

-- Se nos provee esta función:
afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista
  = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

-- TERREMOTO

-- Sin funcion afectar a los que cumplen

terremoto :: Auto -> [Auto]
terremoto auto = (map (bajarVelocidad 50).filter (estaCerca auto)) carrera ++ filter (not.estaCerca auto) carrera
-- Sin querer hice la funcion anterior pero no genérica

terremoto2 :: String -> [Auto] -> [Auto]
terremoto2 col lista = (map (bajarVelocidad 50).filter (estaCerca (head (filtrarAutoDeColor2 col lista)))) lista ++ filter (not.estaCerca (head (filter (\auto -> color auto == col) lista))) lista

terremoto3 :: String -> [Auto] -> [Auto]
terremoto3 col lista = (map (bajarVelocidad 50).filter (estaCerca (filtrarAutoDeColor col lista))) lista ++ filter (not.estaCerca (filtrarAutoDeColor col lista)) lista


-- Con funcion afectar a los que cumplen
{-}
terremotoV2 :: Auto -> [Auto]
terremotoV2 auto = afectarALosQueCumplen (estaCerca auto) (\autof -> autof { velocidad = velocidad autof - 50}) carrera
-} -- Esta funcion está mal porque para que después los efectos sean acumulativos, debe recibir una lista y no aplicar los efectos a carrera.

terremotoV22 :: String -> [Auto] -> [Auto]
terremotoV22 col lista = afectarALosQueCumplen (estaCerca (filtrarAutoDeColor col lista)) (bajarVelocidad 50) lista

-- MIGUELITOS
{-
miguelitos :: Int -> Auto -> [Auto]
miguelitos n auto = map ((\decremento autos -> autos { velocidad = velocidad auto - decremento}) n) (filter (distanciaEsMayor auto) carrera) ++ filter (not.distanciaEsMayor auto) carrera

miguelitos2 :: Int -> Auto -> [Auto]
miguelitos2 n auto = map (bajarVelocidad n) (filter (distanciaEsMayor auto) carrera) ++ filter (not.distanciaEsMayor auto) carrera
-} -- Mal porque deben recibir la lista en la que se va a aplicar

miguelitos3 :: Int -> String -> [Auto] -> [Auto]
miguelitos3 n col lista = map (bajarVelocidad n) (filter (distanciaEsMayor (head (filter (\auto -> color auto == col) lista))) lista)  ++ filter (not.distanciaEsMayor (head (filter (\auto -> color auto == col) lista))) lista

--JETPACK

correrAutoJetpack :: Int -> Auto -> Auto
correrAutoJetpack n auto = auto {distancia = distancia auto + (n * (2*velocidad auto))}
{-
jetPack :: Int -> Auto -> [Auto]
jetPack n auto = map (correrAutoJetpack n) (filter (auto ==) carrera) ++ filter (auto /=) carrera
-}
jetPack2 :: Int -> String -> [Auto] -> [Auto]
jetPack2 n col lista = map (correrAutoJetpack n) (filter (\auto -> color auto == col) lista) ++ filter (\auto -> color auto /= col) lista

jetPack22 :: Int -> String -> [Auto] -> [Auto]
jetPack22 n col lista = map (correrAutoJetpack n) (filtrarAutoDeColor2 col lista) ++ filter (\auto -> color auto /= col) lista

{-
NOTA IMPORTANTE: Las funciones deben recibir el color del auto (funciona como identificador) porque
una vez que se alteraron los parámetros del auto, deja de llamarse como se llamaba, por ejemplo: 
autoAzul == head (filter (\auto -> color auto == "azul") (jetPack 3 autoAzul)) =(consola)= False
entonces cuando queramos aplciar de manera acumulada los power ups, si usamos el nombre del dato, ej autoAzul, no
va a funcionar
-}

--4

aplicarListaDeFAArg :: [t -> t] -> t -> [t]
aplicarListaDeFAArg (f:fs) x = f x : aplicarListaDeFAArg fs x
aplicarListaDeFAArg [] _ = []

correnTodos :: Int -> [Auto] -> [Auto]
correnTodos n = map (correrAuto n)

usaPowerup :: (t1 -> t2) -> t1 -> t2
usaPowerup f = f

listaEventos :: [[Auto] -> [Auto]]
listaEventos = [map (correrAuto 30), jetPack2 3 "azul", terremoto2 "blanco",map (correrAuto 40), miguelitos3 20 "blanco" ,jetPack2 6 "azul",map (correrAuto 10)]

listaEventos2 :: [[Auto] -> [Auto]]
listaEventos2 =
  [correnTodos 30,
  usaPowerup (jetPack2 3) "azul",
  usaPowerup terremoto2 "blanco",
  correnTodos 40,
  usaPowerup (miguelitos3 20) "blanco",
  usaPowerup (jetPack2 6) "azul",
  correnTodos 10]

simularCarreraPasoAPaso :: t -> [t -> t] -> [t]
simularCarreraPasoAPaso listaAutos [] = [listaAutos]
simularCarreraPasoAPaso listaAutos (f:fs) = listaAutos : simularCarreraPasoAPaso (f listaAutos) fs

simularCarreraPasoAPaso2 :: t -> [t -> t] -> [t]
simularCarreraPasoAPaso2 listaAutos [] = [listaAutos]
simularCarreraPasoAPaso2 listaAutos listaDeEventos = head (map ($ listaAutos) listaDeEventos) : simularCarreraPasoAPaso2 (head(map ($ listaAutos) listaDeEventos)) (drop 1 listaDeEventos)

aplicarListaDeFAArgs :: [t -> a] -> [t] -> [a]
aplicarListaDeFAArgs [] _ = []
aplicarListaDeFAArgs _ [] = []
aplicarListaDeFAArgs (f:fs) (x:xs) = f x : aplicarListaDeFAArgs fs xs

tuplarPosiciones :: [Auto] -> [Auto] -> [(Int, String)]
tuplarPosiciones (x:xs) lista = (puesto (color x) lista, color x) : tuplarPosiciones xs lista
tuplarPosiciones [] _ = []

seguirCarrera :: [Auto] -> [Carrera -> Carrera] -> [[(Int, String)]]
seguirCarrera listaAutos listaDeEventos =  aplicarListaDeFAArgs (map tuplarPosiciones (simularCarreraPasoAPaso listaAutos listaDeEventos)) (simularCarreraPasoAPaso2 listaAutos listaDeEventos)

listaTuplas :: [Auto] -> [Carrera -> Carrera] -> [(Int, String)]
listaTuplas listaAutos listaDeEventos = concat (seguirCarrera listaAutos listaDeEventos)

finCarrera :: Foldable t => t (b -> b) -> b -> b -- Solo da el estado final de la carrera
finCarrera listaDeEventos listaAutos = foldl (flip ($)) listaAutos listaDeEventos -- listaAutos $ evento1 $ evento2

{-
NOTAS

- Cuando voy a tener que aplicar funciones acumulativas a elementos en particular, voy a tener que tener
identificadores en el data

- Se puede usar argumento recibido en funcion como argumento en una lambda dentro de esa función

-}