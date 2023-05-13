-- 1

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
estaCerca auto1 auto2 = (color auto1 /= color auto2) && 10 > abs (distancia auto1 - distancia auto2)

distanciaEsMayor :: Auto -> Auto -> Bool
distanciaEsMayor auto1 auto2 = distancia auto1 > distancia auto2

noEsMimsoAuto :: Auto -> Auto -> Bool
noEsMimsoAuto auto1 auto2 = color auto1 /= color auto2

vaTranquilo :: Auto -> Bool
vaTranquilo auto = and (map not (map (estaCerca auto) carrera)) && and (map (distanciaEsMayor auto) (filter (noEsMimsoAuto auto) carrera))


puesto :: String -> [Auto] -> Int
puesto col lista = length lista - length (filter (distanciaEsMayor (head (filter (\auto -> color auto == col) lista))) lista)


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

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista
  = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

-- TERREMOTO

-- Sin funcion afectar a los que cumplen
noEsMismoYEstaCerca :: Auto -> Auto -> Bool
noEsMismoYEstaCerca auto1 auto2 = noEsMimsoAuto auto1 auto2 && estaCerca auto1 auto2
terremoto :: Auto -> [Auto]
terremoto auto = (map (bajarVelocidad 50).filter (noEsMismoYEstaCerca auto)) carrera ++ filter (not.noEsMismoYEstaCerca auto) carrera

terremoto2 :: String -> [Auto] -> [Auto]
terremoto2 col lista = (map (bajarVelocidad 50).filter (noEsMismoYEstaCerca (head (filter (\auto -> color auto == col) lista)))) lista ++ filter (not.noEsMismoYEstaCerca (head (filter (\auto -> color auto == col) lista))) lista

-- Con funcion afectar a los que cumplen
terremotoV2 :: Auto -> [Auto]
terremotoV2 auto = afectarALosQueCumplen (noEsMismoYEstaCerca auto) (\autof -> autof { velocidad = velocidad autof - 50}) carrera

-- MIGUELITOS

miguelitos :: Int -> Auto -> [Auto]
miguelitos n auto = map ((\decremento autos -> autos { velocidad = velocidad auto - decremento}) n) (filter (distanciaEsMayor auto) carrera) ++ filter (not.distanciaEsMayor auto) carrera

miguelitos2 :: Int -> Auto -> [Auto]
miguelitos2 n auto = map (bajarVelocidad n) (filter (distanciaEsMayor auto) carrera) ++ filter (not.distanciaEsMayor auto) carrera

miguelitos3 :: Int -> String -> [Auto] -> [Auto]
miguelitos3 n col lista = map (bajarVelocidad n) (filter (distanciaEsMayor (head (filter (\auto -> color auto == col) lista))) lista)  ++ filter (not.distanciaEsMayor (head (filter (\auto -> color auto == col) lista))) lista

--JETPACK

correrAutoJetpack :: Int -> Auto -> Auto
correrAutoJetpack n auto = auto {distancia = distancia auto + (n * (2*velocidad auto))}


jetPack :: Int -> Auto -> [Auto]
jetPack n auto = map (correrAutoJetpack n) (filter (auto ==) carrera) ++ filter (auto /=) carrera
jetPack2 :: Int -> String -> [Auto] -> [Auto]
jetPack2 n col lista = map (correrAutoJetpack n) (filter (\auto -> color auto == col) lista) ++ filter (\auto -> color auto /= col) lista

--4


aplciarArgAListaDeFunc :: [t -> t] -> t -> [t]
aplciarArgAListaDeFunc (f:fs) x = f x : aplciarArgAListaDeFunc fs x
aplciarArgAListaDeFunc [] _ = []

correnTodos :: Int -> [Auto] -> [Auto]
correnTodos n = map (correrAuto n)

usaPowerup :: t1 -> (t2 -> t1 -> t3) -> t2 -> t3
usaPowerup col f n  = f n col


listaEventos :: [[Auto] -> [Auto]]
listaEventos = [map (correrAuto 30), jetPack2 3 "azul", terremoto2 "blanco",map (correrAuto 40), miguelitos3 20 "blanco" ,jetPack2 6 "azul",map (correrAuto 10)]

simularCarrera :: t -> [t -> t] -> [t]
simularCarrera _ [] = []
simularCarrera listaAutos (f:fs) = listaAutos : simularCarrera (f listaAutos) fs

tuplarPosiciones :: [Auto] -> [(Int, String)]
tuplarPosiciones (x:xs) = (puesto (color x) (x:xs), color x) : tuplarPosiciones xs
tuplarPosiciones [] = []

seguirCarrera :: [Auto] -> [[Auto] -> [Auto]] -> [[(Int, String)]]
seguirCarrera lista eventos = map tuplarPosiciones (simularCarrera lista eventos)


