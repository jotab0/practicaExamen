
data Pelicula = Pelicula {
    nombre :: String,
    actores :: [String],
    duracion :: Int,
    añoDeEstreno :: Int,
    premios :: [String]
} deriving Show

taxiDriver :: Pelicula
taxiDriver = Pelicula "Taxi driver" ["De Niro", "Foster"] 113 1976 []
machete :: Pelicula
machete = Pelicula "Machete" ["De Niro", "Rodriguez"] 105 2010 []
harryPotter :: Pelicula
harryPotter = Pelicula "Harry Potter 9" ["Watson", "Radcliffe", "Grint","chewbaca"] 1000 2022 []
peliculaComedia :: Pelicula
peliculaComedia = Pelicula "Comedia100" ["Carrey", "Grint"] 30 2022 []
triChamp :: Pelicula
triChamp = Pelicula "Comedia100" ["Carrey", "Grint","the rock","rocky","piedrita rodriguez"] 200 1974 []

--1
actorTrabajo :: String -> Pelicula -> Bool
actorTrabajo actor pelicula = actor `elem` actores pelicula

-- 2 MAL PORQUE SE PUEDE RESOLVER CON FILTER Y MAP
generosSegunActores :: (Foldable t1, Eq t2) => [t2] -> [(a, t1 t2)] -> [[a]]
generosSegunActores _ [] = []
generosSegunActores [] _ = []
generosSegunActores (x:xs) lista = sacarGenero x lista : generosSegunActores xs lista

sacarGenero :: (Foldable t1, Eq t2) => t2 -> [(a, t1 t2)] -> [a]
sacarGenero _ [] = []
sacarGenero actor (x:xs)
    | actor `elem` snd x = fst x : sacarGenero actor xs
    | otherwise = sacarGenero actor xs

--concat (generosSegunActores (actores taxiDriver) todosLosActores) 
--["drama","drama"]

-- lambda redundante pero sirve para entender
-- esDelGenero = (\pelicula -> concat (generosSegunActores (actores pelicula) todosLosActores)) 

tamañoGeneros :: Pelicula -> Int
tamañoGeneros = length.(\pelicula -> concat (generosSegunActores (actores pelicula) todosLosActores))

-- 2 SIN RECURSIV

-- Película es de cierto genero --> si la mayoria de los actores son de un determ genero
-- Actor puede tener varios generos --> ver cual genero aparece mas veces
-- Ver por actor los generos posibles de una pelicula

todosLosActores :: [(String, [String])]
todosLosActores = [("comedia", ["Carrey", "Grint", "Stiller"]),("accion", ["Stallone","Carrey", "Willis","Schwarzenegger"]), ("drama", ["De Niro", "Foster"])]

devolverDondeAparece :: String -> [String]
devolverDondeAparece actor = map fst (filter (obtenerCoincidencia actor) todosLosActores)

obtenerCoincidencia :: (Foldable t, Eq a1) => a1 -> (a2, t a1) -> Bool
obtenerCoincidencia actor (_,actores) = actor `elem` actores

generoSegunActores :: Pelicula -> [String]
generoSegunActores pelicula = concatMap devolverDondeAparece (actores pelicula)

esGenero :: String -> Pelicula -> Bool
esGenero genero pelicula = length (filter (genero ==) (generoSegunActores pelicula)) > length (generoSegunActores pelicula) `div` 2

-- 3

-- Funciones con premios

clasicoSetentista :: Pelicula -> Bool
clasicoSetentista pelicula = añoDeEstreno pelicula <= 1979 && añoDeEstreno pelicula >= 1970
plomo :: Pelicula -> Bool
plomo pelicula = duracion pelicula > 180
tresSonMultitud :: Pelicula -> Bool
tresSonMultitud pelicula = length (actores pelicula) > 3
nSonMultitud :: Int -> Pelicula -> Bool
nSonMultitud n pelicula = length (actores pelicula) > n

peliculaGanoPremio :: Pelicula -> Bool
peliculaGanoPremio pelicula
    |   clasicoSetentista pelicula = True
    |   plomo pelicula = True
    |   tresSonMultitud pelicula = True
    |   otherwise = False

-- 4

festivalDeCannes :: [Pelicula -> Bool]
festivalDeCannes = [clasicoSetentista, tresSonMultitud]

festivalDeBerlin :: [Pelicula -> Bool]
festivalDeBerlin = [nSonMultitud 4, plomo, clasicoSetentista]

aplicarArgumentoAListaDeF :: t -> [t -> a] -> [a]
aplicarArgumentoAListaDeF _ [] = []
aplicarArgumentoAListaDeF a (f:fs) = f a : aplicarArgumentoAListaDeF a fs

cuantosPremios :: [t -> Bool] -> t -> Int
cuantosPremios festival pelicula = length (filter id (aplicarArgumentoAListaDeF pelicula festival))

nuevoPremio :: Pelicula -> Bool
nuevoPremio pelicula = ((>15).length.concat) (actores pelicula)

nuevoFestival :: [Pelicula -> Bool]
nuevoFestival = [nuevoPremio]

-- Si quisiera agregar premios:
agregarPremio :: Pelicula -> String -> Pelicula
agregarPremio pelicula premio = pelicula {premios = premios pelicula ++ [premio]}

