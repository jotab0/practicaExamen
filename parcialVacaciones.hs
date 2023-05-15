import Text.Show.Functions ()
data Turista = Turista {
    cansancio :: Int,
    stress :: Int,
    viajaSolo :: Bool,
    idiomas :: [String]
} deriving Show

alterarCansancio :: Int -> Turista -> Turista
alterarCansancio n turista
    | cansancio turista - n > 0 = turista { cansancio = cansancio turista + n}
    | otherwise = turista { cansancio = 0 }
alterarStress :: Int -> Turista -> Turista
alterarStress n turista
    | stress turista - n > 0 = turista { stress = stress turista + n }
    | otherwise = turista { stress = 0 }
acompañado :: Turista -> Turista
acompañado turista = turista { viajaSolo = True }
agregarIdioma :: String -> Turista -> Turista
agregarIdioma idioma turista  = turista { idiomas = idioma : idiomas turista}

type Excursion = Turista -> Turista

--Ir a la playa: si está viajando solo baja el cansancio en 5 unidades, si no baja el stress 1 unidad.
irALaPlaya :: Excursion
irALaPlaya turista
    | viajaSolo turista = alterarCansancio (-5) turista
    | otherwise = alterarStress (-1) turista

--Apreciar algún elemento del paisaje: reduce el stress en la cantidad de letras de lo que se aprecia. 
apreciaPaisaje :: String -> Excursion
apreciaPaisaje paisaje = alterarStress (-(length paisaje))

--Salir a hablar un idioma específico: el turista termina aprendiendo dicho idioma y continúa el viaje acompañado.
salirAHablar :: String -> Excursion
salirAHablar idioma = acompañado.agregarIdioma idioma
{-
Caminar ciertos minutos: aumenta el cansancio pero reduce el stress según 
la intensidad de la caminad, ambos en la misma cantidad. El nivel de intensidad se 
calcula en 1 unidad cada 4 minutos que se caminen.
-}

caminarNMinutos :: Int -> Turista -> Turista
caminarNMinutos n = alterarStress (-(n`div`4)).alterarCansancio (n `div` 4)

{-
Paseo en barco: depende de cómo esté la marea
si está fuerte, aumenta el stress en 6 unidades y el cansancio en 10.
si está moderada, no pasa nada.
si está tranquila, el turista camina 10’ por la cubierta, aprecia la vista del “mar”, 
y sale a hablar con los tripulantes alemanes.
-}

--paseoEnBarco 
data Marea = Fuerte | Moderada | Tranquila

paseoEnBarco :: Marea -> Turista -> Turista
paseoEnBarco Fuerte turista = (alterarCansancio 10.alterarStress 6) turista
paseoEnBarco Moderada turista = turista
paseoEnBarco Tranquila turista = (apreciaPaisaje "mar".caminarNMinutos 10. salirAHablar "aleman") turista

{-
Crear un modelo para los turistas y crear los siguientes tres ejemplos:
Ana: está acompañada, sin cansancio, tiene 21 de stress y habla español.
Beto y Cathi, que hablan alemán, viajan solos, y Cathi además habla catalán. 
Ambos tienen 15 unidades de cansancio y stress.
-}

fabian :: Turista
fabian = Turista { cansancio = 50 , stress = 100, viajaSolo = False, idiomas = ["espaniol","Portugues"] }

ana :: Turista
ana = Turista { cansancio = 20 , stress = 200, viajaSolo = False, idiomas = ["espaniol"] }

beto :: Turista
beto = Turista { cansancio = 34, stress = 153, viajaSolo = True, idiomas = ["aleman"] }

cathi :: Turista
cathi = Turista { cansancio = 29, stress = 120, viajaSolo = True, idiomas = ["aleman", "catalan"] }

{-
Modelar las excursiones anteriores de forma tal que para agregar una 
excursión al sistema no haga falta modificar las funciones existentes. Además:
Hacer que un turista haga una excursión. Al hacer una excursión, 
el turista además de sufrir los efectos propios de la excursión, reduce en un 10% su stress.
-}

hacerExcursion :: Excursion -> Turista -> Turista
hacerExcursion f turista = alterarStress (-(stress turista * 10 `div` 100)) (f turista)

{-
Dada la función

deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2

Definir la función deltaExcursionSegun que a partir de un índice, un turista 
y una excursión determine cuánto varió dicho índice después de que el turista 
haya hecho la excursión. Llamamos índice a cualquier función que devuelva un número a partir de un turista.

Por ejemplo, si “stress” es la función que me da el stress de un turista:
> deltaExcursionSegun stress ana irALaPlaya = 3
(porque al ir a la playa Ana queda con 18 de estrés (21 menos 1 menos 10% de 20))
-}

deltaExcursionSegun :: Num a => (Turista -> a) -> Turista -> Excursion -> a
deltaExcursionSegun indice turista excursion = abs (indice turista - indice (hacerExcursion excursion turista))

{-
Usar la función anterior para resolver cada uno de estos puntos:
- Saber si una excursión es educativa para un turista, que implica que termina aprendiendo algún idioma.
- Conocer las excursiones desestresantes para un turista (aquellas que le reducen al 
menos 3 unidades de stress al turista.)
-}

esEducativa :: Excursion -> Turista -> Bool
esEducativa excursion turista = deltaExcursionSegun (length.idiomas) turista excursion > 0
esDesestresante :: Excursion -> Turista -> Bool
esDesestresante excursion turista = deltaExcursionSegun stress turista excursion > 0 && 0 < stress turista - stress (hacerExcursion excursion turista)

{-
Para mantener a los turistas ocupados todo el día, la empresa vende 
paquetes de excursiones llamados tours. Un tour se compone por una serie de excursiones.
Completo: Comienza con una caminata de 20 minutos para apreciar una "cascada", 
luego se camina 40 minutos hasta una playa, y finaliza con una salida con gente local que habla "melmacquiano".
Lado B: Este tour consiste en ir al otro lado de la isla a hacer alguna 
excursión (de las existentes) que elija el turista. Primero se hace un paseo en barco por 
aguas tranquilas (cercanas a la costa) hasta la otra punta de la isla, luego realiza la 
excursión elegida y finalmente vuelve caminando hasta la otra punta, tardando 2 horas.
Isla Vecina: Se navega hacia una isla vecina para hacer una excursión. Esta excursión 
depende de cómo esté la marea al llegar a la otra isla: si está fuerte se aprecia un 
"lago", sino se va a una playa. En resumen, este tour implica hacer un paseo en barco 
hasta la isla vecina, luego llevar a cabo dicha excursión, y finalmente volver a hacer un 
paseo en barco de regreso. La marea es la misma en todo el camino.
-}
type Tour = [Excursion]

tourCompleto :: [Excursion]
tourCompleto = [caminarNMinutos 20, apreciaPaisaje "cascada", caminarNMinutos 40,irALaPlaya, salirAHablar "melmacquiano"]

ladoB :: Excursion -> [Excursion]
ladoB excursionElegida = [paseoEnBarco Tranquila, excursionElegida, caminarNMinutos 120]

islaVecina :: Marea -> [Excursion]
islaVecina Fuerte = [paseoEnBarco Fuerte,apreciaPaisaje "lago",paseoEnBarco Fuerte]
islaVecina condicionMarea = [paseoEnBarco condicionMarea,irALaPlaya,paseoEnBarco condicionMarea]

conjTours1 :: [[Excursion]]
conjTours1 = [tourCompleto, ladoB irALaPlaya, islaVecina Fuerte]
{-
Modelar los tours para:
Hacer que un turista haga un tour. Esto implica, primero un aumento del stress en 
tantas unidades como cantidad de excursiones tenga el tour, y luego realizar las excursiones en orden.
-}

turistaHaceTour :: Foldable t => Turista -> t Excursion -> Turista
turistaHaceTour turista tour = foldl (flip hacerExcursion) (alterarStress (length tour) turista) tour

turistaHaceTour2 :: Turista -> [Excursion] -> Turista
turistaHaceTour2 turista tour = foldl (flip ($)) turista (map hacerExcursion tour)
-- Interesantes ambas funciones

{-
Dado un conjunto de tours, saber si existe alguno que sea convincente para un turista. 
Esto significa que el tour tiene alguna excursión desestresante la cual, además, deja al
turista acompañado luego de realizarla.
-}
quedaAcompañado :: (t -> Turista) -> t -> Bool
quedaAcompañado excursion turista = viajaSolo (excursion turista)

esConvincenteElTour :: [Excursion] -> Turista -> Bool
esConvincenteElTour tour turista = not (null (filter (flip quedaAcompañado turista) (filter (flip esDesestresante turista) tour)))

esConvincenteElTour2 :: [Excursion] -> Turista -> Bool
esConvincenteElTour2 tour turista = any (`quedaAcompañado` turista) (filter (`esDesestresante` turista) tour)

esConvincenteAlgunTour :: [[Excursion]] -> Turista -> [[Excursion]]
esConvincenteAlgunTour tours turista = filter (flip esConvincenteElTour turista) tours

aplicarFuncion :: (t1 -> t2) -> t1 -> t2
aplicarFuncion f x = f x
--

{-
Saber la efectividad de un tour para un conjunto de turistas. Esto se calcula como la sumatoria 
de la espiritualidad recibida de cada turista a quienes les resultó convincente el tour. 
La espiritualidad que recibe un turista es la suma de las pérdidas de stress y cansancio tras el tour.
-}

conjTuristas :: [Turista]
conjTuristas = [ana, fabian, beto , cathi]

sumaStress :: [Turista] -> Int
sumaStress turistas = sum (map stress turistas)

turistasHacenTour :: Foldable t => [Turista] -> t Excursion -> [Turista]
turistasHacenTour turistas tour = map (`turistaHaceTour` tour) turistas

-- Esta funcion no hace falta
turistasHacenTours :: (Foldable t1, Foldable t2) =>t1 (t2 Excursion) -> [Turista] -> [Turista]
turistasHacenTours tours turistas = foldl turistasHacenTour turistas tours
--
filtrarTuristasConvencidos :: [Turista] -> [Excursion] -> [Turista]
filtrarTuristasConvencidos turistas tour = filter (esConvincenteElTour tour) turistas

-- Esta tampoco hace falta
filtrarTuristasConvencidosConjTour :: Foldable t => [Turista] -> t [Excursion] -> [Turista]
filtrarTuristasConvencidosConjTour turistas tours = foldl (filtrarTuristasConvencidos) turistas tours
--

sumaPerdidasStressYCansancio :: Foldable t => Turista -> t Excursion -> Int
sumaPerdidasStressYCansancio turista tour
    |   stress turista - stress (turistaHaceTour turista tour) >= 0 && cansancio turista - cansancio (turistaHaceTour turista tour) >= 0 = (stress turista - stress (turistaHaceTour turista tour)) + (cansancio turista - cansancio (turistaHaceTour turista tour))
    |   otherwise = 0

efectividadTour :: [Turista] -> [Excursion] -> Int
efectividadTour turistas tour = sum (map ($ tour) (map sumaPerdidasStressYCansancio (filtrarTuristasConvencidos turistas tour)))
-- En esta ultima funcion se mapera sumaPerdidasStressYCansancio al conj de turistas filtrado y luego se
-- le pasa como argumento tour a través de map a cada funcion mapeada en la lista. Luego se suma todo
