
import Text.Show.Functions ()

-- PARTE 1

{-
Las leyes
El Congreso de la Nación sanciona diferentes leyes cada año, algunas más extensas o importantes que otras, pero en particular más que el detalle de su articulado nos interesa conocer cierta información clave, como ser el tema que trata, el presupuesto que requiere su implementación y cuáles son los partidos políticos, grupos de poder u otros sectores que la impulsaron o la apoyan. 
Por ejemplo:
La ley de uso medicinal del cannabis implica un presupuesto de 5 unidades, fue apoyada por el partido cambio de todos y el sector financiero.
La ley de educación superior requiere un presupuesto de 30 unidades y fue apoyada por docentes universitarios y el partido de centro federal
A la ley de profesionalización del tenista de mesa, con un presupuesto de 1 unidad la apoya el partido de centro federal, la liga de deportistas autónomos y el club paleta veloz. 
También hay una ley sobre tenis, apoyada por la liga de deportistas autónomos, con un presupuesto de 2.
-}

data Ley = Ley { tema :: String, presupuesto :: Int, apoyo :: [String]}

leyes :: [Ley]
leyes = [leyCannabis, leyEducacionSuperior, leyTenisMesa, leyTenis]

leyCannabis :: Ley
leyCannabis =  Ley "Uso medicinal del cannabis" 5 ["Cambio de todos", "Sector financiero"]
leyEducacionSuperior :: Ley
leyEducacionSuperior = Ley "educación superior" 30 ["Docentes Universitarios", "Partido de centro federal"]
leyTenisMesa :: Ley
leyTenisMesa = Ley "Ley de profesionalizacion del tenista de mesa" 1 ["Partido de centro federal", "Liga de deportistas autónomos", "Club Paleta Veloz"]
leyTenis :: Ley
leyTenis = Ley "tenis" 2 ["Liga de deportistas autónomos"]

{- 
Averiguar si dos leyes son compatibles, que se da cuando tienen al menos un sector en común que 
la apoya y el tema de una de las leyes esté incluido en el otro. Por ejemplo, 
son compatibles la ley de “profesionalización del tenista de mesa” y la de “tenis”.
-}

aplicacInv :: t1 -> (t1 -> t2) -> t2
aplicacInv x f = f x

sectorEnComun :: Ley -> Ley -> Bool
sectorEnComun ley1 ley2 = or (map (aplicacInv (apoyo ley2)) (map elem (apoyo ley1)))

-- SECTOR EN COMUN RECURSIVA

elemConListas :: (Foldable t, Eq a) => [a] -> t a -> [Bool]
elemConListas [] _ = []
elemConListas (x:xs) ley2 = elem x ley2 : elemConListas xs ley2

sectorEnComunR :: Ley -> Ley -> Bool
sectorEnComunR ley1 ley2 = or (elemConListas (apoyo ley1) (apoyo ley2))

--

temaEnComun :: Ley -> Ley -> [Bool]
temaEnComun _ ley2 | null (tema ley2) = []
temaEnComun ley1 ley2 = (tema ley1 == take (length (tema ley1)) (tema ley2)) : temaEnComun ley1 (quitarCaracter ley2)

hayTemaEnComun :: Ley -> Ley -> Bool
hayTemaEnComun ley1 ley2 = or (temaEnComun ley1 ley2)

quitarCaracter :: Ley -> Ley
quitarCaracter ley = ley {tema = drop 1 (tema ley)}

sonCompatibles :: Ley -> Ley -> Bool
sonCompatibles ley1 ley2 = hayTemaEnComun ley1 ley2 || hayTemaEnComun ley2 ley1 && sectorEnComun ley1 ley2
