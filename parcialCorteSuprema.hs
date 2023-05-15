

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

data Ley = Ley { tema :: String, presupuesto :: Int, apoyo :: [String]} deriving Show

conjuntoLeyes :: [Ley]
conjuntoLeyes = [leyCannabis, leyEducacionSuperior, leyTenisMesa, leyTenis, leyAhoraSi]

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
sectorEnComun ley1 ley2 = or (map ($ apoyo ley2) (map elem (apoyo ley1)))

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
sonCompatibles ley1 ley2 = hayTemaEnComun ley2 ley1 && sectorEnComun ley1 ley2

-- PARTE 2
{-
Constitucionalidad de las leyes
La legislación vigente establece que son 5 los jueces que integran la Corte Suprema, 
pero previendo posibles cambios que puedan suceder contemplaremos la posibilidad de que 
la cantidad de integrantes sea diferente.  Es tarea de la corte establecer si una determinada 
ley es constitucional o no. Para ello, cada juez vota de acuerdo a sus principios, experiencia,
intereses o como le dé la gana, y si resultan mayoritarios los votos negativos, 
la ley se declara inconstitucional. En estos casos, ningún juez de la corte puede 
abstenerse o votar en blanco. 
De los jueces no nos interesa saber su información personal, 
su patrimonio ni su autopercepción, sino simplemente como votan.

Algunos de ellos son:
Uno de los jueces se basa en la opinión pública: si el tema de la ley está en agenda, 
seguro que la declara constitucional. (Se conoce el conjunto de temas en agenda, 
que es único para todos)
Otro de los jueces, cuando se entera que la ley fue apoyada por el sector financiero, 
es imposible que la declare inconstitucional.
Hay un juez muy preocupado por las arcas del estado que declara inconstitucionales las 
leyes que requieren un presupuesto de más de 10 unidades. Existe otro juez con mentalidad 
similar pero un poco más tolerante que las declara inconstitucional recién si superan las 
20 unidades de presupuesto.
Y el último de los jueces actuales decide declarar constitucional a toda ley que 
haya sido apoyada por el partido conservador
-}

type Principios = Ley -> Bool
type Juez = Principios

data CorteSuprema = CorteSuprema {
    jueces :: [Juez],
    agenda :: [String]
}

corteSuprema :: CorteSuprema
corteSuprema = CorteSuprema juecesCorte (map tema conjuntoLeyes)

juecesCorte :: [Juez]
juecesCorte = [juezOpinionPublica, juezAntiSectorFinanciero, juezRata, juezRata2, juezProSectorConservador]

juezOpinionPublica :: Juez
juezOpinionPublica ley = tema ley `elem` agenda corteSuprema

juezAntiSectorFinanciero :: Juez
juezAntiSectorFinanciero ley = "Sector Financiero" `notElem` apoyo ley

juezRata :: Juez
juezRata ley = presupuesto ley < 10

juezRata2 :: Juez
juezRata2 ley = presupuesto ley < 20

juezProSectorConservador :: Juez
juezProSectorConservador ley = "Partido Conservador" `elem` apoyo ley

-- Hacer que una Corte Suprema determine si se considera constitucional o no una ley.

esConstitucional :: [t1 -> Bool] -> t1 -> Bool
esConstitucional jueces ley = fromIntegral (length (filter ($ ley) jueces)) - fromIntegral (length jueces) / 2 > 0 -- Uso fromIntegral para más precision

{-
Agregar nuevos jueces que puedan integrar la corte suprema:
Uno que siempre vote afirmativamente
Un juez inventado, con lógica totalmente diferente (no trivial).
Otro juez que también tenga preocupación presupuestaria pero con otro importe.
-}

juezSiSeñor :: Juez
juezSiSeñor _ = True

juezInventado :: Juez
juezInventado ley = length (vocales ley) > 2

vocales :: Ley -> [Char]
vocales ley = filter (\c -> c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u' ||
                            c == 'A' || c == 'E' || c == 'I' || c == 'O' || c == 'U') (tema ley)

juezDespilfarrador :: Juez
juezDespilfarrador ley = presupuesto ley > 100

{-
Hacer una función que dada una serie de leyes, averigue cuáles que no serían consideradas constitucionales 
con la actual conformación de la corte sí lo serían en caso de agregarle un conjunto de nuevos integrantes. 
-}

nuevosJuecesCorte :: [Juez]
nuevosJuecesCorte = [juezOpinionPublica, juezAntiSectorFinanciero, juezRata, juezRata2, juezProSectorConservador, juezInventado, juezSiSeñor, juezDespilfarrador]

ahoraSi :: [a] -> [a -> Bool] -> [a -> Bool] -> [a]
ahoraSi leyes viejosJueces nuevosJueces = filter (esConstitucional nuevosJueces) (filter (not.esConstitucional viejosJueces) leyes)

leyAhoraSi :: Ley
leyAhoraSi = Ley "Ahora siii" 120 ["Sector Progresista"]

-- PARTE 3
{-
Cuestión de principios 
A veces pasa que a los jueces les pasan cosas, 
se sospecha de su independencia o de pronto cambian el sentido de su voto. 
-}

{-
Hacer la función borocotizar, que dada una conformación de la Corte Suprema pasen a votar de forma 
contraria a lo que votaban antes.
-}

borocotizar :: [a -> Bool] -> [a -> Bool]
borocotizar = map (not .)

{-
Determinar si un juez curiosamente coincide en su posición con un sector social, que se da cuando de 
un conjunto dado de leyes actualmente en tratamiento, sólo vota las que son apoyadas por dicho sector.
-}

intereses :: (Ley -> Bool) -> [Ley] -> Bool
intereses juez leyes = (and.map ((head.concatMap apoyo) (filter juez leyes) ==)) (concatMap apoyo (filter juez leyes))
-- Lo que hace esta funcion es filtrar las leyes que el juez aprobaria (que al aplicarlas al juez devuelven true) 
-- y liego concatena la lista con listas de los sectores que apoyan cada ley aprobada. Luego se repite lo mismo
-- pero se toma la cabeza de esta lista con los sectores que apoyan la ley. Luego si esta cabeza es igual
-- a todos los elementos de la lista que contiene a todos los sectores que apoyan a sus respectivas leyes,
-- quiere decir que solo hay un sector, por lo tanto hay interes.

-- Para entender un poco más sobre filter: 

-- map ($ 3) (filter ($ 2) [(<5),(2<)]) = [True] -> osea que filter me esta devolviendo las funciones que aplicadas
-- al argumento que le pasa ($ 2) son verdaderas