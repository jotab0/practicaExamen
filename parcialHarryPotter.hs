{- A) Modelar los postres. Un mismo postre puede tener muchos sabores, tiene un peso y se sirve a
cierta temperatura.
Por ejemplo, un bizcocho borracho de fruta y crema de 100 gramos servido a 25°C.-}

import Text.Show.Functions ()

data Postre = Postre {
    sabores :: [String],
    peso :: Float,
    temperatura :: Int
} deriving (Show,Eq)

bizcochoBorracho :: Postre
bizcochoBorracho = Postre ["Fruta", "Crema"] 100 25

{- 
B) Modelar los hechizos, sabiendo que deberían poderse agregar más sin modificar el código
existente. Por ahora existen los siguientes:
○ Incendio: calienta el postre 1 grado y lo hace perder 5% de su peso.
○ Immobulus: congela el postre, llevando su temperatura a 0.
○ Wingardium Leviosa: levanta el postre en el aire y lo deja caer, lo que agrega a sus
sabores el sabor “concentrado”. Además, pierde 10% de su peso.
○ Diffindo: Corta el postre, disminuyendo su peso en el porcentaje indicado.
○ Riddikulus: Requiere como información adicional un sabor y lo agrega a los sabores que
tiene un postre, pero invertido.
○ Avada kedavra: Hace lo mismo que el immobulus pero además hace que el postre pierda
todos sus sabores. 
-}

-- Una forma de hacerlo

data Hechizos = Incendio | Immobulus | WingardiumLeviosa | Diffindo | Riddikulus String | AvadaKedavra

aplicarHechizoData :: Hechizos -> Postre -> Postre
aplicarHechizoData Incendio postre = postre { temperatura = temperatura postre + 1, peso = peso postre - peso postre * 0.05 }

-- Como a mi me parece:

-- HECHIZOS: 
type Hechizo = Postre -> Postre

incendio :: Hechizo
incendio postre = postre { temperatura = temperatura postre + 1, peso = peso postre - peso postre * 0.05 }

immobulus :: Hechizo
immobulus postre = postre { temperatura = 0}

wingardiumLeviosa :: Hechizo
wingardiumLeviosa postre = postre { sabores = "concentrado" : sabores postre, peso = peso postre * 0.90}

diffindo :: Float -> Hechizo
diffindo porcentaje postre = postre { peso = peso postre - peso postre * porcentaje/100 }

riddikulus :: [Char] -> Hechizo
riddikulus nuevoSabor postre = postre { sabores = reverse nuevoSabor : sabores postre}

avadaKedavra :: Hechizo
avadaKedavra postre = postre { temperatura = 0, sabores = []}

{- 
Dado un conjunto de postres en la mesa, saber si hacerles un determinado hechizo los dejará
listos (un postre está listo cuando pesa algo más que cero, tiene algún sabor y además no está
congelado).
Por ejemplo, si en la mesa está el bizcocho mencionado anteriormente y una tarta de melaza de
0 grados y 50 gramos, y les hago el hechizo incendio, quedan listos, pero si les hago el hechizo
riddikulus con el sabor “nomil” no, porque la tarta sigue congelada.
-}

mesa :: [Postre]
mesa = [bizcochoBorracho, tortaDdl, Postre ["Merengue"] 60 0]
tortaDdl :: Postre
tortaDdl = Postre ["Dulce de leche", "Chocolate", "Mani"] 60 46

estaListo :: [Postre] -> Hechizo -> [Bool]
estaListo postres hechizo = map (postreEstaListo . hechizo) postres

postreEstaListo :: Postre -> Bool
postreEstaListo postre = peso postre > 0 && (not.null) (sabores postre) && temperatura postre > 0

-- Dado un conjunto de postres en la mesa, conocer el peso promedio de los postres listos

pesoPromedioPostresListos :: [Postre] -> Float
pesoPromedioPostresListos postre = (sum.map peso) (filter postreEstaListo postre) / (fromIntegral.length.map peso) (filter postreEstaListo postre)

{-
2. Magos
De un mago se conocen sus hechizos aprendidos y la cantidad de horrorcruxes que tiene.
-}

data Mago = Mago {
    hechizos :: [Hechizo],
    horrorcruxes :: Int
} deriving Show

{-
A) Hacer que un mago asista a la clase de defensa contra las cocinas oscuras y practique con un
hechizo sobre un postre (se espera obtener el mago). Cuando un mago practica con un hechizo,
lo agrega a sus hechizos aprendidos.
Además si el resultado de usar el hechizo en el postre es el mismo que aplicarle “avada
kedavra” al postre, entonces suma un horrorcrux.
-}

harryPopoter :: Mago
harryPopoter = Mago {
    hechizos = [diffindo 3, immobulus, incendio, avadaKedavra,wingardiumLeviosa,riddikulus "pistacho"],
    horrorcruxes = 0
}

claseDefensa :: Mago -> Hechizo -> Postre -> Mago
claseDefensa mago hechizoLanzado postre
    | hechizoLanzado postre == avadaKedavra postre = mago { hechizos = hechizoLanzado  : hechizos mago, horrorcruxes = horrorcruxes mago + 1}
    | otherwise = mago { hechizos = hechizoLanzado  : hechizos mago}

claseDefensa1 :: Mago -> Hechizo -> Postre -> Mago
claseDefensa1 mago hechizoLanzado postre
    | hechizoLanzado postre == Postre [] (peso postre) 0 = mago { hechizos = hechizoLanzado  : hechizos mago, horrorcruxes = horrorcruxes mago + 1}
    | otherwise = mago { hechizos = hechizoLanzado  : hechizos mago}

{-
B) Dado un postre y un mago obtener su mejor hechizo, que es aquel de sus hechizos que deja al
postre con más cantidad de sabores luego de usarlo.
-}

mejorHechizo1 :: Mago -> Postre -> [Hechizo]
mejorHechizo1 mago postre = filter (masCantSabores postre (mayorLength (map sabores (map (aplicInv postre) (hechizos mago))))) (hechizos mago)

mejorHechizo :: Mago -> Postre -> [Hechizo]
mejorHechizo mago postre = filter (masCantSabores postre (masSabores postre mago)) (hechizos mago)

masCantSabores :: t -> Int -> (t -> Postre) -> Bool
masCantSabores postre n hechizo = (length.sabores) (hechizo postre) >= n

mayorLength :: Foldable t => [t a] -> Int
mayorLength listaDeListas = maximum (map length listaDeListas)

masSabores :: Postre -> Mago -> Int
masSabores postre mago = mayorLength (map (sabores . aplicInv postre) (hechizos mago))

aplicInv :: t1 -> (t1 -> t2) -> t2
aplicInv x f = f x

{-
3. Infinita Magia
A) Construir una lista infinita de postres, y construir un mago con infinitos hechizos.
-}

listaInfPostres :: Int -> [Postre]
listaInfPostres  n
    | n < 10 = Postre ["mango","camie","camarin"] 10 n : listaInfPostres (n + 1)
    | otherwise = []

mesaInfinita :: [Postre]
mesaInfinita = Postre ["mango","camie","camarin"] 10 60 : listaInfPostres 1

voldemort :: Mago
voldemort = Mago (listaInfinita wingardiumLeviosa) 10
listaInfinita :: t -> [t]
listaInfinita n = n : listaInfinita n

