{- A) Modelar los postres. Un mismo postre puede tener muchos sabores, tiene un peso y se sirve a
cierta temperatura.
Por ejemplo, un bizcocho borracho de fruta y crema de 100 gramos servido a 25°C.-}

data Postre = Postre {
    sabores :: [String],
    peso :: Float,
    temperatura :: Int
} deriving Show

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
