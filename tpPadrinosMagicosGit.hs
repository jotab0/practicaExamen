
--1

data Chico = Chico {
    nombre :: String,
    edad :: Int,
    habilidades :: [String],
    deseos :: [Chico -> Chico]
}
