module Lib where
import Text.Show.Functions

type Lugar = String
type CantidadPublico = Int
type EstadoAnimo = String
type Descripcion = String
type Decibeles = Int


data Festival= Festival{
    lugar :: Lugar,
    cantidadPublico :: CantidadPublico,
    estadoAnimo :: EstadoAnimo,
    bandas :: [Banda] 
} deriving (Show)

data Banda= Banda{
    descripcion :: [Descripcion],
    decibeles :: Decibeles,
    genero :: Genero
} deriving (Show)

type Genero = Festival->Festival

modificarCantidadPublico :: (CantidadPublico->CantidadPublico)->Festival->Festival
modificarCantidadPublico unaFuncion unFestival = Festival {
    cantidadPublico = unaFuncion.cantidadPublico $unFestival
}

nuevoEstadoDeAnimo :: (EstadoAnimo->EstadoAnimo)->Festival->Festival
nuevoEstadoDeAnimo unaFuncion unFestival = Festival {
    estadoAnimo = unaFuncion.estadoAnimo $unFestival
}

cumpleEstadoAnimo :: EstadoAnimo->Festival->Bool
cumpleEstadoAnimo unEstadoAnimo = (==unEstadoAnimo).estadoAnimo

efectoMetal :: EstadoAnimo->Festival->Festival
efectoMetal unEstadoAnimo =aumentarPublicoMetal. nuevoEstadoDeAnimo (\[animo]-> [animo] ++  unEstadoAnimo)

aumentarPublicoMetal :: Festival->Festival
aumentarPublicoMetal = modificarCantidadPublico (\cantidad->cantidad+(1*cantidad `div` 100))

--Generos
rockNacional :: Genero
rockNacional = modificarCantidadPublico(+100)

pop :: Genero
pop unFestival
 |cumpleEstadoAnimo "indiferente" unFestival= modificarCantidadPublico (*2).nuevoEstadoDeAnimo (\estado->"euforico") $unFestival
 | otherwise = unFestival

metal :: String->EstadoAnimo->Genero
metal "heavy metal" "pesado" unFestival = efectoMetal "pesado" unFestival
metal "trash metal" "basura" unFestival = efectoMetal "basura" unFestival
metal _ unEstadoAnimo unFestival = efectoMetal unEstadoAnimo unFestival

--Modelado
hullabalooza = Festival "Springfield" 20000 "indiferente" [losRedondos,metallica,soda]
losRedondos = Banda ["legendaria","pogosa"] 45 rockNacional
soda = Banda ["irrepetible"] 40 rockNacional
miranda = Banda ["insipida", "incolora", "inodora"] 60 pop
metallica = Banda ["legendaria", "vendida"] 60 (metal "heavy metal" "pesado")