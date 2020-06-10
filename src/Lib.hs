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


rockNacional :: Genero
rockNacional = modificarCantidadPublico(+100)

pop :: Genero
pop unFestival
 |cumpleEstadoAnimo "indiferente" unFestival= modificarCantidadPublico (*2).nuevoEstadoDeAnimo (\estado->"euforico") $unFestival
 | otherwise = unFestival

cumpleEstadoAnimo :: EstadoAnimo->Festival->Bool
cumpleEstadoAnimo unEstadoAnimo = (==unEstadoAnimo).estadoAnimo

type Metal = Genero
heavyMetal :: Metal
heavyMetal = aumentarPublicoMetal. nuevoEstadoDeAnimo (\[animo]-> [animo] ++ "pesado") 

trashMetal :: Metal
trashMetal = aumentarPublicoMetal.nuevoEstadoDeAnimo (\[animo]-> [animo] ++ "basura")

subgeneroMetal :: EstadoAnimo->Genero
subgeneroMetal unEstadoAnimo = aumentarPublicoMetal.nuevoEstadoDeAnimo(\[animo]-> [animo] ++ unEstadoAnimo)

aumentarPublicoMetal :: Festival->Festival
aumentarPublicoMetal = modificarCantidadPublico (\cantidad->cantidad+(1*cantidad `div` 100))


hullabalooza = Festival "Springfield" 20000 "indiferente" [losRedondos,metallica,soda]
losRedondos = Banda ["legendaria","pogosa"] 45 rockNacional
soda = Banda ["irrepetible"] 40 rockNacional
miranda = Banda ["insipida", "incolora", "inodora"] 60 pop
metallica = Banda ["legendaria", "vendida"] 60 (heavyMetal)