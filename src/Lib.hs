module Lib where
import Text.Show.Functions

type Lugar = String
type CantidadPublico = Int
type EstadoAnimo = String
type Descripcion = String
type Decibeles = Int


data Festival= Festival {
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
modificarCantidadPublico unaFuncion unFestival = unFestival {
    cantidadPublico = unaFuncion.cantidadPublico $unFestival
}

nuevoEstadoDeAnimo :: (EstadoAnimo->EstadoAnimo)->Festival->Festival
nuevoEstadoDeAnimo unaFuncion unFestival = unFestival {
    estadoAnimo = unaFuncion.estadoAnimo $unFestival
}
efectoMetal :: EstadoAnimo->Festival->Festival
efectoMetal unEstadoAnimo =aumentarPublicoMetal. nuevoEstadoDeAnimo (\animo-> animo ++  unEstadoAnimo)

cumpleEstadoAnimo :: EstadoAnimo->Festival->Bool
cumpleEstadoAnimo unEstadoAnimo = (==unEstadoAnimo).estadoAnimo

aumentarPublicoMetal :: Festival->Festival
aumentarPublicoMetal = modificarCantidadPublico (\cantidad->cantidad+(1*cantidad `div` 100))

--Generos
rockNacional :: Genero
rockNacional = modificarCantidadPublico(+100)

pop :: Genero
pop unFestival
 |cumpleEstadoAnimo "indiferente" unFestival= modificarCantidadPublico (*2).nuevoEstadoDeAnimo (\estado->"euforico") $unFestival
 | otherwise = unFestival


type Metal = Genero
heavyMetal :: Metal
heavyMetal = efectoMetal " pesado" 

trashMetal :: Metal
trashMetal = efectoMetal " basura"

subgeneroMetal :: EstadoAnimo->Metal
subgeneroMetal unEstadoAnimo = aumentarPublicoMetal.nuevoEstadoDeAnimo(\animo-> animo ++ " " ++ unEstadoAnimo)


--Modelado
hullabalooza = Festival "Springfield" 20000 "indiferente" [losRedondos,metallica,soda]
losRedondos = Banda ["legendaria","pogosa"] 45 rockNacional
soda = Banda ["irrepetible"] 40 rockNacional
miranda = Banda ["insipida", "incolora", "inodora"] 60 pop
metallica = Banda ["legendaria", "vendida"] 60 heavyMetal

tocar :: Banda->Festival->Festival
tocar unaBanda unFestival = (genero unaBanda) unFestival


--2)

bandaLoca = Banda ["original"] 100 trashMetal

--3)

generoTheStokes :: Genero
generoTheStokes = pop.heavyMetal

theStokes = Banda ["suicidio asistido","emocional","linda"] 45 generoTheStokes

--4)

suceder :: Festival->Festival
suceder unFestival = foldr ($) unFestival (map (genero) (bandas unFestival))

--5)
type Criterio = Banda->Bool

tieneMasDeTresDescripciones :: Banda->Bool
tieneMasDeTresDescripciones  = (>3).length.descripcion

tieneComoDescripcion :: Descripcion->Banda->Bool
tieneComoDescripcion unaDescripcion = (elem unaDescripcion).descripcion

tocaTantosDecibeles :: Decibeles->Banda->Bool
tocaTantosDecibeles unosDecibeles = (>unosDecibeles).decibeles

criterioVendida :: Criterio
criterioVendida unaBanda = tieneComoDescripcion "vendida" unaBanda || tieneMasDeTresDescripciones unaBanda

criterioAcustica :: Criterio
criterioAcustica = tocaTantosDecibeles 55 

criterioLegendaria :: Criterio 
criterioLegendaria unaBanda = tieneComoDescripcion "legendaria" unaBanda && tocaTantosDecibeles 40 unaBanda

--5)
