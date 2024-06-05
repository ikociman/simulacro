-- Escribí tu código acá
-- Los autos se componen de marca, modelo, desgaste (ruedas y chasis, son dos números), velocidad máxima (m/s), y el tiempo de carrera, 
-- que lo vamos a considerar inicialmente 0 y tendremos en cuenta luego el uso.

import Text.Show.Functions

-------------
-- Punto 1 --
-------------

data Auto = Auto {
    marca :: String,
    modelo :: String,
    desgaste :: (Float,Float), -- (rueda, chasis)
    velocidadMaxima :: Float,
    tiempoDeCarrera :: Float
} deriving (Show, Eq)

ferrari :: Auto
ferrari = Auto{
    marca="Ferrari",
    modelo="F50",
    desgaste=(0,0), 
    velocidadMaxima=65,
    tiempoDeCarrera=0
}

lamborghini :: Auto
lamborghini = Auto{
    marca="Lamborghini",
    modelo="Diablo",
    desgaste=(7,4),
    velocidadMaxima=73,
    tiempoDeCarrera=0
}

fiat :: Auto
fiat = Auto{
    marca="Fiat",
    modelo="600",
    desgaste=(33,27),
    velocidadMaxima=44,
    tiempoDeCarrera=0
}

-------------
-- Punto 2 --
-------------

desgasteRuedas :: Auto -> Float
desgasteRuedas = fst.desgaste

desgasteChasis :: Auto -> Float
desgasteChasis = snd.desgaste

enBuenEstado :: Auto -> Bool
enBuenEstado unAuto = desgasteChasis unAuto < 40 && desgasteRuedas unAuto < 60

noDaMas :: Auto -> Bool
noDaMas unAuto = desgasteChasis unAuto > 80 || desgasteRuedas unAuto > 80

-------------
-- Punto 3 --
-------------

cambiarDesgasteChasis :: (Float -> Float) -> Auto -> Auto
cambiarDesgasteChasis modificador unAuto = unAuto {desgaste = (desgasteRuedas unAuto, (modificador)(desgasteChasis unAuto))}

cambiarDesgasteRuedas :: (Float -> Float) -> Auto -> Auto
cambiarDesgasteRuedas modificador unAuto = unAuto {desgaste = ((modificador)(desgasteRuedas unAuto), desgasteChasis unAuto)}

reparar :: Auto -> Auto
reparar = cambiarDesgasteChasis (*0.15) . cambiarDesgasteRuedas (*0)


-------------
-- Punto 4 --
-------------

--Una pista está compuesta de distintas partes (curvas, rectas, boxes), donde cada tramo termina realizando una transformación sobre 
--el auto que la atraviesa.

modificarTiempo :: (Float -> Float) -> Auto -> Auto
modificarTiempo modificador unAuto = unAuto {tiempoDeCarrera = (modificador)(tiempoDeCarrera unAuto)}

danioRuedaCurva :: Float -> Float -> Float
danioRuedaCurva longitud angulo = (3 *) $ (/) longitud angulo

atravesarCurva :: Float -> Float -> Auto -> Auto
atravesarCurva angulo longitud unAuto = cambiarDesgasteRuedas ((-) (danioRuedaCurva longitud angulo)) . modificarTiempo (+ (angulo/(velocidadMaxima unAuto)/2)) $ unAuto

curvaPeligrosa :: Auto -> Auto
curvaPeligrosa = atravesarCurva 60 300

curvaTranca :: Auto -> Auto
curvaTranca = atravesarCurva 110 550

atravesarRecta :: Float -> Auto -> Auto
atravesarRecta longitud unAuto = cambiarDesgasteChasis (* 0.99) . modificarTiempo (+ (longitud / (velocidadMaxima unAuto))) $ unAuto

tramoRectoClassic :: Auto -> Auto
tramoRectoClassic = atravesarRecta 750

tramito  :: Auto -> Auto
tramito = atravesarRecta 280

boxes :: Bool -> Auto -> Auto
boxes False unAuto = unAuto
boxes True unAuto = modificarTiempo (+ 10) . reparar $ unAuto 

limpiezaLLuvia :: (Auto -> Auto) -> Auto -> Auto
limpiezaLLuvia tramo unAuto =  modificarTiempo (+ ((tiempoDeCarrera (tramo unAuto)) - (tiempoDeCarrera unAuto))/2) unAuto