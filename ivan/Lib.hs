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
ferrari = Auto "Ferrari" "F50" (0,0) 65 0

lamborghini :: Auto
lamborghini = Auto "Lamborghini" "Diablo" (7,4) 73 0

fiat :: Auto
fiat = Auto "Fiat" "600" (33,27) 44 0

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

actualizarValorChasis :: Float -> Auto -> Auto
actualizarValorChasis desgasteNuevo unAuto = unAuto { desgaste = (fst (desgaste unAuto), desgasteNuevo) }

actualizarValorRuedas :: Float -> Auto -> Auto
actualizarValorRuedas desgasteNuevo unAuto = unAuto { desgaste = (desgasteNuevo, snd (desgaste unAuto)) }

modificarChasis :: (Float -> Float) -> Auto -> Auto
modificarChasis operacion unAuto = actualizarValorChasis (operacion (desgasteChasis unAuto)) unAuto

modificarRuedas :: (Float -> Float) -> Auto -> Auto
modificarRuedas operacion unAuto = actualizarValorRuedas (operacion (desgasteRuedas unAuto)) unAuto

reparar :: Auto -> Auto
reparar = modificarChasis (0.15 *) . actualizarValorRuedas 0


-------------
-- Punto 4 --
-------------

modificarTiempo :: (Float -> Float) -> Auto -> Auto
modificarTiempo modificador unAuto = unAuto {tiempoDeCarrera = modificador (tiempoDeCarrera unAuto)}

sumarSegundos :: Float -> Auto -> Auto
sumarSegundos segundos = modificarTiempo ((+) segundos)

atravesarCurva :: Float -> Float -> Auto -> Auto
atravesarCurva angulo longitud unAuto = modificarRuedas ((-) $ 3 * longitud / angulo) . sumarSegundos (2 * longitud / velocidadMaxima unAuto) $ unAuto

atravesarRecta :: Float -> Auto -> Auto
atravesarRecta longitud unAuto = modificarChasis ((-) $ longitud / 100) . sumarSegundos (longitud / velocidadMaxima unAuto) $ unAuto

curvaPeligrosa :: Auto -> Auto
curvaPeligrosa = atravesarCurva 60 300

curvaTranca :: Auto -> Auto
curvaTranca = atravesarCurva 110 550

tramoRectoClassic :: Auto -> Auto
tramoRectoClassic = atravesarRecta 750

tramito  :: Auto -> Auto
tramito = atravesarRecta 280

boxes :: Auto -> Auto
boxes unAuto
    | not.enBuenEstado $ unAuto = sumarSegundos 10 . reparar $ unAuto
    | otherwise = unAuto

valorAbsoluto :: Float -> Float
valorAbsoluto valor
    | valor < 0 = valor * (-1)
    | otherwise = valor

diferenciaDeTiempos :: Auto -> Auto -> Float
diferenciaDeTiempos unAuto otroAuto = valorAbsoluto $ tiempoDeCarrera unAuto - tiempoDeCarrera otroAuto

diferenciaChasis :: Auto -> Auto -> Float
diferenciaChasis unAuto otroAuto = valorAbsoluto $ desgasteChasis unAuto - desgasteChasis otroAuto

diferenciaRuedas :: Auto -> Auto -> Float
diferenciaRuedas unAuto otroAuto = valorAbsoluto $ desgasteRuedas unAuto - desgasteRuedas otroAuto

tramoMojado :: (Auto -> Auto) -> Auto -> Auto
tramoMojado tramo unAuto = sumarSegundos ((diferenciaDeTiempos unAuto $ tramo unAuto)/2) unAuto

--tieneRipo :: (Auto -> Auto) -> Auto -> Auto
--tieneRipo tramo unAuto =  modificarRuedas ((-) $ 1) unAuto

-- doble de tiempo
-- doble de efecto

-- Algunos tramos tienen ripio (sí... está un poco descuidada la pista) y produce el doble de efecto de un tramo normal equivalente, 
-- y se tarda el doble en atravesarlo también. Nos aclaran que, por suerte, nunca hay boxes con ripio.


{-
modificarTiempo :: (Float -> Float) -> Auto -> Auto
modificarTiempo modificador unAuto = unAuto {tiempoDeCarrera = modificador (tiempoDeCarrera unAuto)}

danioRuedaCurva :: Float -> Float -> Float
danioRuedaCurva longitud angulo = 3 * longitud / angulo

atravesarCurva :: Float -> Float -> Auto -> Auto
atravesarCurva angulo longitud unAuto = modificarRuedas (( - (3 * angulo))) . modificarTiempo (+ (angulo/(velocidadMaxima unAuto)/2)) $ unAuto

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
-}