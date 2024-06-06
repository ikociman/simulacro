-- Escribí tu código acá
data Auto = Auto {
    marca         :: String, 
    modelo        :: String,
    desgaste      :: (Float,Float),
    velMax        :: Float, 
    tiempoCarrera :: Float
} deriving(Show, Eq)

ferrari :: Auto
ferrari = Auto {
    marca = "Ferrari",
    modelo = "F50",
    desgaste = (0, 0),
    velMax = 73,
    tiempoCarrera = 0
}

lamborguini :: Auto
lamborguini = Auto {
    marca = "Lamborguini",
    modelo = "Diablo",
    desgaste = (4, 7),
    velMax = 73,
    tiempoCarrera = 0
}

fiat :: Auto
fiat = Auto {
    marca = "Fiat",
    modelo = "600",
    desgaste = (27, 33),
    velMax = 44,
    tiempoCarrera = 0
}

type Tramo = Auto -> Auto


-- Punto 2
enBuenEstado :: Auto -> Bool
enBuenEstado (Auto _ _ desgaste _ _ ) = 
    fst desgaste < 60 && snd desgaste < 40


noDaMas :: Auto -> Bool
noDaMas (Auto _ _ desgaste _ _ ) = 
    fst desgaste > 60 || snd desgaste > 40



-- Punto 3
desgasteRuedas :: Auto -> Float
desgasteRuedas auto = 
    fst (desgaste auto)


desgasteChasis :: Auto -> Float
desgasteChasis auto = 
    snd (desgaste auto)


cambiarDesgasteChasis :: (Float -> Float) -> Auto -> Auto
cambiarDesgasteChasis modificador auto = 
    auto {
        desgaste = (desgasteRuedas auto, (modificador)(desgasteChasis auto))
    }


cambiarDesgasteRuedas :: (Float -> Float) -> Auto -> Auto
cambiarDesgasteRuedas modificador auto = 
    auto {
        desgaste = ((modificador) (desgasteRuedas auto), desgasteChasis auto)
    }


reparacion :: Auto -> Auto
reparacion = cambiarDesgasteChasis (*0.15) . cambiarDesgasteRuedas (*0)

-- Punto 4
incrementarTiempo :: Float -> Auto -> Auto
incrementarTiempo incremento auto = 
    auto {
        tiempoCarrera = tiempoCarrera auto + incremento 
    }


curva :: Float -> Float -> Auto -> Auto
curva angulo long auto =
    incrementarTiempo (long / (velMax auto / 2)) . cambiarDesgasteRuedas ((-)(3 * long / angulo)) $ auto


curvaPeligrosa :: Auto -> Auto
curvaPeligrosa  = 
    curva 60 300  


curvaTranca :: Auto -> Auto
curvaTranca  =
    curva 110 550  


recto :: Float -> Auto -> Auto
recto long auto =
    incrementarTiempo (long / velMax auto) . cambiarDesgasteChasis ((-)(long / 100)) $ auto


tramoRectoClassic :: Auto -> Auto
tramoRectoClassic  = 
    recto 750 


tramito :: Auto -> Auto
tramito =
    recto 280  


boxes :: Auto -> Auto
boxes auto 
    | enBuenEstado auto = auto
    | otherwise = (penalizacion 10 . reparacion) $ auto 


penalizacion :: Float -> Auto -> Auto
penalizacion tiempo auto = 
    auto {
        tiempoCarrera = tiempoCarrera auto + tiempo
    }


hayLimpiezaOlluvia :: Tramo -> Auto -> Auto
hayLimpiezaOlluvia tramo auto = 
    auto {
        tiempoCarrera = tiempoCarrera auto + (diferenciaTiempos auto (tramo auto) / 2)
    }
 
diferenciaTiempos :: Auto -> Auto -> Float
diferenciaTiempos autoInicial autoFinal = 
    tiempoCarrera autoFinal - tiempoCarrera autoInicial

hayRipio :: Tramo -> Auto -> Auto
hayRipio tramo auto = 
    auto {
        desgaste = desgastePorRipio tramo auto,
        tiempoCarrera = tiempoCarrera auto + (diferenciaTiempos auto (tramo auto) * 2)
    }

diferenciaDesgaste :: Auto -> Auto -> Auto
diferenciaDesgaste autoInicial autoFinal = 
    autoFinal {
        desgaste = (desgasteRuedas autoFinal - desgasteRuedas autoInicial, desgasteChasis autoFinal - desgasteChasis autoInicial)
    }

desgastePorRipio :: Tramo -> Auto -> (Float, Float)
desgastePorRipio tramo auto  = 
    (desgasteRuedas auto - desgasteRuedas (diferenciaDesgaste auto (tramo auto)) * 2, desgasteChasis auto - desgasteChasis (diferenciaDesgaste auto (tramo auto)) * 2)

hayObstruccion :: Tramo -> Float -> Auto -> Auto
hayObstruccion tramo espOcupado auto =
    desgastePorObstruccion tramo espOcupado auto
    
desgastePorObstruccion :: Tramo -> Float -> Auto -> Auto
desgastePorObstruccion tramo espOcupado auto  = 
    auto {
        desgaste = (desgasteRuedas auto - desgasteObstruccionTramo tramo espOcupado auto, desgasteChasis auto)
    }

desgasteObstruccionTramo :: Tramo -> Float -> Auto -> Float
desgasteObstruccionTramo tramo espOcupado auto = 
    desgasteRuedas auto - (2 * espOcupado + (fst.desgaste.tramo)auto)







    

