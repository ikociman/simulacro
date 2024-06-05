-- Escribí tu código acá
data Auto = Auto {
    marca         :: String, 
    modelo        :: String,
    desgaste      :: (Float,Float),
    velMax        :: Float, 
    tiempoCarrera :: Float
} deriving(Show, Eq);

ferrari :: Auto;
ferrari = Auto {
    marca = "Ferrari",
    modelo = "F50",
    desgaste = (0, 0),
    velMax = 73,
    tiempoCarrera = 0
}

lamborguini :: Auto;
lamborguini = Auto {
    marca = "Lamborguini",
    modelo = "Diablo",
    desgaste = (4, 7),
    velMax = 73,
    tiempoCarrera = 0
}

fiat :: Auto;
fiat = Auto {
    marca = "Fiat",
    modelo = "600",
    desgaste = (27, 33),
    velMax = 44,
    tiempoCarrera = 0
}

type Tramo = Auto -> Auto;


-- Punto 2
enBuenEstado :: Auto -> Bool;
enBuenEstado (Auto _ _ desgaste _ _ ) = 
    fst desgaste < 60 && snd desgaste < 40;
;

noDaMas :: Auto -> Bool;
noDaMas (Auto _ _ desgaste _ _ ) = 
    fst desgaste > 60 || snd desgaste > 40;
;


-- Punto 3
reparacionChasis :: Float -> Float;
reparacionChasis desgasteChasis = 
    desgasteChasis - desgasteChasis * 0.85;
;

reparacionRuedas :: (Float -> Float) -> Float -> Float;
reparacionRuedas desgasteRuedas long ang =
    desgasteRuedas - 3 * long / ang;
;

-- Punto 4
curva :: Float -> Float -> Auto -> Auto;
curva angulo long auto =
    auto {
        desgaste = (fst (desgaste auto) - 3 * long / angulo , snd (desgaste auto)),
        tiempoCarrera = tiempoCarrera auto +  long / (velMax auto / 2)
    };
;

curvaPeligrosa :: Auto -> Auto;
curvaPeligrosa auto = 
    curva 60 300 $ auto;
;

curvaTranca :: Auto -> Auto;
curvaTranca auto =
    curva 110 550 $ auto;
;

recto :: Float -> Auto -> Auto;
recto long auto =
    auto {
        desgaste = (fst (desgaste auto), snd (desgaste auto) - long / 100),
        tiempoCarrera = tiempoCarrera auto + long / velMax auto
    };
;

tramoRectoClassic :: Auto -> Auto;
tramoRectoClassic auto = 
    recto 750 $ auto;
;

tramito :: Auto -> Auto;
tramito auto =
    recto 280 $ auto
;

boxes :: Auto -> Auto;
boxes auto 
    | enBuenEstado auto = auto
    | otherwise = (penalizacion.reparacion) $ auto 
;

penalizacion :: Auto -> Auto;
penalizacion auto = 
    auto {
        tiempoCarrera = tiempoCarrera auto + 10
    };
;

limpiezaOlluvia :: Tramo -> Auto -> Auto;
limpiezaOlluvia tramo auto = 
    auto {
        tiempoCarrera = (tramo auto - tiempoCarrera auto) / 2
    };
; 


    

