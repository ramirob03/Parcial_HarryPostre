module Library where
import PdePreludat

--Postres 
--Punto A

type Sabor = String
data Postre = UnPostre {
    nombrePostre :: String,
    sabores :: [Sabor],
    peso :: Number,
    temperatura :: Number
} deriving (Show, Eq)

bizcocho :: Postre
bizcocho = UnPostre "Bizcocho" ["borracho", "fruta", "crema"] 100 25

--Punto B

type Hechizo = Postre -> Postre

incendio :: Hechizo
incendio = perderPeso 5 . calentarPostre 1

immobulus :: Hechizo
immobulus = congelarPostre

wingardiumLeviosa :: Hechizo
wingardiumLeviosa = perderPeso 10 . agregarSabor "concentrado"

diffindo :: Number -> Hechizo
diffindo = perderPeso

riddikulus :: Sabor -> Hechizo
riddikulus sabor = agregarSabor (reverse sabor)

avadaKedravra :: Hechizo
avadaKedravra = eliminarSabores . congelarPostre

--Delegacion de funciones

calentarPostre :: Number -> Postre -> Postre
calentarPostre temp postre = postre {temperatura = temperatura postre + temp}

perderPeso :: Number -> Postre -> Postre
perderPeso porcentajePeso postre = postre {peso = peso postre - peso postre * porcentajePeso/100}

congelarPostre :: Postre -> Postre
congelarPostre postre = postre {temperatura = 0}

agregarSabor :: Sabor -> Postre -> Postre
agregarSabor sabor postre = postre {sabores = sabor : sabores postre}

eliminarSabores :: Postre -> Postre
eliminarSabores postre = postre {sabores = []}

--Punto C

estanListos :: Hechizo -> [Postre] -> Bool
estanListos hechizo = all (estaListo . hechizo)

estaListo :: Postre -> Bool
estaListo postre = not (null (sabores postre)) && peso postre > 0 && temperatura postre > 0

--Punto D

promedio :: [Number] -> Number
promedio listaPesos = sum listaPesos / length listaPesos 

pesoPromedio :: [Postre] -> Number
pesoPromedio = promedio . map peso . filter estaListo

--Magos
--Punto A

data Mago = UnMago {
    nombreMago :: String,
    hechizos :: [Hechizo],
    horrorcruxes :: Number
} deriving Show

--Hipotesis: no se puede aprender el avada kedavra en la clase de defensa contra las cocinas oscuras
claseDefCocinasOscuras :: Postre -> Hechizo -> Mago -> Mago
claseDefCocinasOscuras postre hechizo  = sumarHorrorcruxSi (hechizo postre) . aprenderHechizo hechizo 

aprenderHechizo :: Hechizo -> Mago -> Mago
aprenderHechizo hechizo mago = mago {hechizos = hechizo : hechizos mago}

sumarHorrorcruxSi :: Postre -> Mago -> Mago
sumarHorrorcruxSi postre mago 
    | esEquivalenteAAvadaKedavra postre = mago {hechizos = avadaKedravra : hechizos mago}
    | otherwise = mago

esEquivalenteAAvadaKedavra :: Postre -> Bool
esEquivalenteAAvadaKedavra postre = postre == avadaKedravra postre

--Punto B

elMejorHechizo :: Postre -> Mago -> Hechizo
elMejorHechizo postre = elMejor postre . hechizos

elMejor :: Postre -> [Hechizo] -> Hechizo
elMejor _ [hechizo] = hechizo
elMejor postre (primero:segundo:restoHechizos)
    | tieneMasSabores postre primero segundo = elMejor postre (primero:restoHechizos)
    | otherwise = elMejor postre (segundo:restoHechizos)

tieneMasSabores :: Postre -> Hechizo -> Hechizo -> Bool
tieneMasSabores postre hechizo1 hechizo2 = (length . sabores . hechizo1) postre > (length . sabores . hechizo2) postre

--Infinita Magia
--Punto A

mesaInfinita :: [Postre]
mesaInfinita = repeat bizcocho

magoExtraPoderoso :: Mago
magoExtraPoderoso = UnMago {nombreMago = "Ramiro", hechizos = repeat incendio, horrorcruxes = 0}

--Punto B
{-Puedo realizar una funcion que verifique que cada uno de los postres este listo, en caso de estar todos listos va a romper ya que nunca terminaria
de verificar que todos los postres esten listo. Pero si alguno de los postres no esta listo, me va a devolver False ya que encontro uno que no esta
listo, por lo tanto la funcion deja de operar al llegar a ese postre. Esto es gracias a un concepto que utiliza Haskell llamado lazy evaluation-}

--Punto C
{-No es posible encontrar el mejor hechizo porque siempre va a tener que comparar con un siguiente, mas alla de que conceptualmente hablando
ya lo hallamos encontrado, la maquina no lo sabe por lo tanto seguiria buscando hasta que rompa el programa-}