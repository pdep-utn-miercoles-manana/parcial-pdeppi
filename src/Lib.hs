-- PARCIAL PDEPPI

-- PARTE A

-- PUNTO 1
import Text.Show.Functions

data Persona = Persona {
    nombrePersona :: String,
    direccion :: String,
    dineroDisponible :: Int,
    comidaFavorita :: Comida,
    cupones :: [Cupon]
} deriving (Show)

data Comida = Comida {
    nombreComida :: String,
    costo :: Int,
    ingredientes :: [Ingrediente]
} deriving (Show)

type Ingrediente = String

type Cupon = Comida -> Comida

paula :: Persona
paula = Persona {
    nombrePersona = "Paula",
    direccion = "Thames 1585",
    comidaFavorita = hamburguesaDeluxe,
    dineroDisponible = 3600,
    cupones = []
} 

hamburguesaDeluxe :: Comida
hamburguesaDeluxe = Comida {
    nombreComida = "hamburguesa deluxe",
    costo = 350,
    ingredientes = ["pan","carne","lechuga","tomate","panceta","queso","huevo frito"]
}

daniel :: Persona
daniel = Persona {
    nombrePersona = "Daniel",
    direccion = "Baker Street 221A",
    comidaFavorita = fideosConTuco,
    dineroDisponible = 0,
    cupones = []
}

fideosConTuco :: Comida
fideosConTuco = Comida {
    nombreComida = "fideos con tuco",
    costo = 100,
    ingredientes = ["fideos","tomate","ajo","zanahoria","amor"]
}

-- PARTE B

comprar :: Persona -> Comida -> Persona
comprar persona comida 
    | puedeComprar comida persona && valeMenosDe 200 comida = nuevaComidaFavorita comida.cambiarCantidadDeDineroDisponible (costo comida) $ persona
    | puedeComprar comida persona = cambiarCantidadDeDineroDisponible (costo comida) $ persona
    | otherwise = persona

puedeComprar :: Comida -> Persona -> Bool
puedeComprar comida persona = ((>).dineroDisponible $ persona ).costo $ comida

valeMenosDe :: Int -> Comida -> Bool
valeMenosDe cantidad = (<cantidad).costo

nuevaComidaFavorita :: Comida -> Persona -> Persona
nuevaComidaFavorita comida persona = persona {comidaFavorita = comida}

cambiarCantidadDeDineroDisponible :: Int -> Persona -> Persona
cambiarCantidadDeDineroDisponible cantidadADescontar persona = persona {dineroDisponible = (dineroDisponible persona) - cantidadADescontar}

carritoDeCompras :: [Comida] -> Persona -> Persona
carritoDeCompras comidas persona 
    | puedeComprarTodo comidas persona = cambiarCantidadDeDineroDisponible 100.foldl (comprar) persona.reverse $ comidas
    | otherwise = persona

puedeComprarTodo :: [Comida] -> Persona -> Bool
puedeComprarTodo comidas persona = ((dineroDisponible persona) >).(subtract 100).costoTotalDeComidas $ comidas

costoTotalDeComidas :: [Comida] -> Int
costoTotalDeComidas comidas = sum $ map (costo) comidas

-- CUPONES

semanaVegana :: Cupon
semanaVegana comida
    | esVegana comida = aplicarDescuento (div (costo comida) 2) comida
    | otherwise = comida

esVegana :: Comida -> Bool
esVegana comida = not (tieneAlgunaCarne comida || tieneAlgunTipoDeHuevo comida || tieneAlgunTipoDeQueso comida)

tieneAlgunaCarne :: Comida -> Bool
tieneAlgunaCarne = alguno esCarne

alguno :: (Ingrediente -> Bool) -> Comida -> Bool
alguno tipoDeIngrediente = any tipoDeIngrediente.ingredientes

esCarne :: Ingrediente -> Bool
esCarne  = empiezaCon "carne"

empiezaCon :: String -> String -> Bool
empiezaCon subcadena = (==subcadena).take (length subcadena)

tieneAlgunTipoDeHuevo :: Comida -> Bool
tieneAlgunTipoDeHuevo = alguno esHuevo

esHuevo :: Ingrediente -> Bool
esHuevo = empiezaCon "huevo"

tieneAlgunTipoDeQueso :: Comida -> Bool
tieneAlgunTipoDeQueso = alguno esQueso

esQueso :: Ingrediente -> Bool
esQueso = empiezaCon "queso"

aplicarDescuento :: Int -> Comida -> Comida
aplicarDescuento descuento comida = comida {costo = (costo comida) - descuento}

esoNoEsCocaPapi :: String -> Cupon
esoNoEsCocaPapi bebida = agregarIngrediente bebida.agregarAlFinalDelNombre "Party"

agregarIngrediente :: Ingrediente -> Comida -> Comida
agregarIngrediente ingrediente comida = comida {ingredientes = (ingrediente:).ingredientes $ comida}

agregarAlFinalDelNombre :: String -> Comida -> Comida
agregarAlFinalDelNombre cadena comida = comida{nombreComida = (nombreComida comida)++" "++cadena}

sinTACCis :: Cupon
sinTACCis = agregarALosIngredientes "libre de gluten" 

agregarALosIngredientes :: String -> Comida -> Comida
agregarALosIngredientes cadena comida = comida{ingredientes = map (++" "++cadena).ingredientes $ comida}

findeVegetariano :: Cupon
findeVegetariano comida
    | not.tieneAlgunaCarne $ comida = aplicarDescuento (div ((*3).costo $ comida) 100) comida
    | otherwise = comida

largaDistancia :: Cupon
largaDistancia = cambiarCostoDeComida 50.quitarIngredientesConMasDeXLetras 10

cambiarCostoDeComida :: Int -> Comida -> Comida
cambiarCostoDeComida cantidad = aplicarDescuento (-cantidad)

quitarIngredientesConMasDeXLetras :: Int -> Comida -> Comida
quitarIngredientesConMasDeXLetras cantidadDeLetrasMaximas comida = comida {ingredientes = filter (tieneMasDe cantidadDeLetrasMaximas).ingredientes $ comida}

tieneMasDe :: Int -> Ingrediente -> Bool
tieneMasDe cantidadMaxima = (>cantidadMaxima).length 

-- PARTE C

comprarConCupones :: Persona -> Persona
comprarConCupones persona = comprar persona.usarCuponesEnComida (comidaFavorita persona).cupones $ persona

usarCuponesEnComida :: Comida -> [Cupon] -> Comida
usarCuponesEnComida comida listaDeCupones = foldr ($) comida listaDeCupones

superComida :: [Comida] -> Comida
superComida comidas = Comida {
    nombreComida = sacarVocales.unirNombres $ comidas,
    costo = costoTotalDeComidas comidas,
    ingredientes = quitarRepetidos.unirTodosLosIngredientes $ comidas
}

sacarVocales :: String -> String
sacarVocales = filter (not.esVocal) 

esVocal :: Char -> Bool
esVocal letra = elem letra "aeiou"

unirNombres :: [Comida] -> String
unirNombres = concat.map (nombreComida) 

quitarRepetidos :: [String] -> [String]
quitarRepetidos [] = []
quitarRepetidos (primerCadena:restoDeCadenas) = (primerCadena:).quitarRepetidos.filter (/=primerCadena) $ restoDeCadenas

unirTodosLosIngredientes :: [Comida] -> [Ingrediente]
unirTodosLosIngredientes = concat.map (ingredientes) 

compraDeluxe :: Persona -> [Comida] -> Persona
compraDeluxe persona = comprar persona.superComida.duplicarCostoDeComidas.quitarLasComidasDeMasde 400

quitarLasComidasDeMasde :: Int -> [Comida] -> [Comida]
quitarLasComidasDeMasde precio = filter (not.valeMenosDe precio) 

duplicarCostoDeComidas :: [Comida] -> [Comida]
duplicarCostoDeComidas = map (duplicarCosto) 

duplicarCosto :: Comida -> Comida
duplicarCosto comida = cambiarCostoDeComida (costo comida) comida





-- precio : suma de todos, nombre: todos sin vocales, ingredientes : todos sin repetir

