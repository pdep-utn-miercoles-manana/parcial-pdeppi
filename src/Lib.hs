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



