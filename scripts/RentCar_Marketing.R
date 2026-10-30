# CASO 3 - MARKETING DIGITAL Y DEMANDA DE ALQUILER DE VEHICULOS
# Juliana Perez, Veronica Sandoval y Camila Salamanca

# PARTE A - PREPARACION Y EXPLORACION DE DATOS
# Punto 1. Reconocimiento y validacion de la base

# Importar base de datos original
RentCar <- read.csv(
  "data/original/Base_datos_caso_3.csv"
)

# Verificar dimensiones y nombres de las variables
dim(RentCar)
colnames(RentCar)

# Revisar estructura de las variables
str(RentCar)

# Obtener resumen de las variables
summary(RentCar)

# Contar valores faltantes
sum(is.na(RentCar))

# Verificar fechas duplicadas
sum(duplicated(RentCar$Week))

# Revisar fechas de la base
class(RentCar$Week)

# Primera y ultima fecha registrada
min(RentCar$Week)
max(RentCar$Week)

# Verificar primeras fechas
head(RentCar$Week)

# Convertir Week a formato de fecha
RentCar$Week <- as.Date(RentCar$Week)

# Comprobar conversion
class(RentCar$Week)

# Verificar rango de fechas
min(RentCar$Week)
max(RentCar$Week)

# Verificar si la conversion produjo valores faltantes
sum(is.na(RentCar$Week))

# Verificar continuidad semanal de las fechas
fechas_esperadas <- seq(
  from = min(RentCar$Week),
  to = max(RentCar$Week),
  by = "week"
)

# Cantidad de semanas esperadas
length(fechas_esperadas)

# Cantidad de semanas registradas
nrow(RentCar)

# Identificar semanas faltantes
setdiff(fechas_esperadas, RentCar$Week)
