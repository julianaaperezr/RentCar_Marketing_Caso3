# CASO 3 - MARKETING DIGITAL Y DEMANDA DE ALQUILER DE VEHICULOS
# Juliana Perez, Veronica Sandoval y Camila Salamanca

library(dplyr)

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

# Identificar variables de respuesta
summary(RentCar$Sales)
summary(RentCar$Orders)

# Revisar valores de las variables de respuesta
min(RentCar$Sales)
max(RentCar$Sales)

min(RentCar$Orders)
max(RentCar$Orders)

# Revisar las variables economicas disponibles
summary(RentCar$SP500)
summary(RentCar$WEI)

# Revisar distribucion de las variables de respuesta
summary(RentCar$Sales)
sd(RentCar$Sales)

summary(RentCar$Orders)
sd(RentCar$Orders)

# Identificar semanas con ventas extremas
RentCar %>%
  filter(Sales == max(Sales))

RentCar %>%
  filter(Sales == min(Sales))

# Identificar semanas con pedidos extremos
RentCar %>%
  filter(Orders == max(Orders))

RentCar %>%
  filter(Orders == min(Orders))

# Resumir semanas con ventas extremas
RentCar %>%
  filter(Sales == max(Sales)) %>%
  select(Week, Sales, Orders)

RentCar %>%
  filter(Sales == min(Sales)) %>%
  select(Week, Sales, Orders)

# Resumir semanas con pedidos extremos
RentCar %>%
  filter(Orders == max(Orders)) %>%
  select(Week, Sales, Orders)

RentCar %>%
  filter(Orders == min(Orders)) %>%
  select(Week, Sales, Orders)

# Revisar nombres de las variables de publicidad
colnames(RentCar)

# Revisar la estructura de las variables
str(RentCar)

# Validacion inicial completada:
# 135 observaciones y 33 variables.
# Sin valores faltantes, semanas duplicadas ni semanas ausentes.
# Week se convirtio a Date sin perder observaciones.
# Se conservaron los datos originales sin eliminar registros.
