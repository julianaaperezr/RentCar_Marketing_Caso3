# CASO 3 - MARKETING DIGITAL Y DEMANDA DE ALQUILER DE VEHICULOS
# Juliana Perez, Veronica Sandoval y Camila Salamanca

library(dplyr)
install.packages("ggplot2")
library(ggplot2)


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

# Punto 2 - Construccion de los canales de Amazon
# Identificar las columnas originales de publicidad
colnames(RentCar)

# Identificar columnas de costos de publicidad
colnames(RentCar)[1:17]

# Construir costos agregados de los canales de Amazon
RentCar <- RentCar %>%
  mutate(
    Amazon_DSP_Cost =
      DSP...Evergreen.Cost +
      DSP...Seasonal.Cost,
    
    Amazon_Search_Cost =
      Search...Men...Brand.Cost +
      Search...Men...Non.Brand.Cost +
      Search...Women...Brand.Cost +
      Search...Women...Non.Brand.Cost,
    
    Amazon_Sponsored_Cost =
      Sponsored.Brands.Paid.Search.Cost +
      Sponsored.Display.Non.Brand.Cost +
      Sponsored.Products.Brand.Search.Cost +
      Sponsored.Products.NonBrand.Search.Cost
  )

# Verificar las variables de costos construidas
summary(RentCar$Amazon_DSP_Cost)

summary(RentCar$Amazon_Search_Cost)

summary(RentCar$Amazon_Sponsored_Cost)

# Construir impresiones agregadas de los canales de Amazon
RentCar <- RentCar %>%
  mutate(
    Amazon_DSP_Impressions =
      DSP...Evergreen.Impression +
      DSP...Seasonal.Impression,
    
    Amazon_Search_Impressions =
      Search...Men...Brand.Impression +
      Search...Men...Non.Brand.Impression +
      Search...Women...Brand.Impression +
      Search...Women...Non.Brand.Impression,
    
    Amazon_Sponsored_Impressions =
      Sponsored.Brands.Paid.Search.Impression +
      Sponsored.Display.Non.Brand.Impression +
      Sponsored.Products.Brand.Search.Impression +
      Sponsored.Products.NonBrand.Search.Impression
  )

# Verificar las variables de impresiones construidas
summary(RentCar$Amazon_DSP_Impressions)

summary(RentCar$Amazon_Search_Impressions)

summary(RentCar$Amazon_Sponsored_Impressions)

# Verificar valores faltantes en las seis variables nuevas
sum(is.na(RentCar$Amazon_DSP_Cost))
sum(is.na(RentCar$Amazon_Search_Cost))
sum(is.na(RentCar$Amazon_Sponsored_Cost))

sum(is.na(RentCar$Amazon_DSP_Impressions))
sum(is.na(RentCar$Amazon_Search_Impressions))
sum(is.na(RentCar$Amazon_Sponsored_Impressions))

# Confirmar dimensiones de la base ampliada
dim(RentCar)

# Punto 3 - Ampliacion de la base con variables de control
# 3.1. Preparación de Google Trends Brand - Enterprise Rent-A-Car
# Importar referencia mensual de Google Trends

GoogleTrends_Referencia <- read.csv(
  "data/original/GoogleTrends_Enterprise_Referencia.csv",
  skip = 1,
  header = FALSE
)


# Importar datos semanales de 2024
GoogleTrends_2024 <- read.csv(
  "data/original/GoogleTrends_Enterprise_2024.csv",
  skip = 1,
  header = FALSE
)


# Importar datos semanales de 2025
GoogleTrends_2025 <- read.csv(
  "data/original/GoogleTrends_Enterprise_2025.csv",
  skip = 1,
  header = FALSE
)


# Importar datos semanales de 2026
GoogleTrends_2026 <- read.csv(
  "data/original/GoogleTrends_Enterprise_2026.csv",
  skip = 1,
  header = FALSE
)


# Convertir las fechas al formato Date
GoogleTrends_Referencia$V1 <- as.Date(
  GoogleTrends_Referencia$V1,
  format = "%Y-%m-%d"
)

GoogleTrends_2024$V1 <- as.Date(
  GoogleTrends_2024$V1,
  format = "%Y-%m-%d"
)

GoogleTrends_2025$V1 <- as.Date(
  GoogleTrends_2025$V1,
  format = "%Y-%m-%d"
)

GoogleTrends_2026$V1 <- as.Date(
  GoogleTrends_2026$V1,
  format = "%Y-%m-%d"
)


# Verificar estructura de los archivos
str(GoogleTrends_Referencia)
str(GoogleTrends_2024)
str(GoogleTrends_2025)
str(GoogleTrends_2026)


# Verificar cantidad de observaciones
nrow(GoogleTrends_Referencia)
nrow(GoogleTrends_2024)
nrow(GoogleTrends_2025)
nrow(GoogleTrends_2026)


# Verificar fechas iniciales y finales
min(GoogleTrends_Referencia$V1)
max(GoogleTrends_Referencia$V1)

min(GoogleTrends_2024$V1)
max(GoogleTrends_2024$V1)

min(GoogleTrends_2025$V1)
max(GoogleTrends_2025$V1)

min(GoogleTrends_2026$V1)
max(GoogleTrends_2026$V1)


# Verificar valores faltantes
sum(is.na(GoogleTrends_Referencia$V1))
sum(is.na(GoogleTrends_Referencia$V2))

sum(is.na(GoogleTrends_2024$V1))
sum(is.na(GoogleTrends_2024$V2))

sum(is.na(GoogleTrends_2025$V1))
sum(is.na(GoogleTrends_2025$V2))

sum(is.na(GoogleTrends_2026$V1))
sum(is.na(GoogleTrends_2026$V2))


# Preparar la serie semanal de 2024
GoogleTrends_2024_preparado <- GoogleTrends_2024 %>%
  select(V1, V2)

colnames(GoogleTrends_2024_preparado) <- c(
  "Fecha",
  "Indice_GoogleTrends"
)


# Preparar la serie semanal de 2025
GoogleTrends_2025_preparado <- GoogleTrends_2025 %>%
  select(V1, V2)

colnames(GoogleTrends_2025_preparado) <- c(
  "Fecha",
  "Indice_GoogleTrends"
)


# Preparar la serie semanal de 2026
GoogleTrends_2026_preparado <- GoogleTrends_2026 %>%
  select(V1, V2)

colnames(GoogleTrends_2026_preparado) <- c(
  "Fecha",
  "Indice_GoogleTrends"
)


# Verificar semanas compartidas entre las descargas
# Semana compartida entre 2024 y 2025
GoogleTrends_2024_preparado %>%
  filter(Fecha == as.Date("2024-12-29"))

GoogleTrends_2025_preparado %>%
  filter(Fecha == as.Date("2024-12-29"))


# Semana compartida entre 2025 y 2026
GoogleTrends_2025_preparado %>%
  filter(Fecha == as.Date("2025-12-28"))

GoogleTrends_2026_preparado %>%
  filter(Fecha == as.Date("2025-12-28"))

# 3.2. Empalmar las series semanales de Google Trends Brand
# Factores de ajuste usando las semanas compartidas
factor_2025 <- 66 / 55
factor_2026 <- factor_2025 * (70 / 45)

# Mantener 2024 como escala de referencia
Brand_2024 <- GoogleTrends_2024_preparado %>%
  mutate(Brand = Indice_GoogleTrends) %>%
  select(Fecha, Brand)

# Ajustar la escala de 2025
Brand_2025 <- GoogleTrends_2025_preparado %>%
  mutate(Brand = Indice_GoogleTrends * factor_2025) %>%
  filter(Fecha > as.Date("2024-12-29")) %>%
  select(Fecha, Brand)

# Ajustar la escala de 2026
Brand_2026 <- GoogleTrends_2026_preparado %>%
  mutate(Brand = Indice_GoogleTrends * factor_2026) %>%
  filter(Fecha > as.Date("2025-12-28")) %>%
  select(Fecha, Brand)

# Construir la serie completa
GoogleTrends_Brand <- dplyr::bind_rows(
  Brand_2024,
  Brand_2025,
  Brand_2026
)

# Verificar cobertura
nrow(GoogleTrends_Brand)
min(GoogleTrends_Brand$Fecha)
max(GoogleTrends_Brand$Fecha)

# 3.3. Preparación de Google Trends Non-Brand - Car Rental
# Importar los tres archivos semanales
NonBrand_2024 <- read.csv(
  "data/original/GoogleTrends_CarRental_2024.csv",
  skip = 1,
  header = FALSE
)

NonBrand_2025 <- read.csv(
  "data/original/GoogleTrends_CarRental_2025.csv",
  skip = 1,
  header = FALSE
)

NonBrand_2026 <- read.csv(
  "data/original/GoogleTrends_CarRental_2026.csv",
  skip = 1,
  header = FALSE
)


# Convertir las fechas
NonBrand_2024$V1 <- as.Date(NonBrand_2024$V1, format = "%Y-%m-%d")
NonBrand_2025$V1 <- as.Date(NonBrand_2025$V1, format = "%Y-%m-%d")
NonBrand_2026$V1 <- as.Date(NonBrand_2026$V1, format = "%Y-%m-%d")


# Preparar los nombres de las columnas
colnames(NonBrand_2024) <- c("Fecha", "NonBrand")
colnames(NonBrand_2025) <- c("Fecha", "NonBrand")
colnames(NonBrand_2026) <- c("Fecha", "NonBrand")


# Verificar cobertura y fechas compartidas
nrow(NonBrand_2024)
nrow(NonBrand_2025)
nrow(NonBrand_2026)

tail(NonBrand_2024, 1)
head(NonBrand_2025, 1)

tail(NonBrand_2025, 1)
head(NonBrand_2026, 1)

# 3.4. Empalme de Google Trends Non-Brand
# Calcular factores de ajuste
factor_nonbrand_2025 <- 75 / 63
factor_nonbrand_2026 <- factor_nonbrand_2025 * (67 / 61)

# Mantener 2024 como referencia
NonBrand_2024_ajustado <- NonBrand_2024 %>%
  select(Fecha, NonBrand)

# Ajustar 2025
NonBrand_2025_ajustado <- NonBrand_2025 %>%
  mutate(NonBrand = NonBrand * factor_nonbrand_2025) %>%
  filter(Fecha > as.Date("2024-12-29")) %>%
  select(Fecha, NonBrand)

# Ajustar 2026
NonBrand_2026_ajustado <- NonBrand_2026 %>%
  mutate(NonBrand = NonBrand * factor_nonbrand_2026) %>%
  filter(Fecha > as.Date("2025-12-28")) %>%
  select(Fecha, NonBrand)

# Construir una sola serie semanal
GoogleTrends_NonBrand <- dplyr::bind_rows(
  NonBrand_2024_ajustado,
  NonBrand_2025_ajustado,
  NonBrand_2026_ajustado
)

# Verificar cobertura
nrow(GoogleTrends_NonBrand)
min(GoogleTrends_NonBrand$Fecha)
max(GoogleTrends_NonBrand$Fecha)

# 3.5. Preparación de la variable de control: precio de gasolina
# Importar datos semanales de gasolina
Gasolina_USA <- read.csv(
  "data/original/Gasolina_USA.csv"
)

# Convertir fechas
Gasolina_USA$observation_date <- as.Date(
  Gasolina_USA$observation_date,
  format = "%Y-%m-%d"
)

# Revisar estructura y fechas
str(Gasolina_USA)
head(Gasolina_USA)
tail(Gasolina_USA)

# 3.6. Preparación semanal del precio de gasolina
# Seleccionar el período de análisis
Gasolina_USA_preparada <- Gasolina_USA %>%
  filter(
    observation_date >= as.Date("2024-01-01"),
    observation_date <= as.Date("2026-10-26")
  ) %>%
  select(observation_date, GASREGW)

# Renombrar las columnas
colnames(Gasolina_USA_preparada) <- c(
  "Fecha",
  "Precio_Gasolina"
)

# Verificar cobertura
nrow(Gasolina_USA_preparada)
head(Gasolina_USA_preparada)
tail(Gasolina_USA_preparada)

# 3.7. Preparación de la tasa de interés de Estados Unidos
# Importar datos diarios
Tasa_Interes_USA <- read.csv(
  "data/original/Tasa_Interes_USA.csv"
)

# Convertir fechas
Tasa_Interes_USA$observation_date <- as.Date(
  Tasa_Interes_USA$observation_date,
  format = "%Y-%m-%d"
)

# Seleccionar período de análisis
Tasa_Interes_USA_preparada <- Tasa_Interes_USA %>%
  filter(
    observation_date >= as.Date("2024-01-01"),
    observation_date <= as.Date("2026-10-30")
  )

# Verificar estructura y cobertura
str(Tasa_Interes_USA_preparada)
head(Tasa_Interes_USA_preparada)
tail(Tasa_Interes_USA_preparada)

# 3.8. Preparación semanal de la tasa de interés
# Crear fechas semanales correspondientes a RentCar
Fechas_Semanales <- data.frame(
  Fecha = seq(
    as.Date("2024-01-07"),
    as.Date("2026-10-25"),
    by = "week"
  )
)

# Para cada domingo, seleccionar la última observación disponible
Tasa_Interes_Semanal <- Fechas_Semanales %>%
  left_join(
    Tasa_Interes_USA_preparada %>%
      mutate(Fecha = observation_date),
    by = "Fecha"
  ) %>%
  select(Fecha, DFF)

# Renombrar la variable
colnames(Tasa_Interes_Semanal) <- c(
  "Fecha",
  "Tasa_Interes"
)

# Verificar resultados
nrow(Tasa_Interes_Semanal)
head(Tasa_Interes_Semanal)
tail(Tasa_Interes_Semanal)
sum(is.na(Tasa_Interes_Semanal$Tasa_Interes))

colnames(RentCar)

# 3.9. Integración de variables de control con RentCar
# Confirmar formato de las fechas
str(RentCar$Week)

# Convertir fechas si es necesario
RentCar$Week <- as.Date(RentCar$Week)

# Alinear gasolina con las semanas de RentCar
# La observación del lunes se asigna al domingo siguiente

Gasolina_Semanal <- Gasolina_USA_preparada %>%
  mutate(Fecha = Fecha + 6)

# Integrar las cuatro variables
RentCar_Ampliada <- RentCar %>%
  left_join(
    GoogleTrends_Brand,
    by = c("Week" = "Fecha")
  ) %>%
  left_join(
    GoogleTrends_NonBrand,
    by = c("Week" = "Fecha")
  ) %>%
  left_join(
    Gasolina_Semanal %>%
      select(Fecha, Precio_Gasolina),
    by = c("Week" = "Fecha")
  ) %>%
  left_join(
    Tasa_Interes_Semanal,
    by = c("Week" = "Fecha")
  )

# Verificar tamaño de la muestra
nrow(RentCar)
nrow(RentCar_Ampliada)

# Verificar valores faltantes en las nuevas variables
sum(is.na(RentCar_Ampliada$Brand))
sum(is.na(RentCar_Ampliada$NonBrand))
sum(is.na(RentCar_Ampliada$Precio_Gasolina))
sum(is.na(RentCar_Ampliada$Tasa_Interes))

# Verificar primeras observaciones
RentCar_Ampliada %>%
  select(
    Week,
    Brand,
    NonBrand,
    Precio_Gasolina,
    Tasa_Interes
  ) %>%
  head()

# 3.10. Ajuste temporal de las variables económicas
# Conservar la base integrada original
RentCar_Ampliada_Preliminar <- RentCar_Ampliada

# Usar información económica de dos semanas anteriores
RentCar_Ampliada <- RentCar_Ampliada %>%
  mutate(
    Precio_Gasolina = lag(Precio_Gasolina, 2),
    Tasa_Interes = lag(Tasa_Interes, 2)
  )

# Verificar muestra y valores faltantes
nrow(RentCar_Ampliada)

sum(is.na(RentCar_Ampliada$Precio_Gasolina))
sum(is.na(RentCar_Ampliada$Tasa_Interes))

# 3.10. Ajuste temporal de las variables económicas
# Conservar la base integrada preliminar
RentCar_Ampliada_Preliminar <- RentCar_Ampliada

# Preparar gasolina incluyendo semanas anteriores
Gasolina_Historica <- Gasolina_USA %>%
  filter(
    observation_date >= as.Date("2023-12-01"),
    observation_date <= as.Date("2026-10-26")
  ) %>%
  mutate(Fecha = observation_date + 6) %>%
  select(Fecha, GASREGW)

colnames(Gasolina_Historica) <- c(
  "Fecha",
  "Precio_Gasolina"
)

# Preparar tasa de interés incluyendo semanas anteriores
Tasa_Historica <- Tasa_Interes_USA %>%
  filter(
    observation_date >= as.Date("2023-12-01"),
    observation_date <= as.Date("2026-10-29")
  ) %>%
  filter(as.POSIXlt(observation_date)$wday == 0) %>%
  select(observation_date, DFF)

colnames(Tasa_Historica) <- c(
  "Fecha",
  "Tasa_Interes"
)

# Aplicar rezago de dos semanas mediante las fechas
Gasolina_Historica <- Gasolina_Historica %>%
  mutate(Fecha = Fecha + 14)

Tasa_Historica <- Tasa_Historica %>%
  mutate(Fecha = Fecha + 14)

# Reconstruir la base ampliada
RentCar_Ampliada <- RentCar %>%
  left_join(
    GoogleTrends_Brand,
    by = c("Week" = "Fecha")
  ) %>%
  left_join(
    GoogleTrends_NonBrand,
    by = c("Week" = "Fecha")
  ) %>%
  left_join(
    Gasolina_Historica,
    by = c("Week" = "Fecha")
  ) %>%
  left_join(
    Tasa_Historica,
    by = c("Week" = "Fecha")
  )

# Validación final
nrow(RentCar_Ampliada)
ncol(RentCar_Ampliada)

sum(is.na(RentCar_Ampliada$Brand))
sum(is.na(RentCar_Ampliada$NonBrand))
sum(is.na(RentCar_Ampliada$Precio_Gasolina))
sum(is.na(RentCar_Ampliada$Tasa_Interes))

# PUNTO 4 - EXPLORACION Y ESTADISTICAS DESCRIPTIVAS
# Ordenar las semanas para estudiar los datos en el tiempo.

Datos_4 <- RentCar_Ampliada %>% arrange(Week)

# Seleccionar las variables
variables_4 <- c("Sales", "Orders", "Amazon_DSP_Cost", "Amazon_Search_Cost", "Amazon_Sponsored_Cost", "Amazon_DSP_Impressions", "Amazon_Search_Impressions", "Amazon_Sponsored_Impressions", "SP500", "WEI", "Brand", "NonBrand", "Precio_Gasolina", "Tasa_Interes")

# Comprobar que las columnas necesarias existan.
stopifnot(all(variables_4 %in% names(Datos_4)))

# Preparar una tabla con las variables numericas.
tabla_4 <- Datos_4 %>% select(all_of(variables_4))

# Calcular cantidad de datos, media, desviacion, minimo y maximo por variable.
descriptivos_4 <- data.frame(Variable = variables_4, N = sapply(tabla_4, function(x) sum(!is.na(x))), Media = sapply(tabla_4, function(x) mean(x, na.rm = TRUE)), Desviacion = sapply(tabla_4, function(x) sd(x, na.rm = TRUE)), Minimo = sapply(tabla_4, function(x) min(x, na.rm = TRUE)), Maximo = sapply(tabla_4, function(x) max(x, na.rm = TRUE)))

# Mostrar la tabla de estadisticas descriptivas.
print(descriptivos_4)

# Dibujar la evolucion semanal de las ventas.
print(ggplot(Datos_4, aes(x = Week, y = Sales)) + geom_line(color = "steelblue") + labs(title = "Ventas por semana", x = "Semana", y = "Sales") + theme_minimal())

# Dibujar la evolucion semanal de las ordenes.
print(ggplot(Datos_4, aes(x = Week, y = Orders)) + geom_line(color = "darkgreen") + labs(title = "Ordenes por semana", x = "Semana", y = "Orders") + theme_minimal())

# Preparar una tabla con los tres costos de Amazon.
costos_4 <- Datos_4 %>% select(Week, Amazon_DSP_Cost, Amazon_Search_Cost, Amazon_Sponsored_Cost)

# Convertir los tres costos en dos columnas para graficarlos juntos.
costos_largos_4 <- tidyr::pivot_longer(costos_4, cols = -Week, names_to = "Canal", values_to = "Costo")

# Mostrar los costos de los tres canales a lo largo del tiempo.
print(ggplot(costos_largos_4, aes(x = Week, y = Costo, color = Canal)) + geom_line() + labs(title = "Costos semanales de Amazon", x = "Semana", y = "Costo") + theme_minimal())

# Mostrar la relacion entre el costo DSP y las ventas.
print(ggplot(Datos_4, aes(x = Amazon_DSP_Cost, y = Sales)) + geom_point() + geom_smooth(method = "lm", se = FALSE) + labs(title = "DSP y ventas") + theme_minimal())

# Mostrar la relacion entre el costo Search y las ventas.
print(ggplot(Datos_4, aes(x = Amazon_Search_Cost, y = Sales)) + geom_point() + geom_smooth(method = "lm", se = FALSE) + labs(title = "Search y ventas") + theme_minimal())

# Mostrar la relacion entre el costo Sponsored y las ventas.
print(ggplot(Datos_4, aes(x = Amazon_Sponsored_Cost, y = Sales)) + geom_point() + geom_smooth(method = "lm", se = FALSE) + labs(title = "Sponsored y ventas") + theme_minimal())

# Calcular la matriz de covarianzas con las variables seleccionadas.
covarianzas_4 <- cov(tabla_4, use = "pairwise.complete.obs")

# Mostrar las covarianzas de Sales con las demas variables.
print(covarianzas_4["Sales", , drop = FALSE])

# Calcular la matriz de correlaciones para comparar distintas escalas.
correlaciones_4 <- cor(tabla_4, use = "pairwise.complete.obs")

# Mostrar la matriz de correlaciones.
print(round(correlaciones_4, 3))

# Mostrar las cinco semanas con mayores ventas.
print(Datos_4 %>% arrange(desc(Sales)) %>% select(Week, Sales, Orders) %>% head(5))

# Mostrar las cinco semanas con mayores ordenes.
print(Datos_4 %>% arrange(desc(Orders)) %>% select(Week, Sales, Orders) %>% head(5))

# PUNTO 5 - TRES REGRESIONES SIMPLES

# Estimar la relacion entre las ventas y el costo DSP.
modelo_5_DSP <- lm(Sales ~ Amazon_DSP_Cost, data = Datos_4)

# Mostrar coeficientes, valores p y R cuadrado del modelo DSP.
summary(modelo_5_DSP)

# Mostrar intervalos de confianza del 95 por ciento del modelo DSP.
confint(modelo_5_DSP)

# Estimar la relacion entre las ventas y el costo Search.
modelo_5_Search <- lm(Sales ~ Amazon_Search_Cost, data = Datos_4)

# Mostrar los resultados del modelo Search.
summary(modelo_5_Search)

# Mostrar intervalos de confianza del modelo Search.
confint(modelo_5_Search)

# Estimar la relacion entre las ventas y el costo Sponsored.
modelo_5_Sponsored <- lm(Sales ~ Amazon_Sponsored_Cost, data = Datos_4)

# Mostrar los resultados del modelo Sponsored.
summary(modelo_5_Sponsored)

# Mostrar intervalos de confianza del modelo Sponsored.
confint(modelo_5_Sponsored)

# Comparar los coeficientes de los tres modelos simples.
print(data.frame(Canal = c("DSP", "Search", "Sponsored"), Coeficiente = c(coef(modelo_5_DSP)[2], coef(modelo_5_Search)[2], coef(modelo_5_Sponsored)[2]), R2 = c(summary(modelo_5_DSP)$r.squared, summary(modelo_5_Search)$r.squared, summary(modelo_5_Sponsored)$r.squared)))

# PUNTO 6 - REGRESIONES MULTIPLES CON CONTROLES

# Revisar las correlaciones entre las variables externas antes de elegir controles.
print(round(cor(Datos_4 %>% select(SP500, WEI, Brand, NonBrand, Precio_Gasolina, Tasa_Interes), use = "pairwise.complete.obs"), 2))

# Elegir Brand, WEI, gasolina y tasa como controles iniciales; revisar su dependencia con los resultados.
controles_6 <- c("Brand", "WEI", "Precio_Gasolina", "Tasa_Interes")

# Mantener una muestra comun sin datos faltantes y ordenada por semana.
Datos_modelos <- Datos_4 %>% select(Week, Sales, Orders, Amazon_DSP_Cost, Amazon_Search_Cost, Amazon_Sponsored_Cost, all_of(controles_6)) %>% tidyr::drop_na() %>% arrange(Week)

# Mostrar cuantas semanas se pueden usar en las regresiones.
nrow(Datos_modelos)

# Crear la formula que usaran las regresiones.
formula_ventas <- Sales ~ Amazon_DSP_Cost + Amazon_Search_Cost + Amazon_Sponsored_Cost + Brand + WEI + Precio_Gasolina + Tasa_Interes

# Estimar las ventas usando los costos y los controles.
modelo_6_Sales <- lm(formula_ventas, data = Datos_modelos)

# Mostrar coeficientes, pruebas t, valores p, R cuadrado y prueba F.
summary(modelo_6_Sales)

# Mostrar los intervalos de confianza del 95 por ciento.
confint(modelo_6_Sales)

# Crear la formula para explicar las ordenes.
formula_ordenes <- update(formula_ventas, Orders ~ .)

# Estimar las ordenes usando los mismos costos y controles.
modelo_6_Orders <- lm(formula_ordenes, data = Datos_modelos)

# Mostrar los resultados de las ordenes.
summary(modelo_6_Orders)

# Mostrar intervalos de confianza para las ordenes.
confint(modelo_6_Orders)

# Recordar que H0 dice que cada coeficiente es cero y H1 que es diferente de cero.
# Un valor p menor a 0.05 aporta evidencia contra H0, pero no prueba causalidad.

# PUNTO 7 - LOGARITMOS Y TEMPORADA ALTA

# Comprobar que las ventas y ordenes sean positivas.
stopifnot(all(Datos_modelos$Sales > 0), all(Datos_modelos$Orders > 0))

# Crear una variable de temporada alta usando junio, julio, noviembre y diciembre como hipotesis de calendario.
Datos_modelos <- Datos_modelos %>% mutate(Mes = as.integer(format(Week, "%m")), Temporada_Alta = ifelse(Mes %in% c(6, 7, 11, 12), 1, 0))

# Revisar cuantas semanas se clasificaron como temporada alta.
table(Datos_modelos$Temporada_Alta)

# Dividir los datos por fecha: primeras 80 por ciento semanas para entrenar.
corte_7 <- floor(nrow(Datos_modelos) * 0.80)

# Preparar las semanas de entrenamiento.
entrenamiento_7 <- Datos_modelos[1:corte_7, ]

# Preparar las semanas posteriores para evaluar predicciones.
evaluacion_7 <- Datos_modelos[(corte_7 + 1):nrow(Datos_modelos), ]

# Crear la formula de ventas con logaritmo natural.
formula_log_sales <- update(formula_ventas, log(Sales) ~ .)

# Crear la formula de ordenes con logaritmo natural.
formula_log_orders <- update(formula_ordenes, log(Orders) ~ .)

# Crear la formula de ventas con una dummy de temporada alta.
formula_dummy_sales <- update(formula_ventas, . ~ . + Temporada_Alta)

# Crear la formula de ordenes con una dummy de temporada alta.
formula_dummy_orders <- update(formula_ordenes, . ~ . + Temporada_Alta)

# Entrenar el modelo de ventas con logaritmos.
modelo_7_log_Sales <- lm(formula_log_sales, data = entrenamiento_7)

# Mostrar sus resultados.
summary(modelo_7_log_Sales)

# Entrenar el modelo de ordenes con logaritmos.
modelo_7_log_Orders <- lm(formula_log_orders, data = entrenamiento_7)

# Mostrar sus resultados.
summary(modelo_7_log_Orders)

# Entrenar el modelo de ventas con temporada alta.
modelo_7_dummy_Sales <- lm(formula_dummy_sales, data = entrenamiento_7)

# Mostrar sus resultados.
summary(modelo_7_dummy_Sales)

# Entrenar el modelo de ordenes con temporada alta.
modelo_7_dummy_Orders <- lm(formula_dummy_orders, data = entrenamiento_7)

# Mostrar sus resultados.
summary(modelo_7_dummy_Orders)

# Calcular el factor de correccion al volver de logaritmos a la escala original.
factor_log_sales <- mean(exp(residuals(modelo_7_log_Sales)))

# Calcular el mismo factor para ordenes.
factor_log_orders <- mean(exp(residuals(modelo_7_log_Orders)))

# Predecir ventas del modelo logaritmico en la escala original.
pred_log_sales <- exp(predict(modelo_7_log_Sales, newdata = evaluacion_7)) * factor_log_sales

# Predecir ventas del modelo de temporada alta.
pred_dummy_sales <- predict(modelo_7_dummy_Sales, newdata = evaluacion_7)

# Predecir ordenes del modelo logaritmico en la escala original.
pred_log_orders <- exp(predict(modelo_7_log_Orders, newdata = evaluacion_7)) * factor_log_orders

# Predecir ordenes del modelo de temporada alta.
pred_dummy_orders <- predict(modelo_7_dummy_Orders, newdata = evaluacion_7)

# Crear una funcion sencilla para calcular el error absoluto medio.
calcular_MAE <- function(real, predicho) mean(abs(real - predicho))

# Crear una funcion para calcular la raiz del error cuadratico medio.
calcular_RMSE <- function(real, predicho) sqrt(mean((real - predicho)^2))

# Comparar los errores de prediccion de ambos caminos en la misma escala.
comparacion_7 <- data.frame(KPI = c("Sales", "Sales", "Orders", "Orders"), Camino = c("Logaritmo", "Temporada alta", "Logaritmo", "Temporada alta"), MAE = c(calcular_MAE(evaluacion_7$Sales, pred_log_sales), calcular_MAE(evaluacion_7$Sales, pred_dummy_sales), calcular_MAE(evaluacion_7$Orders, pred_log_orders), calcular_MAE(evaluacion_7$Orders, pred_dummy_orders)), RMSE = c(calcular_RMSE(evaluacion_7$Sales, pred_log_sales), calcular_RMSE(evaluacion_7$Sales, pred_dummy_sales), calcular_RMSE(evaluacion_7$Orders, pred_log_orders), calcular_RMSE(evaluacion_7$Orders, pred_dummy_orders)))

# Mostrar la tabla de comparacion: errores menores indican mejores predicciones en este periodo.
print(comparacion_7)

# Recordar que el logaritmo cambia la escala y la dummy compara meses de temporada alta con otros meses.
# Revisar y justificar los meses elegidos con el calendario comercial antes de entregar.
