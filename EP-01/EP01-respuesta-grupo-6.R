
# Importar un paquete , instalándolo de ser necesario.
# Se utiliza tidyverse, ya que contiene el paquete dplyr.
if (!require (tidyverse) ) {
  install.packages("tidyverse",dependencies = TRUE)
  require(tidyverse)
}
# Se setea el workspace (Carpeta donde se encuentran los archivos)
# (Asegurarse de cambiar la ruta de la carpeta antes de ejecutar el script, y que el archivo 
# csv esté en la carpeta).
setwd("C:\\Users\\Dyllan\\Desktop\\TrabajosIME")

# Leer un dataframe desde archivo csv .
datos <- read.csv2("EP01 Datos Covid.csv", stringsAsFactors=FALSE)

#Preguntas y respuestas:
# ¿Qué variables se han cargado?
# Respuesta: En total se han cargado 735 columnas y 17 filas,  de las cuales se encuentran 
# las variables de nombre de las regiones y la cantidad de infectados por Covid-19 por día.
#
# ¿Qué tipo tiene cada una de estas variables?
# Respuesta: El nombre de las regiones corresponde a una variable categórica de tipo 
# nominal y los infectados por día corresponde a una variable de tipo numérica discreta.
#
# ¿Qué escala parecen tener estas variables?
# Respuesta: La variable de las regiones tiene una escala nominal, dado que solo son 
# nombres y solo sirven para separar elementos, en cambio, las variables de los infectados 
# por día tiene una escala de razón, dado que existe un cero verdadero y estas variables 
# corresponden a datos discretos.


# PREGUNTA 1:

# ¿Qué día se produjo el mayor número de casos con síntomas en la región de Ñuble entre 
# el 01-nov-2020 y el 31-may-2021?
# Se selecciona la fila nuble.
region <- datos %>% filter ( Region == "nuble" )

# Ahora debemos seleccionar dentro de la fecha límite establecida:
fechaLimite <- region %>% select(X01.11.2020:X31.05.2021)

# Aplicamos la traspuesta de la matriz con fecha límite obteniendo una columna solo con los casos de covid.
numeroCasosFilas <- t(fechaLimite)

# Luego las fechas que están como string debemos pasarla a tipo fecha.
fecha <- as.Date(colnames(fechaLimite), format="X%d.%m.%Y")

# Ahora creamos la nueva matriz con los datos transformados.
matrizConPeriodos <- data.frame(fecha,numeroCasosFilas)

# Ahora que tenemos la nueva matriz con la fecha límite se puede buscar el maximo caso de covid.
# Se encuentra el mayor número de contagiados.

valormax <- matrizConPeriodos %>% filter(numeroCasosFilas == max(matrizConPeriodos$numeroCasosFilas))

# Ahora que se tiene el valor max, que en este caso son dos días, se debe mostrar por pantalla.
cat("Los dias donde se encontraron mas infectados en la region de nuble corresponden a:\n")
print(valormax)

# PREGUNTA 2:
# ¿Cuál fue el total de casos con síntomas para cada mes de este periodo?

# Se realiza una agrupación por mes y luego se realiza la suma de los casos.
sumaPorCadaMes <- matrizConPeriodos %>% group_by(fecha = lubridate::floor_date(fecha, 'month')) %>%
  summarize(casosTotales = sum(numeroCasosFilas))
# Formato para columna fecha, que debe ser llamada mes.
sumaPorCadaMes$fecha <- format(sumaPorCadaMes$fecha, "%B-%Y")
  colnames(sumaPorCadaMes)[1] <- "mes"

# Se muestra respuesta por pantalla.
cat("El total de contagios por cada mes son:\n")
print(sumaPorCadaMes)
