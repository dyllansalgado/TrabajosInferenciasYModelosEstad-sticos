
#------------------------------------------------------------------------------
# Importar los paquetes, instalándolos de ser necesario.
if (!require(tidyverse)) {
  install.packages("tidyverse", dependencies = TRUE)
  require(tidyverse) }
if (!require(ggpubr)) {
  install.packages("ggpubr", dependencies = TRUE)
  require(ggpubr) }
if (!require(leaps)) {
  install.packages("leaps", dependencies = TRUE)
  require(leaps) }
if (!require(car)){
  install.packages("car", dependencies = TRUE)
}
if (!require(ggplot2)){
  install.packages("ggplot2", dependencies = TRUE)
}
#dir <- setwd("C:\\Users\\Dyllan\\Desktop\\TrabajosIME\\EP-13")
#Se leen los datos y se guardan.
datos <- read.csv2(file.choose())
#Se establece un alfa.
alfa <- 0.01

#Enunciado:
#Un estudio recolectó medidas anatómicas de 247 hombres y 260 mujeres 
#(Heinz et al., 2003). Estas mediciones están disponibles en el archivo 
#EP13 Datos.csv que acompaña a este enunciado. El estudio incluyó nueve
#mediciones del esqueleto (ocho diámetros y una profundidad de hueso a hueso) y 
#doce mediciones de grosor (circunferencias) que incluyen el tejido.

#1. Definir la semilla a utilizar, que corresponde a los últimos cuatro dígitos 
# del RUN (sin considerar el dígito verificador) del integrante de menor edad del 
# equipo.

#Se establece una semilla con los últimos 4 dígitos finales del RUN del integrante
# de menor edad del grupo
set.seed(7250)

#2. Seleccionar una muestra de 50 mujeres (si la semilla es un número par) o 
# 50 hombres (si la semilla es impar).

#Como la semilla ha terminado en 0, se asume que es un número par. Por lo 
# que se selecciona una muestra con 50 mujeres de forma aleatoria.

# Se obtienen solo las mujeres solicitadas por el enunciado.
mujeres_datos <- datos %>% filter (Gender == 0 )

#Se selecciona una muestra aleatoria de 50 mujeres.
mujeres_datos <- sample_n(mujeres_datos, 50)

#3. Seleccionar de forma aleatoria ocho posibles variables predictoras.

#Se seleccionan de forma aleatoria ocho posibles variables predictoras.
#Se saca la columna peso, ya que es la variable respuesta y no un predictor.
columnasSinPeso <- select(mujeres_datos,!Weight)
#Se crea un sample con los 8 predictores solicitados de forma aleatoria.
columnasRandoms <- sample(colnames(columnasSinPeso),8,replace = FALSE)
print(columnasRandoms)
#Se seleccionan de la matriz mujeres_datos solo las columnas seleccionadas
# de forma aleatoria.
mujeres_VarPredictoras <- mujeres_datos %>% select(columnasRandoms)


#4. Seleccionar, entre las variables que no fueron escogidas en el punto 
#anterior, una que el equipo considere que podría ser útil para predecir la 
#variable Peso, justificando bien esta selección.

#El primer paso consiste en obtener solamente la columna Peso, ya que corresponde
# a nuestra variable de respuesta.
columnaPeso<- select(mujeres_datos,Weight)
#Luego se realiza la correlación de todas las demás columnas comparadas 
# con la columnaPeso. Se utiliza la función cor(x,y) donde x: corresponde
# a los predictores que no han sido seleccionados en la pregunta 3 e y corresponde
# a la variable respuesta.
columnasNoEscogidas <- mujeres_datos %>% select(!columnasRandoms)
#Se realiza la correlación.
correlacionDatos1 <- cor(columnasNoEscogidas,columnaPeso)
print(correlacionDatos1)
#Al analizar la matriz otorgada con el print(correlacionDatos1) se puede apreciar
# que el valor que más se acerca a 1 corresponde al predictor Hip.Girth con un
# R de 0.9158, esto quiere decir que esta correlación es bastante alta, por lo que
# se tiene una relación lineal muy fuerte.

#El siguiente cálculo de correlación corresponde a analizar todos los predictores
# de la matriz de mujeres_datos, para ver visualmente si calza el predictor
# seleccionado con un mayor R. Al analizarlo se puede observar que el predictor
# Hip.Girth sigue siendo el más cercano a 1 con 0.9158379
correlacionDatos2 <- cor(mujeres_datos)
print(correlacionDatos2)

# 5) Usando el entorno R, construir un modelo de regresión lineal simple con el 
#predictor seleccionado en el paso anterior.

#Antes de realizar el modelo de regresión simple, se deben verificar ciertos pasos:

#1)Los datos deben presentar una relación lineal.
#2)La distribución de los residuos debe ser cercana a la normal.
#3)La variabilidad de los puntos en torno a la línea de mínimos cuadrados 
#debe ser aproximadamente constante.
#4)Las observaciones deben ser independientes entre sí.

#Ajustar modelo de mínimos cuadrados  .
modelo <- lm( Weight ~ Hip.Girth , data = mujeres_datos )
print ( summary ( modelo ) )

#Graficar el modelo .
p <- ggscatter (
  mujeres_datos , x = "Weight", y = "Hip.Girth", color = " blue ", fill = " blue ",
  xlab = " Peso ", ylab = " Grosor altura a las caderas")

p <- p + geom_smooth ( method = lm , se = FALSE , colour = "red")

# Graficar los residuos .
b_1 <- modelo$coefficients[2]
b_0 <- modelo$coefficients[1]
residuos <- mujeres_datos[["Hip.Girth"]] - ( b_1 * mujeres_datos[["Weight"]] + b_0)
datos <- data.frame ( mujeres_datos , residuos )

r <- ggscatter ( datos , x = "Weight", y = "Hip.Girth", color = "blue",
                 fill = "blue", xlab = "Peso", ylab = "Residuo")

r <- r + geom_hline ( yintercept = 0 , colour = "red")

g <- ggarrange (p , r , ncol = 2 , nrow = 1)
print ( g )
# Verificar normalidad de los residuos .
cat (" Prueba de normalidad para los residuos \n")
print ( shapiro.test ( datos$residuos ) )

#Como el predictor seleccionado que corresponde a Hip.Girth y la respuesta que 
# es el peso tienen una correlación muy fuerte, ya que está muy cercano al 1. Se 
# puede concluir que los datos si tienen o siguen una tendencia central.

# Al analizar el valor obtenido por la prueba de shapiro.wilk se puede ver que
# este valor es mayor a nuestro nivel de significación de 0.01
# alfa < p-value, por lo que se puede concluir con un 95% de confianza que los 
# datos si siguen una distribución normal.

#La variabilidad de los puntos en torno a la línea de mínimos cuadrados 
#si es aproximadamente constante.

#Por último, las observaciones son independientes entre sí, pues han sido 
#seleccionadas de manera aleatoria con el sample y corresponden a menos del 
#10 % de la población.

#Al obtener el resultado del modelo de mínimos cuadrados se puede observar
# que el valor P de Hip.Girth corresponde a 2e-16 lo cual es un valor muy bajo
#, pero es distinto de 0 por lo que se puede concluir con un 99% de
# confiabilidad de que si existe regresión lineal entre x e y, o en este caso
# para Hip.Girth (Grosor a la altura de las caderas) y el peso.



##############################################################
#RLM
##############################################################



#6.Usando herramientas para la exploración de modelos del entorno R, escoger 
#entre dos y cinco predictores de entre las variables seleccionadas en los 
#puntos 3 y 4 (9 en total) para construir un modelo de regresión lineal múltiple.
columnaPregunta5<- select(mujeres_datos,Hip.Girth)
dataFrameRLM <- data.frame(columnaPregunta5,mujeres_VarPredictoras,columnaPeso)

# Ajustar modelo con todos los subconjuntos .
modeloRLM <- regsubsets(Weight ~ ., data = dataFrameRLM , method = "exhaustive",
                          nbest = 1 , nvmax = 5)

print (plot(modeloRLM))

#Como se puede ver en el gráfico, el predictor más adecuado para realizar la
# regresión corresponde a Hip.Girth y Elbows.diameter, ya que tienen
# coloreado la mayor cantidad de recuadros para las variables presentes
# en dicho modelo, además contienen la menor cantidad de bic.

#7). Evaluar los modelos y “arreglarlos” en caso de que tengan algún problema con 
#las condiciones que deben cumplir.

#Ajustando el modelo
nuevoRLM <- lm(Weight ~ Hip.Girth + Elbows.diameter , data = dataFrameRLM)
plot(nuevoRLM)
#A fin de que el modelo sea generalizable, tenemos que verificar el cumplimiento
# de ciertas condiciones:
#1) Independencia de los residuos:

#Comprobando independencia de los residuos.
cat("\nPrueba de Durbin-Watson para autocorrelaciones entre errores: \n")
print(durbinWatsonTest(nuevoRLM))
# Como el p value obtenido es mayor a nuestro nivel de significación, es decir
# alfa < 0.536, se puede concluir con un 99% de confianza que los residuos son 
# independientes

#2) Distribución normal de los residuos:
#Comprobando normalidad de los residuos.
cat("\nPrueba de normalidad para los residuos: \n")
print(shapiro.test(nuevoRLM$residuals))
#Como el p-value obtenido es mayor a nuestro nivel de significación, es decir
# alfa < 0.2184, se puede concluir con  un 99% de confianza que si cumple
# la condición de normalidad.

#3) Homocedasticidad de los residuos:
#Comprobando homocedasticidad de los residuos.
cat("\nPrueba de homocedasticidad para los residuos: \n")
print(ncvTest(nuevoRLM))
#Como el p-value obtenido es mayor a nuestro nivel de significación, es decir
# alfa < 0.029863, se puede concluir con un 99% de confianza que el supuesto de 
# homocedasticidad se cumple.

#4) Multicolinealidad:
#Comprobando la multicolinealidad.
vifs <- vif(nuevoRLM)
cat("\nVerificar la multicolinealidad: \n")
cat("- VIFs: \n")
print(vifs)
cat("- Tolerancias: \n")
print(1/vifs)
cat("- VIF medio: ", mean(vifs), "\n")

#Se puede ver que el VIF es mucho menor a 10, además no se encuentra 
# dentro de los valores críticos que son entre 5 y 2.5, al analizar las tolerancias
# los valores no son menores a 0.2 por lo que no serian críticos y asimismo no 
# se encuentran cercanos a 0.4, por último al analizar el promedio nos da un valor
# de 1.3 es mayor 1, pero al no ser una diferencia tan elevada se asumirá que 
# se cumple esta condición.

#8) Evaluar el poder predictivo del modelo en datos no utilizados para 
#construirlo (o utilizando validación cruzada).

#Una vez realizado el ajuste del modelo y revisión de condiciones, se procede a 
#reducir el dataframe para que solo contenga los predictores utilizados y la respuesta.

predictores <- names(coef(nuevoRLM))[-1]
dataFrameRLM <- dataFrameRLM[,c(predictores, "Weight")]

#Se construye una matriz de datos con la respuesta predicha, los residuos y algunas
#estadísticas para evaluar la influencia de las observaciones.

resultados <- data.frame(respuesta_predicha = fitted(nuevoRLM))

resultados[["residuos_estandarizados"]] <- rstandard(nuevoRLM)
resultados[["residuos_estudiantizados"]] <- rstudent(nuevoRLM)
resultados[["distancia_Cook"]] <- cooks.distance(nuevoRLM)
resultados[["dfbeta"]] <- dfbeta(nuevoRLM)
resultados[["dffit"]] <- dffits(nuevoRLM)
resultados[["apalancamiento"]] <- hatvalues(nuevoRLM)
resultados[["covratio"]] <- covratio(nuevoRLM)

cat("Identificación de los valores atípicos\n")
#Observaciones con residuos estandarizados fuera del 95% esperado.

sospechosos1 <- which(abs(resultados[["residuos_estandarizados"]]) > 1.96)
cat("Residuos estandarizados fuera del 95% esperado:", sospechosos1, "\n")

#Observaciones con distancia de Cook mayor a uno.
sospechosos2 <- which(resultados[["cooks.distance"]] > 1)
cat("Residuos con una distancia de Cook alta: ", sospechosos2, "\n")

#Observaciones con apalancamiento mayor igual al doble del apalancamiento
#promedio.
#vncol <- ncol(dataFrameRLM) +1
#vnrow <- nrow(dataFrameRLM)
apal_medio <- ((ncol(dataFrameRLM) +1) / nrow(dataFrameRLM))
sospechosos3 <- which(resultados[["apalancamiento"]] > 2*apal_medio)
cat("Residuos con apalancamiento fuera de rango: ", sospechosos3, "\n")

#Observaciones con DFBeta mayor o igual a 1.
sospechosos4 <- which(apply(resultados[["dfbeta"]] >=1,1,any))
names(sospechosos4) <- NULL
cat("Residuos con DFBeta >=1: ", sospechosos4,"\n")

#Observaciones con razón de covarianza fuera de rango.
inferior <- 1-3*apal_medio
superior <- 1+3*apal_medio
sospechosos5 <- which(resultados[["covratio"]] < inferior | resultados[["covratio"]] > superior)
cat("Residuos con razón de covarianza fuera de rango: ", sospechosos5, "\n")

#Resumen de valores sopechosos
sospechosos <- c(sospechosos1,sospechosos2,sospechosos3,sospechosos4,sospechosos5)
sospechosos <- sort(unique(sospechosos))

cat("Resumen de valores sospechosos \n")
cat("Apalancamiento promedio: ", apal_medio, "\n")
cat("Intervalo razón covarianza: [", inferior, "; ",superior, "]\n\n", sep = " ")

print(round(resultados[sospechosos, c("distancia_Cook", "apalancamiento", "covratio")], 3))


#Conclusión
#Al analizar los resultados obtenidos en la prueba de Durbin-Watson, donde se obtuvo un valor
#p=0.536 podemos decir que los residuos son independientes, luego en la prueba de Shapiro-Wilk
#a los residuos se obtiene un p=0.2184, lo cuál es mayor al nivel de significancia establecido
#por lo que los residuos no se distribuyen de una forma normal, luego al analizar el resultado
#de la prueba de multicolinealidad podemos interpretar que existe la probabilidad de que el 
#modelo se encuentre sesgado, por lo que finalmente el modelo RLS se ajusta mejor a los datos.











