
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
if (!require(reshape)){
  install.packages("reshape", dependencies = TRUE)
  require(reshape)
}
if (!require(pROC)){
  install.packages("pROC", dependencies = TRUE)
  require(pROC)
}

if (!require(caret)){
  install.packages("caret", dependencies = TRUE)
  require(caret)
}

# Se setea el workspace
setwd("C:\\Users\\Dyllan\\Desktop\\TrabajosIME\\EP-13")
datos <- read.csv2("EP13 Datos.csv",encoding = "UTF-8")
#Se leen los datos y se guardan.
#datos <- read.csv2(file.choose())

#1.Definir la semilla a utilizar, que corresponde a los últimos cuatro dígitos 
# del RUN (sin considerar el dígito verificador) del integrante de mayor edad 
# del equipo.

#Se establece una semilla con los últimos 4 dígitos finales del RUN del 
# integrante de mayor edad del grupo.
set.seed(2186)
alfa <- 0.05
#Seleccionar una muestra de 120 mujeres (si la semilla es un número par) o 120 
# hombres (si la semilla es impar), asegurando que la mitad tenga estado 
# nutricional “sobrepeso” y la otra mitad “no sobrepeso”. Dividir esta muestra 
# en dos conjuntos: los datos de 80 personas (40 con EN “sobrepeso”) para 
# utilizar en la construcción de los modelos y 40 personas (20 con EN 
# “sobrepeso”) para poder evaluarlos. 

#Como la semilla es un número par se escoge una muestra de 120 mujeres.
mujeres_datos <- datos %>% filter (Gender == 0 )
#Se crea la variable IMC.

#Se selecciona la columna de Estatura que se encuentra en cm y se debe pasar a 
# metros.
estaturaMetros <- select(mujeres_datos,Height)/100
Peso <- select(mujeres_datos,Weight)
#Ahora que se tiene la estatura en metros se puede calcular el IMC.

IMC <- Peso/(estaturaMetros**2)

mujeres_datos <- data.frame(mujeres_datos,IMC)
#Se añade al dataframe la variable nueva llamada IMC
mujeres_datos <- rename(mujeres_datos, c(Weight.1="IMC"))
#Viendo la literatura ifelse(vector, valor_si_TRUE, valor_si_FALSE)
# ref: https://bookdown.org/jboscomendoza/r-principiantes4/if-else.html
mujeres_datos$EN <- ifelse(mujeres_datos$IMC >= 25.0, "sobrepeso","no sobrepeso")
mujeres_datos$EN <- factor(mujeres_datos$EN)

# 2)Seleccionar una muestra de 120 mujeres (si la semilla es un número par) 
#o 120 hombres (si la semilla es impar), asegurando que la mitad tenga estado 
#nutricional “sobrepeso” y la otra mitad “no sobrepeso”. Dividir esta muestra 
#en dos conjuntos: los datos de 80 personas (40 con EN “sobrepeso”) para utilizar
#en laconstrucción de los modelos y 40 personas (20 con EN “sobrepeso”) para 
#poder evaluarlos. 

#Como la semilla es un número par se seleccionan 120 mujeres donde 60 deben tener
# sobrepeso y 60 no sobre peso.
#En este caso para las mujeres con sobrepreso se utiliza la reposición, ya que
# sin reposición solo se han encontrado 45 mujeres.
sobrePeso <- sample_n(mujeres_datos %>% filter(EN == "sobrepeso"),60,replace=TRUE)
noSobrePeso <- sample_n(mujeres_datos %>% filter(EN == "no sobrepeso"),60,replace=FALSE)

#Ahora se deben crear 2 conjuntos de datos, donde el primero se tiene 40 mujeres
# con sobrepeso y 40 no sobrepeso. Y el segundo conjunto de datos se tiene 20 
# con sobrepreso y 20 no sobrepreso.

Conjunto40sobrepeso <- sobrePeso[1:40,]
Conjunto20sobrepeso <- sobrePeso[41:60,]
Conjunto40Nosobrepeso <- noSobrePeso[1:40,]
Conjunto20Nosobrepeso <- noSobrePeso[41:60,]

conjunto <- rbind(Conjunto40sobrepeso,Conjunto40Nosobrepeso)
evaluador <- rbind(Conjunto20sobrepeso,Conjunto20Nosobrepeso)


#3)Recordar las ocho posibles variables predictoras seleccionadas de forma 
#aleatoria en el ejercicio anterior.
# Las 8 variables predictoras del ep13 corresponden a :
#Navel.Girth, Chest.Girth, Elbows.diameter, Age, Bitrochanteric.diameter,
#Biacromial.diameter, Knees.diameter, Wrists.diameter

conjuntoVariables <- select(conjunto,Navel.Girth,Chest.Girth,Elbows.diameter,
                            Age,Bitrochanteric.diameter,Biacromial.diameter,
                            Knees.diameter,Wrists.diameter,EN)

evaluadorVariables <- select(evaluador,Navel.Girth,Chest.Girth,Elbows.diameter,
                            Age, Bitrochanteric.diameter,Biacromial.diameter,
                            Knees.diameter, Wrists.diameter,EN)

#4) Seleccionar, de las otras variables, una que el equipo considere que podría 
#ser útil para predecir la clase EN,justificando bien esta selección.

#Luego se realiza la correlación de todas las demás columnas comparadas 
# con la columnaIMC. Se utiliza la función cor(x,y) donde x: corresponde
# a los predictores que no han sido seleccionados en la pregunta 3 e y corresponde
# a la variable predictora.
columnasSinEN <- select(mujeres_datos,!EN)
columnasIMC <- select(mujeres_datos,IMC)
#Se realiza la correlación.
correlacionDatos1 <- cor(columnasSinEN,columnasIMC)
print(correlacionDatos1)
#Al analizar la matriz de correlación existen dos valores que se encuentras más
# cercano a 1, que corresponden a Weight con 0.86425087 y Waist.Girth 0.87073890.
# Como grupo se ha llegado a la conclusión que el predictor a utilizar sera el
# peso Weight, por el motivo que tiene mayor relación con el IMC y sera mucho más
# util para predecir la clase EN.

conjuntoVariables <- select(conjunto,Navel.Girth,Chest.Girth,Elbows.diameter,
                            Age,Bitrochanteric.diameter,Biacromial.diameter,
                            Knees.diameter,Wrists.diameter,EN, Weight)

evaluadorVariables <- select(evaluador,Navel.Girth,Chest.Girth,Elbows.diameter,
                             Age, Bitrochanteric.diameter,Biacromial.diameter,
                             Knees.diameter, Wrists.diameter,EN, Weight)

#5) Usando el entorno R y paquetes estándares, construir un modelo de regresión 
#logística con el predictor seleccionado en el paso anterior y utilizando de la 
#muestra obtenida.

conjuntoVariables$EN <- factor(conjuntoVariables$EN)
# Ajustar modelo .
modelo <- glm( EN ~ Weight , family = binomial(link="logit") , data = conjuntoVariables )
print (summary(modelo))
# Evaluar el modelo con el conjunto de evaluadorVariable .
cat (" Evaluación del modelo a partir del conjunto de evaluadorVariable :\n")
probs_e <- predict( modelo , evaluadorVariables , type = "response")
umbral <- 0.5
preds_e <- sapply( probs_e , function (p) ifelse ( p >= umbral ,"sobrepeso","no sobrepeso") )
preds_e <- factor( preds_e , levels=levels(evaluadorVariables[["EN"]]) )
graficoROC <- roc ( evaluadorVariables[["EN"]] , probs_e )
plot ( graficoROC )
#Al analizar el gráfico se observa que la curva se aleja bastante de la diagonal,
# lo que nos da un indicio de que el predictor seleccionado nos da un buen modelo.
matrizDeConfusion <- confusionMatrix ( preds_e , evaluadorVariables[["EN"]])
print ( matrizDeConfusion )
# Analizando la matriz de confusion se puede ver que el modelo tiene una
# exactitud del 80%, tiene una sensibilidad de un 75% y la especificidad de 85%.
# Este resultado nos indica que modelo se desempeña un poco mejor en identificar
# los elementos de la clase negativa o a su vez determinar cuán exacta es la
# asignación de elementos a la clase positiva(no sobrepeso).


#6)  Usando herramientas estándares para la exploración de modelos del entorno R, buscar 
#entre dos y cinco predictores de entre las 9 variables seleccionadas en pasos anteriores
#para construir un modelo de regresión logística múltiple.

# De lo observado en el punto 4, podemos notar que otras variables posibles a utilizar como
#preductoras corresponden a Chest Girth, Waist Girth, hip Girth, Tigh Girth, Bicep Girth y Weigth,
# pero como Waist Girth y Weigth (utilizada anteriormente) son aquellas con los valores más altos
# y similares entre ellos, es que serán utilizado sólo estos dos.


#Se agrega a los conjuntos deinifidos anteriormente, la variable predictora Waist Girth.
conjuntoVariables2 <- select(conjunto,Navel.Girth,Chest.Girth,Elbows.diameter,
                             Age,Bitrochanteric.diameter,Biacromial.diameter,
                             Knees.diameter,Wrists.diameter,EN, Weight, Waist.Girth)

evaluadorVariables2 <- select(evaluador,Navel.Girth,Chest.Girth,Elbows.diameter,
                              Age, Bitrochanteric.diameter,Biacromial.diameter,
                              Knees.diameter, Wrists.diameter,EN, Weight, Waist.Girth)

conjuntoVariables2$EN <- factor(conjuntoVariables$EN)

# Ajustar modelo para el predictor Waist Girth.
modelo_Waist <- glm( EN ~ Waist.Girth , family = binomial(link="logit") , data = conjuntoVariables2 )
print (summary(modelo_Waist))

# Ajustar modelo para el predictor Weight.
modelo_Weight <- glm( EN ~ Weight , family = binomial(link="logit") , data = conjuntoVariables2 )
print (summary(modelo_Weight))

#Ajuste de modelo con los dos predictores seleccionados
modelo_2_predictores <- glm( EN ~ Waist.Girth + Weight , family = binomial(link="logit") , data = conjuntoVariables2 )
print (summary(modelo_2_predictores))

# Comparación del modelo con cada uno de los predictores predictores.

cat (" Likelihood Ratio Test para los modelos con predictor a Waist Girth y Weight\n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
print(anova(modelo_Waist , modelo_Weight , test = "LRT") )

# Ajustar modelo con regresión escalonada.
cat("Modelo con regresión escalonada\n")
cat("--------------------------------------\n")
mejor <- step(modelo_Weight, scope = list(lower = modelo_Weight, upper = modelo_2_predictores),
              direction = "both", trace = 0)

print(summary(mejor))
#Del resultado obtenido se observa que el modelo que utiliza como variables 
#predictoras al peso y además a Waist Girth muestra similares resultados al 
#modelo anterior, esto debido a que ambos predictores poseen un nivel de
#correlación muy similar con respecto a la varible en estudio.

#7)Evaluar la confiabilidad de los modelos (i.e. que tengan un buen nivel de 
#ajuste y son generalizables) y “arreglarlos” en caso de que tengan algún problema.

# Ajustar modelo usando validación cruzada de 5 pliegues .
modelo <- train ( EN ~ Weight , data = conjuntoVariables , method = "glm",
                     family = binomial ( link = "logit") ,
                     trControl = trainControl(method = "cv", number = 5 ,
                                              savePredictions = TRUE ) )

print ( summary (modelo))

# Evaluar el modelo
cat (" Evaluación del modelo basada en validación cruzada :\n")
matriz <- confusionMatrix ( modelo $ pred $ pred , modelo $ pred $ obs )
print ( matriz )
# Analizando la matriz de confusion ajustada se puede ver que el modelo tiene una
# exactitud del 86.25%, tiene una sensibilidad de un 85% y la especificidad de 87.5%.
# Este resultado nos indica que modelo se desempeña un poco mejor en identificar
# los elementos de la clase negativa o a su vez determinar cuán exacta es la
# asignación de elementos a la clase positiva(no sobrepeso). A su vez, al realizar
# el ajuste se puede observar que la exactitud ha aumentado más de un 5%.

#8)Usando código estándar1, evaluar el poder predictivo de los modelos con 
#los datos de las 40 personas que no se incluyeron en su construcción en 
#términos de sensibilidad y especificidad.

#Para verificar el poder predictivo se utilizara el metodo visto en clases.
# Ajustar modelo nulo .
nulo <- glm( EN ~ 1 , family = binomial ( link = "logit") , data = conjuntoVariables )

# Ajustar modelo completo .
cat ("\n\n")
completo <- glm(EN ~ . , family = binomial ( link = "logit") ,
                     data = conjuntoVariables )

# Ajustar modelo con regresión escalonada .
cat (" Modelo con regresión escalonada \n")
mejor <- step (nulo , scope = list ( lower = nulo , upper = completo ) ,
                   direction = "both", trace = 0)

print (summary (mejor))

# Verificación de multicolinealidad .
cat (" Verificación de colinealidad \n")
cat ("\ nVIF :\n")
vifs <- vif (mejor)
print (vifs)
# Al analizar el VIF de las pruebas seleccionadas, se debe escoger el menor
# que en este caso calza con el predictor seleccionado al inicio, que corresponde
# a Weight o peso.




