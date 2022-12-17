#Prueba 3 FORMA 2
#Dyllan Salgado
#Rut: 20227250-9
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
  require(leaps)
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
setwd("C:\\Users\\Dyllan\\Desktop\\PruebaIME")
datos <- read.csv2("PEP3 Datos.csv",encoding = "UTF-8")

#ENUNCIADO:

# El gobierno estadounidense se encuentra trabajando en un proyecto para frenar 
# la delincuencia. En este contexto, se encuentra estudiando la relación entre 
# la cantidad de delitos violentos en diferentes zonas del país y diversos datos 
# demográficos, a partir de un conjunto de datos con las siguientes variables.

# Usando la semilla 2238, obtenga una muestra de 200 observaciones y descarte 
# la variable Violentos. A partir de esta muestra, construya un modelo de 
# regresión logística multivariada para predecir la Alerta con entre 2 
# y 8 predictores y una exactitud superior a 80%. Considere 70% de las 
# instancias para entrenamiento y el 30%  restante para probar el modelo. 
# Verifique que el modelo no considera casos con demasiada influencia (usando la 
# distancia de Cook) y que no presenta multicolinealidad severa 
# (usando el factor de inflación de la varianza). 
# Concluya acerca de la calidad del modelo obtenido


# Se fija la semilla solicitada
set.seed(2238)

#obtenga una muestra de 200 observaciones y descarte la variable Violentos

#Se selecciona una muestra aleatoria de 200 observaciones.
muestra_200 <- sample_n(datos, 200)
#Se descarta la variable violentos
muestra_200_sinViolentos <- select(muestra_200,!Violentos)

# Construya un modelo de regresión logística multivariada para predecir la 
# Alerta con entre 2 y 8 predictores y una exactitud superior a 80%.

# Para seleccionar el mejor predictor se utiliza el método de todos los subconjuntos,
# ya que el capítulo nos dice explícitamente que este método es una alternativa 
# mucho más exhaustiva.
# Se pasa a factor la Alerta para que pueda ser usada en la función de 
# subconjuntos.
muestra_200_sinViolentos$Alerta <- factor(muestra_200_sinViolentos$Alerta)
#Entrenamiento tiene un 70% de los datos, que equivalena  140 obseravaciónes de 200.
entrenamiento <- muestra_200_sinViolentos[1:140,]
#Prueba tiene un 30% de los datos, que equivalen a  60 obseravaciónes de 200.
prueba <- muestra_200_sinViolentos[141:200,]

modelos <- regsubsets ( Alerta ~ . , data = entrenamiento , method = "exhaustive",
                        nbest = 1 , nvmax = 8)

print ( plot ( modelos ) )

#Al analizar el bic y el gráfico se puede observar que los tres datos predictores
# que deben ser seleccionados son: Urbano, Inmig y Pob, por el motivo que tienen la 
# mayor cantidad de cuadros cubiertos y su bic es bastante bajo.

#Una vez seleccionados o vistos los predictores mejores, se procede a ajustar 
# este modelo con los predictores escogidos

modelo_logistica_multi <- glm(Alerta ~ Inmig + Urbano + Pob, data = entrenamiento, family = binomial(link = "logit"))
# Se muestran por consola el resultado obtenido del modelo.
print(summary(modelo_logistica_multi))

#Luego se debe verificar una condición, la cual consiste en la multicolinealidad 
# entre los predictores, y para realizar esta acción se utiliza el VIF.
VIF <- vif(modelo_logistica_multi)
print(VIF)
# Según el capítulo muchos de los autores usan el valor VIF ≥ 10 como umbral 
# para preocuparse, aunque hay autores que consideran críticos valores más 
# conservadores, de 5 o incluso de 2,5. En este caso, Urbano, Inmig y Pob no 
# superan el valor 2

# Nos dice que el poder de predicción debe ser superior a un 80%, por lo que se 
# debe calcular y analizar.

# Se evalua la predictividad
# Se establece un umbral
umbral <- 0.5
probabilidad <- predict(modelo_logistica_multi, prueba, 
                        type = "response")
prediccion <- sapply(probabilidad,
                       function (p) ifelse (p >= umbral, "Bajo", "Alto"))
prediccion <- factor(prediccion, levels = levels(prueba[["Alerta"]]))
print(confusionMatrix(prediccion, prueba[["Alerta"]]))

#Se puede ver que este modelo tiene un poder de exactitud de un 85%, 
# una sensibilidad de un 84% y una especificidad de 85%. Este resultado 
# nos indica que el modelo se desempeña un poco mejor en identificar
# los elementos de la clase negativa o a su vez determinar cuán exacta es la
# asignación de elementos a la clase positiva(Baja).

# Para analizar que el modelo no considere casos con demasiada influencia, se
# utiliza la distancia de cook.

# Obtener residuos y estadísticas de influencia de los casos.
eval.modelo_logistica_multi <- data.frame(predicted.probabilities = fitted(modelo_logistica_multi))
eval.modelo_logistica_multi[["standardized.residuals"]] <- rstandard(modelo_logistica_multi)
eval.modelo_logistica_multi[["studentized.residuals"]] <-rstudent(modelo_logistica_multi)
eval.modelo_logistica_multi[["cooks.distance"]] <- cooks.distance(modelo_logistica_multi)
eval.modelo_logistica_multi[["dfbeta"]] <- dfbeta(modelo_logistica_multi)
eval.modelo_logistica_multi[["dffit"]] <- dffits(modelo_logistica_multi)
eval.modelo_logistica_multi[["leverage"]] <- hatvalues(modelo_logistica_multi)
eval.modelo_logistica_multi[["covariance.ratios"]] <- covratio(modelo_logistica_multi)

# Observaciones con distancia de Cook que sean altos
sospechosos <- which(eval.modelo_logistica_multi[["cooks.distance"]] > 1)
print(sospechosos)

#Se puede ver que no existen observaciones con una distancia alta.

#Conclusiones generales del resultado y ejercicio:
# El modelo creado se puede observar que si predice el nivel de alerta delictual
# de la zona, ya que nos arroja como resultado en el momento de generar la matriz 
# de confusión que existen más alertas altas que bajas. Asimismo, se puede
# confiar en este modelo, porque tiene un % de exactitud superior al 80%, y su 
# especificidad y sensibilidad también son superiores al 80%. 

#Ahora se puede generalizar el modelo utilizando la validación cruzada
# de 5 pliegues de la siguiente manera:

modeloGeneralizado <- train ( Alerta ~ Inmig + Urbano + Pob , data = entrenamiento , method = "glm",
                  family = binomial( link = "logit") ,
                  trControl = trainControl( method = "cv", number = 5 ,savePredictions = TRUE ) )
print ( summary(modeloGeneralizado) )

# Se procede a evaluar el modelo generalizado 
matriz <- confusionMatrix( modeloGeneralizado$pred$pred , modeloGeneralizado$pred$obs)
print (matriz)

# Al analizar el resultado, se puede observar que la exactitud ha aumentado a un
# 92% lo que es bastante, permitiendo confiar en nuestro modelo, asimismo la
# sensibilidad y especifidad también aumentaron a un 91% y 92% respectivamente, 
# lo cual indica que el modelo se desempeña un poco mejor en identificar
# los elementos de la clase negativa o a su vez determinar cuán exacta es la
# asignación de elementos a la clase positiva(Baja).



#PREGUNTA 2
# Proponga un ejemplo novedoso (no mencionado en clase ni que aparezca 
# en las lecturas dadas) de un problema de regresión lineal simple que podría 
# aparecer en relación con la próxima celebración de la Navidad.

#RESPUESTA: 

#En una escuela de la ciudad de Lunargenta ubicada en Kalimdor desean saber 
# si la edad y las medidas corporales de los niños(as) influyen en la cantidad de regalos
# que el viejito pascuero les deja en su hogar, en donde se tienen características 
# como su edad, la estatura, el peso y el género.

# Las variables predictoras pueden ser las características y la de respuesta
# son la cantidad de regalos que reciben los niños(as). Encuentro que si existe 
# una relación entre la variable de respuesta y sus predictores, ya que es sabido
# que los niños(as) que representan menor edad en estatura o tienen menor edad
# reciben más regalos para navidad (vi un reportaje que decía que aprox. reciben 
# 5 regalos o más), entonces sería curioso realizar el estudio y ver si 
# las características antes descritas afectan en la cantidad de regalos que reciben
# los niños(as) para navidad.

#PREGUNTA 3
#Explique, en sus palabras, en qué consiste la validación cruzada de k pliegues 
#y en qué se diferencia de la validación cruzada tradicional.

#RESPUESTA:
# La validación cruzada es una técnica utilizada para realizar un remuestreo, 
# de tal manera que se mejore la estimación del error cuadrático medio. Esta validación
# divide el conjunto de datos en k subconjuntos con igual tamaño. La diferencia
# que tiene la validación cruzada de k-pliegues con la validación cruzada tradicional
# radica en que la de k-pliegues divide en k subconjuntos la muestra para obtener
# k estimaciones de errores, a diferencia de la validación tradicional la cual
# separa en un conjunto de entrenamiento y otro de prueba para ser evaluado.
# de la tradicional 


#PREGUNTA 4
#Explique, en sus palabras, qué son las muestras probabilísticas por racimos

#RESPUESTA:

# Para responder esta pregunta primero se debe saber que son las muestras 
# probabilísticas, en palabras sencillas es un grupo de observaciones, las cuales 
# pueden ser personas, objetos, etc. que representan una población, la cual es utilizada
# para inferir acerca de ella. Ahora bien, las muestras probabilísticas por racimos
# corresponden a una muestra de la población seleccionada al azar que ya se 
# encuentran agrupadas, un ejemplo sencillo pueden ser realizar una muestra por racimos
# a estudiantes, donde ya existen grupos de estudiantes, como lo pueden ser
# universitarios, estudiantes de enseñanza media, estudiantes de enseñanza básica, etc.