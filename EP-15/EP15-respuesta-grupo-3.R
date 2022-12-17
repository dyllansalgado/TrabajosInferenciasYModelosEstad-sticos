
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

#1.Definir la semilla a utilizar, que corresponde a los primeros cinco dígitos 
#del RUN del integrante de mayor edad del equipo.

#Se establece una semilla con los últimos 5 dígitos finales del RUN del 
# integrante de mayor edad del grupo.
set.seed(72186)
alfa <- 0.05

#2. Seleccionar una muestra de 100 personas, asegurando que la mitad tenga 
#estado nutricional “sobrepeso” y la otra mitad “no sobrepeso”. 

#Se selecciona la columna de Estatura que se encuentra en cm y se debe pasar a 
# metros.
estaturaMetros <- select(datos,Height)/100
Peso <- select(datos,Weight)
#Ahora que se tiene la estatura en metros se puede calcular el IMC.
IMC <- Peso/(estaturaMetros**2)

datos <- data.frame(datos,IMC)
#Se añade al dataframe la variable nueva llamada IMC
datos <- rename(datos, c(Weight.1="IMC"))
#Viendo la literatura ifelse(vector, valor_si_TRUE, valor_si_FALSE)
# ref: https://bookdown.org/jboscomendoza/r-principiantes4/if-else.html
datos$EN <- ifelse(datos$IMC >= 25.0, "sobrepeso","no sobrepeso")
datos$EN <- factor(datos$EN)
# seleccionan 100 personas donde 50 deben tener sobrepeso y 50 no sobre peso.
sobrePeso <- sample_n(datos %>% filter(EN == "sobrepeso"),50,replace=TRUE)
noSobrePeso <- sample_n(datos %>% filter(EN == "no sobrepeso"),50,replace=FALSE)
#Se dejan en el dataframe
datos<- rbind(sobrePeso,noSobrePeso)

#3.Usando las herramientas del paquete leaps, realizar una búsqueda exhaustiva 
#para seleccionar entre dos y ocho predictores que ayuden a estimar la variable 
#Peso (Weight), obviamente sin considerar las nuevas variables IMC ni EN, y 
#luego utilizar las funciones del paquete caret para construir un modelo de 
#regresiónlineal múltiple con los predictores escogidos y evaluarlo usando 
#bootstrapping.

#Se debe realizar la búsqueda exhaustiva sin las variables IMC ni EN.

columnasSinVariables <- datos %>% select(!c("IMC","EN"))

# Ajustar modelo con todos los subconjuntos, ya que el libro nos dice explícitamente
# que este método es una alternativa mucho más exhaustiva.
modelos <- regsubsets ( Weight ~ . , data = columnasSinVariables , method = "exhaustive",
                           nbest = 1 , nvmax = 8)

print ( plot ( modelos ) )

#Al analizar el bic y el gráfico se puede observar que los dos datos predictores
# mejores pueden ser Waist.Girth y Thigh.Girth.

#Al igual que en el EP-14 se deja un dataframe solo con los predictores de interés.
predictores_escogidos <- columnasSinVariables %>% select(Waist.Girth,Thigh.Girth,Weight)

#Una vez al tener los predictores elegidos se procede a realizar bootstrapping.
# según en este página:
#http://www.sthda.com/english/articles/38-regression-model-validation/156-bootstrap-resampling-essentials-in-r/ 
# en el apartado de "Evaluating a predictive model performance" para aplicar
# bootstrapping a un modelo predictivo se puede utilizar la función train de la 
# siguiente manera:

# Se define train control, el cual indica el metodo de bootstrap con 1999 datos de
# remuestreo.
train.control <- trainControl(method = "boot", number = 1999)
# Se aplica a la función train el metodo anterior
bootstrap <- train(Weight ~., data = predictores_escogidos, method = "lm",
               trControl = train.control)
# Se imprimen los resultados.
print(bootstrap)

#El R-cuadrado representa la proporción de variación en el resultado explicada 
#por las variables predictoras incluidas en el modelo. Cuanto mayor sea el 
#R-cuadrado, mejor será el modelo.
#Se puede observar que el R-Cuadrado es 0.859 lo cual está muy cercano a 1,
# por lo que se puede asumir que el modelo es aceptable.


#4)Haciendo un poco de investigación sobre el paquete caret, en particular 
#cómo hacer Recursive Feature Elimination (RFE), construir un modelo de 
#regresión lineal múltiple para predecir la variable IMC que incluya
#entre 10 y 20 predictores, seleccionando el conjunto de variables que maximice 
#R2 y que use cinco repeticiones de validación cruzada de cinco pliegues para 
#evitar el sobreajuste (obviamente no se debe considerar las variables Peso, 
#Estatura ni estado nutricional –Weight, Height, EN respectivamente).