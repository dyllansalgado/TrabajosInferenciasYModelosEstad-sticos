

############################################
# Se setea el workspace (Carpeta donde se encuentran los archivos)
# (Asegurarse de cambiar la ruta de la carpeta antes de ejecutar el script, y que el 
# archivo csv esté en la carpeta).
dir <- setwd("C:\\Users\\Dyllan\\Desktop\\TrabajosIME\\EP-03")
############################################

# Importar los paquetes, instalándolos de ser necesario.
if (!require(tidyverse)) {
  install.packages("tidyverse", dependencies = TRUE)
  require(tidyverse) }

if (!require(ggpubr)) {
  install.packages("ggpubr", dependencies = TRUE)
  require(ggpubr) }

#Se lee el archivo csv en formato español. Se usa csv2 para su lectura.
población <- read.csv2("EP03 Datos Casen 2017.csv", stringsAsFactors=FALSE)
#Devuelve el número de filas presentes en la poblacion.
tamaño <- nrow(población)
#Se pasa las variables de ytot a numerico.
ingreso <- as.numeric(población[["ytot"]])
#Se asigna a poda un valor decimal de 0.2
poda <- 0.2
#Se busca el quantil 20 de ingreso.
q20 <- quantile(ingreso, poda)
#Para buscar el quantil 80 se debe restar 1 - 0.2
q80 <- quantile(ingreso, 1 - poda)
#Ingreso tiene que ser mayor a 113281 y menor a 750000.
ingreso.podado <- ingreso[ingreso > q20 & ingreso < q80]
#Busco el tamaño de la nueva lista filtrada.
tamaño.podado <- length(ingreso.podado)
#Se calcula la media de los ingresos podado.
media.ingreso <- mean(ingreso.podado)
#Se calcula la desviación estándar.
sd.ingreso <- sqrt (sum((ingreso.podado - media.ingreso)^2) / tamaño.podado )

#Se fija una semilla definida por el equipo de trabajo
set.seed(288)
ingreso.normal <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)
#Los datos se pasan a un data frame y se realiza un gráfico de histograma para 
#mostrar la distribución
ingreso.normal.dataFrame <-data.frame(ingreso.normal)

#Gráfico 1
grafico1 <- gghistogram(data = ingreso.normal.dataFrame, x = "ingreso.normal", 
                        bins = 40,
                        xlab = "Ingresos normales",
                        ylab = "Muestra",
                        title = "Ingresos normales de la muestra de 5000 personas",
                        color = "blue",
                        fill = "red")
print(grafico1)

#Distribucion Z
# Como se está trabajando con una muestra de 5000 personas, se calcula nuevamente 
# la media pero de la muestra.
media.ingreso.normal <- mean(ingreso.normal)
# Luego se calcula la desviación estándar de la muestra
sd.ingreso.normal <- sd(ingreso.normal)
# Ahora que se tiene la media y la muestra, se procede a calcular la distribución Z.
distribucionZ <- (ingreso.normal - media.ingreso.normal)/sd.ingreso.normal
# Se pasa a un data frame la distribución Z obtenida para realizar el gráfico de 
# distribución.
distribucionZ.dataFrame <- data.frame(distribucionZ)

#Gráfico 2
grafico2 <- gghistogram(data = distribucionZ.dataFrame, x = "distribucionZ", 
                        bins = 40,
                        xlab = "Ingresos con distribución Z",
                        ylab = "Muestra",
                        title = "Ingresos con distribución Z de la muestra de 5000 personas",
                        color = "blue",
                        fill = "black")
print(grafico2)

# Dos distribuciones chi cuadrado con más de 3 grados y menos de 15 de libertad.
gradoLibertad1 <- 4
gradoLibertad2 <- 12
#Se busca el largo de la distribución Z para realizar el ciclo.
largoDistribucionZ <- length(distribucionZ)
#Se crea un objeto de tipo numérico de longitud largoDistribucionZ, para que sea #utilizado en el ciclo while.
# Creamos los vectores
chicuadrado1<-numeric(largoDistribucionZ)
chicuadrado2 <-numeric(largoDistribucionZ)
i <- 1 
while (i <= largoDistribucionZ ){
  # Se toma una muestra de distribucionZ.gradolibertad variables
  chi_cuadrado1_gradoL <- (sample(distribucionZ,gradoLibertad1))^2
  chi_cuadrado2_gradoL <- (sample(distribucionZ,gradoLibertad2))^2
  # Se hace una suma de dicha muestra
  sumachi_cuadrado1 <- sum(chi_cuadrado1_gradoL)
  sumachi_cuadrado2 <- sum(chi_cuadrado2_gradoL)
  # Se almacena dicha suma en la correspondiente posición
  chicuadrado1[i] <- sumachi_cuadrado1
  chicuadrado2[i] <- sumachi_cuadrado2
  i <- i+1
}
# Se pasa a un data frame las 2 distribuciones de chi cuadrado obtenida para realizar el # gráfico de distribución.
chicuadrado1.dataFrame <- data.frame(chicuadrado1)
chicuadrado2.dataFrame <- data.frame(chicuadrado2)

#Gráficos 3.1 chicuadrado1 
grafico3.1 <- gghistogram(data = chicuadrado1.dataFrame, x = "chicuadrado1", 
                          bins = 40,
                          xlab = "Ingresos con distribución chi-cuadrado",
                          ylab = "Muestra",
                          title = "Ingresos chi-cuadrado con 4 grados de libertad",
                          color = "blue",
                          fill = "red")
print(grafico3.1)
#Gráficos 3.2 chicuadrado2 
grafico3.2 <- gghistogram(data = chicuadrado2.dataFrame, x = "chicuadrado2", 
                          bins = 40,
                          xlab = "Ingresos con distribución chi-cuadrado",
                          ylab = "Muestra",
                          title = "Ingresos chi-cuadrado con 12 grados de libertad",
                          color = "red",
                          fill = "blue")
print(grafico3.2)

#Distribución F:
# La distribucion F corresponde a la división de ambos chi cuadrados y dichos chi
# cuadrados divididos por su respectivo grado de libertad.
distribucionF <- (chicuadrado1/gradoLibertad1)/(chicuadrado2/gradoLibertad2)

# Se pasa a un data frame la distribución F para realizar el gráfico de distribución.
distribucionF.dataframe <-data.frame(distribucionF)

#Gráfico 4
# Respecto a este gráfico, sucede que en el visualizador de la preview puede
# que no se vea a primera, pero si cambia el tamaño de la ventana de preview debería
# poder visualizarse
grafico4 <- gghistogram(data = distribucionF.dataframe, x = "distribucionF", 
                        bins = 40,
                        xlab = "Ingresos con distribución F",
                        ylab = "Muestra",
                        title = "Ingresos con distribución F de la muestra de 5000 personas",
                        color = "blue",
                        fill = "red")
print(grafico4)

################################################################################
#                                 PARTE 2                                      #
################################################################################
#Se genera una nueva semilla establecida por el equipo de trabajo.
set.seed(288)
#El número de repeticiones también fue establecido por el equipo.
n.repeticiones <- 25
ensayo <- function(x)
  ifelse(sample(población[["sexo"]], 1) == "Mujer", 1, 0)
repeticiones <- sapply(1:n.repeticiones, ensayo)

#Calcular distribución binomial.
#Primero se contarán la cantidad de éxito, donde si aparece mujer(éxito) es 1 y si no 0.
cantidadExitos <- 0
for (o in 1:n.repeticiones){
  if(repeticiones[o] == 1 ){
    cantidadExitos <- cantidadExitos+1 
  }
}
#Ahora que tenemos la cantidad de áxitos debemos buscar p y q,
#donde P es la probabilidad de éxito y Q = 1-p es la probabilidad de no éxito.

p <- cantidadExitos/n.repeticiones
q <- 1-p

#Para calcular la distribución binomial debemos realizar una sumatoria,
#donde a itera hasta n.repeticiones.
resultadoBinomial <- numeric(n.repeticiones)
a <- 0
while (a <= n.repeticiones){
  #Se utiliza la fórmula binomial.
  #La función choose hace la combinatoria (n sobre a).
  calculoBinomial<- (choose(n.repeticiones,a))*(p^a*((q)^(n.repeticiones-a)))
  resultadoBinomial[a] <- calculoBinomial
  a = a+1
}
#Se pasa a un dataframe el resultado binomial para una mejor visualización.
resultadoBinomial.dataframe <- data.frame(resultadoBinomial)

#Gráfico 5
plot(resultadoBinomial,main="Distribución Binomial",xlab= "Repeticiones",ylab = "Probabilidad_éxito" ,type='o',col = 'pink',lwd = 2)

#Calcular distribución geométrica.
resultadoGeometrica<- numeric(n.repeticiones)
a <- 0
while (a <= n.repeticiones){
  #Se utiliza la fórmula geométrica.
  calculoGeometrica <- ((q)^(a-1))*p
  resultadoGeometrica[a] <- calculoGeometrica
  a = a+1
} 
#Se pasa a un dataframe el resultado geométrica para una mejor visualización.
resultadoGeometrica.dataframe <- data.frame(resultadoGeometrica)

#Gráfico 6
plot(resultadoGeometrica,main="Distribución Geométrica",xlab= "Repeticiones",ylab = "Probabilidad_éxito" ,type='o',col = 'red',lwd = 2)

#Calcular distribución binomial negativa.
resultadoBinomialNegativa <- numeric(n.repeticiones)
a <- 0
while (a <= n.repeticiones){
  #Se utiliza la fórmula binomial negativa.
  #La función choose hace la combinatoria (n-1 sobre a-1).
  calculoBinomialNegativa<- (choose(n.repeticiones-1,a-1))*(p^a*((q)^(n.repeticiones-a)))
  resultadoBinomialNegativa[a] <- calculoBinomialNegativa
  a = a+1
} 
#Se pasa a un dataframe el resultado binomial negativo para una mejor visualización.
resultadoBinomialNegativa.dataframe <- data.frame(resultadoBinomialNegativa)

#Gráfico 7
plot(resultadoBinomialNegativa,main="Distribución Binomial Negativa",xlab= "Repeticiones",ylab = "Probabilidad_éxito" ,type='o',col = 'blue',lwd = 2)