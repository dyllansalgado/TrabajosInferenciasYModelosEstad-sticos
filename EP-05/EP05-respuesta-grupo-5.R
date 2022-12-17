

# Importar los paquetes, instalándolos de ser necesario.
if (!require(tidyverse)) {
  install.packages("tidyverse", dependencies = TRUE)
  require(tidyverse) }

if (!require(ggpubr)) {
  install.packages("ggpubr", dependencies = TRUE)
  require(ggpubr) }

#ENUNCIADO.
# Se sabe que una máquina que envasa detergentes industriales llena bidones con un volumen de producto que
# sigue una distribución normal con desviación estándar de 1 litro. Usando una muestra aleatoria de 100 botellas,
# el ingeniero a cargo de la planta requiere determinar si la máquina está llenando los bidones con una media de
# 10 litros.

#Datos entregados:

desviacion_estandar <- 1
tamaño <- 100
media <- 10
media_superior <- 10.5
# Calcular el error estándar .

SE <- desviacion_estandar/sqrt(tamaño)

#PREGUNTA 1
# Si el ingeniero está seguro de que el verdadero volumen medio no puede ser inferior a 10 litros y piensa
# rechazar la hipótesis nula cuando la muestra presente una media mayor a 10,5 litros, ¿cuál es la
# probabilidad de que cometa un error de tipo I?

# Hipotesis:
# H0: El volumen medio de los bidones de detergentes es de 10 [Litros]
# media = 10 [L]
# HA: El volumen medio de los bidones de detergente tiene una media mayor a 10,5 [Litros]
# media > 10,5 [L]

# Se genera distribución normal
x <- seq ( media-4 * SE , media+10 * SE , 0.1)
y <- dnorm (x , mean = media , sd = SE )

data <- data.frame(x,y)
g <- ggplot ( data,aes(x))
g <- g + stat_function (
  fun = dnorm ,
  args = list ( mean = media , sd = SE ) ,
  colour = "red ", size = 1)
g <- g + ylab ("")
g <- g + scale_y_continuous ( breaks = NULL )
g <- g + scale_x_continuous ( name = "Distribución de media bidones de detergente",
                                 breaks = seq (4 , 13  , 0.2) )
g <- g + theme_pubr ()

g <- g + geom_vline(xintercept = media,
                              colour = "blue", linetype = "longdash")

g <- g + geom_area ( data = subset (data ,x > media_superior ) ,
                           aes (y = y) ,
                           colour = "blue",
                           fill = "blue",
                          alpha = 1)

g <- g + ggtitle("Media de bidones y con area marcada ")
print(g)

# con pnorm se calcula el área correspondiente a una sola cola de la grafica,
# es por este motivo y como la hipotesis nos dice media mayor a 10.5, se calcula
# el area de la media superior, dando como resultado la probabilidad de obtener el error tipo I.
probabilidad <- pnorm(media_superior, mean = media,
                           sd = SE, lower.tail = FALSE)

cat("La probabilidad de cometer un error de tipo I es de:",probabilidad)

#RESPUESTA 1:
# La probabilidad de realizar un error tipo I se encuentra definido por la zona marcada en azul,
# y al realizar el calculo nos da que corresponde a 2.866516e-07 o 0.0000002866516. 










#PREGUNTA 4

#Considerando un volumen medio de 10 litros, ¿cuántos bidones deberían 
#revisarse para conseguir un poder estadístico de 0,8 y un nivel de
#significación de 0,05?

# Utilizando media de la pregunta 2, se procede a calcular la d de cohen 
media_pregunta2 <- 10.3
d_cohen_4 <- (media_pregunta2-media)/desviacion_estandar

poder_pregunta4 <- 0.8
nivel_significación_pregunta4 <- 0.05
# Cálculo del tamaño de la muestra usando la función power.t.test()
cat ("Cálculo del poder con power.t.test ()\n")
#Como se quiere calcular n o la cantidad de bidones para revisar,
#en la funcion de power.t.test se coloca nulo.
resultado <- power.t.test( n = NULL ,
                                 delta = d_cohen_4 ,
                                 sd = desviacion_estandar ,
                                 sig.level = nivel_significación_pregunta4 ,
                                 power = poder_pregunta4 ,
                                 type = "one.sample",
                                 alternative = "two.sided")
print (resultado)
#Ahora se tiene la cantidad de bidones para revisar que corresponde a n.
#Por lo que debemos extraerlo de la siguiente manera:
n <- ceiling(resultado [["n" ]])
cat ("Los bidones que deberian revisarse para conseguir un poder estadistico de 0,8 y un nivel de
significación de 0,05 es de = ", n , "\n")

#Respuesta:
#Los bidones que deberian revisarse para conseguir un poder estadistico de 0,8 y un nivel de
# significación de 0,05 es de 89.1495 o aprox 90 bidones.


#PREGUNTA 5:

#¿Y si el ingeniero fuese muy exigente y quisiera reducir la probabilidad de cometer un error de tipo I 
#a un 1% solamente?

# El nivel de significación permite controlar la probabilidad de cometer un error de tipo I,
# es por este motivo que para responder esta pregunta lo unico que se deberia realizar 
# es tener un nivel de significacion de 0,01 
# Como ya se tienen todos los valores en la pregunta 4, se procede a utilizar los mismos valores
# pero solo reemplazando el nivel de significación por el nuevo que es 0.01.

nivel_significacion_pregunta5 <- 0.01

# Cálculo del tamaño de la muestra usando la función power.t.test()
cat ("Cálculo del poder con power.t.test ()\n")
#Como se quiere calcular n o la cantidad de bidones para revisar,
#en la funcion de power.t.test se coloca nulo.
resultado <- power.t.test( n = NULL ,
                           delta = d_cohen_4 ,
                           sd = desviacion_estandar ,
                           sig.level = nivel_significacion_pregunta5 ,
                           power = poder_pregunta4 ,
                           type = "one.sample",
                           alternative = "two.sided")
print (resultado)
#Ahora se tiene la cantidad de bidones para revisar que corresponde a n.
#Por lo que debemos extraerlo de la siguiente manera:
n1 <- ceiling(resultado [["n" ]])
cat ("Si el ingeniero fuese muy exigente y quisiera reducir la probabilidad de cometer un error de tipo I 
a un 1%, la cantidad de bidones deberia ser de = ", n1 , "\n")
#Si el ingeniero fuese muy exigente y quisiera reducir la probabilidad de cometer un error de tipo I 
#a un 1%, la cantidad de bidones deberia ser de 134.