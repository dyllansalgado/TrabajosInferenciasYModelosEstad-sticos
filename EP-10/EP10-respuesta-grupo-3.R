
#ENUNCIADO
#------------------------------------------------------------------------------
#Ñami-Ñam, compañía dedicada a la elaboración y comercialización de golosinas, 
#se prepara para lanzar una nueva línea de productos al mercado. Para asegurar 
#el éxito comercial, ha solicitado a varias empresas de diseño la creación de 
#un empaque para cada uno de los nuevos productos. A fin de decidir qué envase 
#es mejor para cada producto y evaluar un contrato permanente con una de las 
#empresas de diseño, Ñami-Ñam ha reclutado a 2.000 voluntarios de todo el país,
#seleccionados aleatoriamente entre los participantes de un concurso efectuado 
#por Ñami-Ñam el año anterior. Cada participante debe puntuar las distintas 
#alternativas de envase para un producto (seleccionado al azar) mediante una 
#escala Likert de 7 puntos, donde: 1: el envase es muy poco atractivo y 7: el 
#envase es muy atractivo. Los datos recolectados contemplan las siguientes 
#variables:

#▪ Id: identificador único de cada participante.
#▪ Edad: rango etario del participante. Variable categórica con los niveles 
#  Niño, Joven, Adulto.
#▪ Producto: producto para el cual se evalúan los empaques. Variable categórica 
#  con los niveles Alfajor, Caramelos, Chocolate, Cuchuflí, Galletas, Queque.
#▪ Diseno: empresa que diseñó el envase. Variable categórica con los niveles 
#  DisenoColor, KoolDesign, LaKajita,PackPro.
#▪ Puntaje: puntuación obtenida por el envase. Entero [1-7].

# Importar los paquetes, instalándolos de ser necesario.
if (!require(tidyverse)) {
  install.packages("tidyverse", dependencies = TRUE)
  require(tidyverse) }

# Se setea el workspace
setwd("C:\\Users\\Dyllan\\Desktop\\TrabajosIME\\EP-10")
# Se lee el archivo con los datos
datos <- read.csv2("EP10 Datos.csv",encoding = "UTF-8")

#Se establece un nivel de significación para las 2 preguntas.

alfa <- 0.05

#1. ¿Existe diferencia en la puntuación obtenida por los envases diseñados por
#   LaKajita según las evaluaciones realizadas por niños y jóvenes?

# Hipótesis:

#H0: No hay diferencia en la puntuación obtenida por los envases diseñados por
#    LaKajita según las evaluaciones realizadas por niños y jóvenes.

#HA: Si hay diferencia en la puntuación obtenida por los envases diseñados por
#    LaKajita según las evaluaciones realizadas por niños y jóvenes.

# Se obtienen los envases diseñados por LaKajita solicitado por el enunciado.
diseñoLaKajita <- datos %>% filter (Diseno=="LaKajita")
# Ahora se obtienen los datos de niño y joven que evaluaron los envases
# de LaKajita
ninosLaKajita <- diseñoLaKajita %>% filter (Edad=="Nino")
jovenesLaKajita <- diseñoLaKajita %>% filter (Edad=="Joven")

# Para responder a la pregunta 1 que se debe utilizar una prueba 
# T de student, ya que el enunciado nos solicita inferir acerca de dos medias
# muestrales, para este caso se deben verificar 2 puntos:

#1. Las observaciones son independientes entre sí.
# Este punto si se verifica, ya que el enunciado nos dice que los participantes
# han sido seleccionados de forma aleatoria y además cada participante cuenta
# con un ID único.

#2. Las observaciones provienen de una distribución cercana a la normal.
# Para comprobar este punto se va a usar la prueba de Shapiro-wilk, por lo que
# el puntaje se debe pasar a vector para el uso de esta prueba.
ninosLaKajitaVector <- as.vector(ninosLaKajita$Puntaje)
jovenesLaKajitaVector <- as.vector(jovenesLaKajita$Puntaje)

# Ahora para comprobar la normalidad aplicamos la prueba de 
# Shapiro-Wilk.
normalidad_ninos <- shapiro.test(ninosLaKajitaVector)
normalidad_jovenes <- shapiro.test(jovenesLaKajitaVector)
cat("\n> Comprobación de la normalidad de los datos:\n")
print(normalidad_ninos)
print(normalidad_jovenes)

# Al obtener los resultados de shapiro-Wilk de los niños y jovenes,
# dan un p-value de 7.894e-16(ninos) y 1.204e-15(jovenes), se obtiene
# que estos valores son mucho menores a nuestro nivel de significación de 0.05
# p-value < alfa, por lo que se puede concluir con un 95% de confianza que los 
# datos no siguen una distribución normal.

# Como los datos no siguen una distribución normal, se puede asumir que no 
# se puede utilizar la prueba T de student, por lo que se va a utilizar 
# la prueba de suma de rangos de Wilcoxon.

# Para el uso de Wilcoxon se deben verificar 2 puntos importantes:

# 1. Las observaciones de ambas muestras son independientes.
# Este punto si se verifica, ya que el enunciado nos dice que los participantes
# han sido seleccionados de forma aleatoria y además cada participante cuenta
# con un ID único.

# 2. La escala de medición empleada debe ser a lo menos ordinal, de modo que 
# tenga sentido hablar de relaciones de orden (“igual que”, “menor que”, 
# “mayor o igual que”).
# Este punto también se verifica, ya que la pregunta nos solicita verificar
# si existe diferencia entre la puntuación o calificación obtenidas. Por lo que
# se debe utilizar la información de Puntaje que en este caso si tiene una
# escala ordinal, porque permite establecer una relación de orden que va de 
# 1 a 7, donde 1 es envase muy poco atractivo y 7 el envase es muy atractivo.

#Una vez pasados los datos a vector se procede a realizar la función
# de wilcox.test.
wilcoxon <- wilcox.test(ninosLaKajitaVector, 
                        jovenesLaKajitaVector,
                        paired = FALSE,
                        alternative = "two.sided", 
                        conf.level = 1 - alfa)
print(wilcoxon)

#Se puede apreciar que el P-value obtenido con Wilcoxon corresponde a 
# 0.51, lo cual es mucho mayor al nivel de significación establecido de 0.05,
# por lo que p-value > alfa, por ende se falla en rechazar la hipótesis nula 
# en favor de la hipótesis alternativa, por lo que se concluye con un 95% de 
# confianza que no hay diferencia en la puntuación obtenida por los 
# envases diseñados por LaKajita según las evaluaciones realizadas por niños y 
# jóvenes.


#2. ¿Existen diferencias entre las puntuaciones obtenidas para los diferentes 
#   envases de chocolate? De ser así, ¿cuál(es) envase(s) se diferencia(n) de 
#   los demás?

# Hipótesis:
# H0: Las puntuaciones obtenidas para los diferentes envases de chocolate
#     son similares.
# HA: Al menos una de las puntuaciones obtenidas para los diferentes envases de
#     chocolate es distinta de las demás.

# Justificación de que prueba se debe utilizar:
# Para verificar si existen diferencias entre los distintos envases de chocolate, 
# es posible utilizar la prueba de Friedman como versión no paramétrica de la 
# prueba ANOVA,lo que nos permitiría conocer si existen diferencias o no entre 
# los distintos grupos de observaciones


# Verificar condiciones de la prueba
# Para realizar el procedimiento de Friedman se deben cumplir las siguientes 
# condiciones:
#1.La variable independiente debe ser categórica y tener a lo menos tres niveles.
#   En nuestro caso sí se cumple esta condición, ya que nuestra variable es 
#   explícitamente independiente (del enunciado), y en este caso diseño si es una 
#   variable categórica, el cual posee 4 niveles, que corresponden a las 4 
#   empresas distintas encargadas de elaborar los diseños de los envases 
#   (DisenoColor, KoolDesign, LaKajita,PackPro.)

#2. La escala de la variable dependiente debe ser, a lo menos, ordinal.
# Este punto también se verifica, ya que la pregunta nos solicita verificar
# si existe diferencia entre la puntuación o calificación obtenidas. Por lo que
# se debe utilizar la información de Puntaje que en este caso si tiene una
# escala ordinal, porque permite establecer una relación de orden que va de 
# 1 a 7, donde 1 es envase muy poco atractivo y 7 el envase es muy atractivo.

#3. Los sujetos son una muestra aleatoria e independiente de la población

# Este punto si se verifica, ya que el enunciado nos dice que los participantes
# han sido seleccionados de forma aleatoria y además cada participante cuenta
# con un ID único.

# Se obtienen los datos del producto cholocate solicitado por el enunciado.
datosChocolate <- datos %>% filter (Producto=="Chocolate")
# Hacer la prueba de Friedman .
prueba <- friedman.test ( Puntaje ~ Diseno | Id , data = datosChocolate)
print (prueba)

# Como el P-value (0.4008) es mayor a nuestro nivel de significación(0.05) se 
# falla en rechazar la hipótesis nula a favor de la hipótesis alternativa, por 
# lo que se puede concluir con un 95% de confianza que las puntuaciones para los 
# diferentes envases de chocolate son similares entre ellas. En este caso, como 
# nuestro p-value obtenido en la prueba es mayor al nivel de significación, no 
# es necesario realizar un procedimiento post-hoc, ya que no hay diferencias 
# significativas entre los pares de grupos.

