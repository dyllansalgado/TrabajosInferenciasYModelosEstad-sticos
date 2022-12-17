

#Se asigna un nivel de significación para todas las preguntas
alfa <- 0.05

# Comprobación de paquetes
if(!require(tidyverse)){
  install.packages("tidyverse",dependencies = TRUE)
  require(tidyverse)
}
if(!require(RVAideMemoire)){
  install.packages("RVAideMemoire",dependencies = TRUE)
  require(RVAideMemoire)
}
if(!require(rcompanion)){
  install.packages("rcompanion",dependencies = TRUE)
  require(rcompanion)
}

#### Pregunta 1 ####
#En su desquiciada investigación para acabar con los vampiros, 
#Van Helsing ha descubierto que sus enemigos tienen predilección 
#por la sangre humana tipo AII+. El cazador sospecha que estos monstruos 
#tienen preferencia por la sangre de los adultos, pero aún no está 
#convencido. Por esta razón, mediante artimañas, ha encerrado a 14 niños 
#y 20 adultos con este tipo de sangre en un reducto de vampiros. Tras 3 
#noches, 3 de los niños y 13 de los adultos fueron atacados. ¿Existe 
#relación entre el ataque de vampiros y la edad de la víctima?


#Datos obtenidos del enunciado:
#1. Se cree que prefiere sangre de adultos
#2. Tiene 14 niños y 20 adultos
#3. 3 niños fueron atacados y 13 adultos
#¿Existe relación entre edad y los ataques?

#         !Atacados ! No atacados! Total!
# ---------------------------------------
# Niños:  !   3     !    11      !  14  !
# Adultos:!   13    !     7      !  20  !
# ---------------------------------------
# Total   !   16    !    18      !  34  !

#La pregunta nos indica si existe relacion entre las variables, por lo 
#que el enunciado nos da una idea de que se debe utilizar chi-cuadrado 
#de independencia, ademas el enunciado tambien nos dice que los vampiros 
#tienen predilección por la sangre tipo AII+, por lo que podemos suponer 
#que los datos pertenecen a una misma población y fueron seleccionados 
#de forma aleatoria.

#Hipótesis:
#H0: Los ataques son independientes de la edad de las personas.
#HA: Los ataques se encuentran relacionados con la edad de las personas.

#Hipótesis matemática:
# Suponiendo que = es relacion y != es independencia matemáticamente quedaría:
# H0: Ataques = edad
# HA: Ataques != edad

#Se procede a crear la tabla
niños <- c(3 , 11)
adultos <- c(13 , 7)
totalFilasNiños <- (niños[1]+niños[2])
totalFilasAdultos <-(adultos[1]+adultos[2])
totalColumnaAtacados <-(niños[1]+adultos[1])
totalColumnaNoAtacados <-(niños[2]+adultos[2])
total<-(totalFilasNiños + totalFilasAdultos)

tabla <- as.table ( rbind( niños , adultos ) )
dimnames(tabla) <- list(edad = c("Niños","Adultos"),
                        estado = c("Atacados","No atacados"))
print(tabla)

#Análisis de prueba a utilizar:
#Se analiza si en cada grupo se tienen como mínimo 5 observaciones 
#esperadas, para ver si se puede utilizar chi-cuadrado de independencia
observacion1 <- (totalColumnaAtacados*totalFilasNiños)/total
observacion2 <- (totalColumnaAtacados*totalFilasAdultos)/total
observacion3 <- (totalColumnaNoAtacados*totalFilasNiños)/total
observacion4 <- (totalColumnaNoAtacados*totalFilasAdultos)/total
fila_1_Esperada <- c(observacion1,observacion3)
fila_2_Esperada <- c(observacion2,observacion4)
#Se crea la tabla con los datos esperados
tablaObservacioneEsperadas <- as.table ( rbind( fila_1_Esperada , fila_2_Esperada ) )
#Se le añaden los títulos a las variables
dimnames(tablaObservacioneEsperadas) <- list(edad = c("Niños","Adultos"),
                                             estado = c("Atacados","No atacados"))
print(tablaObservacioneEsperadas)
#Al tener la tabla de las frecuencias esperadas, se puede apreciar
#que todos los valores obtenidos superan las 5 observaciones, por lo que
#todas las condiciones se han cumplido para utilizar chi-cuadrado de 
#indepencia.

#Se aplica chi-cuadrado a la tabla inicial
prueba <- chisq.test(tabla)
#Se imprime el resultado
print(prueba)

#El valor p obtenido corresponde a 0.03, lo que es menor al nivel de 
#significación establecido de 0.05, por lo que se rechaza la hipótesis 
#nula en favor de la hipótesis alternativa. Se concluye con un 95% de 
#confianza que, los ataques si se encuentran relacionados con la edad de 
#las personas.


#################################  PREGUNTA 2 ##################################
# Una Universidad ha detectado que muchos de sus nuevos estudiantes ingresan con
# elevados niveles de ansiedad. Para ello, han decidido evaluar un nuevo programa
# de bienvenida que busca facilitar la adaptación a la vida universitaria. Para
# ello, han reclutado a un grupo de 15 voluntarios a quienes se les midió el
# nivel de ansiedad (alto o bajo) antes y después de participar en el programa
# de bienvenida:
#  ▪ 4 estudiantes no presentaron ansiedad ni antes ni después.
#  ▪ 5 estudiantes inicialmente ansiosos dejaron de estarlo.
#  ▪ 4 estudiantes mantuvieron un elevado nivel de ansiedad.
#  ▪ Los 2 estudiantes restantes desarrollaron síntomas de ansiedad tras
#    participar en el programa.

# ¿Qué se puede concluir acerca del nuevo programa de bienvenida?


# Podemos realizar la prueba de McNemar, que resulta apropiada cuando una misma
# característica, con respuesta dicotómica, se mide en dos ocasiones diferentes
# para los mismos sujetos (muestras pareadas) y se desea determinar si se produce
# o no un cambio significativo entre ambas mediciones.

# Hipótesis:
# H0: No hay cambios significativos antes y después en los niveles de ansiedad.
# HA: Sí hay cambios significativos antes y después en los niveles de ansiedad.

# Hipótesis matemática:
# H0: ansiedadAntes-ansiedadDespués = |0|
# HA: ansiedadAntes-ansiedadDespués != |0|

#      ANTES  DESPUÉS
#      Bajo   Bajo
#      Bajo   Bajo
#      Bajo   Bajo
#      Bajo   Bajo

#      Alto   Bajo
#      Alto   Bajo
#      Alto   Bajo
#      Alto   Bajo
#      Alto   Bajo

#      Alto   Alto
#      Alto   Alto
#      Alto   Alto
#      Alto   Alto

#      Bajo   Alto
#      Bajo   Alto


#                    ANTES      
#                 Alto   Bajo   Total
#  DESPUÉS  Alto     4      2       6 
#           Bajo     5      4       9     
#           Total    9      6      15


# Construir la tabla de contingencia .
estudiante_2 <- seq(1:15)
antes <- c( rep("Bajo", 6), rep("Alto", 9) )
despues <- c( rep("Bajo", 4), rep("Alto", 6), rep("Bajo", 5) )
datos <- data.frame(estudiante_2, despues, antes)
tabla2 <- table(despues, antes)
print(tabla2)


# Aplicar prueba de McNemar.
prueba2 <- mcnemar.test(tabla2)
print(prueba2)


# El valor de p es igual a 0,4497. En consecuencia, se falla al rechazar la
# hipótesis nula (para un nivel de significación α = 0,05) y se concluye 
# que no hay evidencia suficiente para creer que existe una diferencia en 
# el los niveles de ansiedad de los estudiantes antes y después de 
# participar en el nuevo programa de bienvenida.


#### Pregunta 3 ####
#En noviembre de 2019, se realizó un estudio acerca de la 
#aprobación al presidente Sebastián Piñera entre 440 profesores y 
#estudiantes de una prestigiosa universidad, obteniéndose los 
#resultados que se muestran en la tabla. ¿Son similares las 
#opiniones de ambos segmentos de la comunidad universitaria?


#             ! Aprueba ! Desaprueba  ! Ninguna ! Total
# ------------------------------------------------
# Estudiantes:!   35    !    208      !  17     ! 260
# Profesores: !   20    !    151      !   9     ! 180
# ------------------------------------------------
# Total       !   55    !    359      !  26     ! 440


#La pregunta nos indica si existe similitud entre las variables, 
#por lo que el enunciado nos da una idea de que se debe utilizar 
#la prueba chi-cuadrado de homogeneidad, ya que nos permite saber 
#si los datos presentan similud en las proporciones.


#Hipótesis:
#H0: Los estudiantes y profesores tienen similitud de opinión 
#respecto a la aprobación del presidente Sebastián Piñera.
#HA: Los estudiantes y profesores tienen opiniones diferentes 
#respecto a la aprobación del presidente Sebastián Piñera.

# Hipótesis matemática:
# H0: estudiantes-profesores = |0|
# HA: estudiantes-profesores != |0|

#Para utilizar chi-cuadrado de homogeneidad se deben verificar dos 
#puntos:
# 1. Datos independientes: Suponemos que la selección de 
# estudiantes y profesores fueron seleccionados de forma aleatoria 
# y no se han repetido.
# 2. Ver que cada grupo tengan como mínimo 5 observaciones 
# esperadas.

#Se crea la tabla entregada en el enunciado.
estudiantes <- c(35,208,17)
profesores <- c(20,151,9)
totalFilasEstudiantes <- (estudiantes[1]+estudiantes[2]+estudiantes[3])
totalFilasProfesores <-(profesores[1]+profesores[2]+profesores[3])
totalColumnaAprueba <-(estudiantes[1]+profesores[1])
totalColumnaDesaprueba <-(estudiantes[2]+profesores[2])
totalColumnaNinguna <-(estudiantes[3]+profesores[3])
totalProfEst <- totalFilasEstudiantes+totalFilasProfesores

tabla3 <- as.table ( rbind ( estudiantes , profesores ))
dimnames ( tabla3 ) <- list ( tipo = c(" Estudiantes ", " Profesores "),
                              resultados = c("Aprueba","Desaprueba","Ninguna"))
print(tabla3)


#Análisis de prueba a utilizar:
#Se analiza si en cada grupo se tienen como mínimo 5 observaciones 
#esperadas, para ver si se puede utilizar chi-cuadrado de 
#homogeneidad.
observacion1.3 <- (totalColumnaAprueba*totalFilasEstudiantes)/totalProfEst
observacion2.3 <- (totalColumnaDesaprueba*totalFilasEstudiantes)/totalProfEst
observacion3.3 <- (totalColumnaNinguna*totalFilasEstudiantes)/totalProfEst
observacion4.3 <- (totalColumnaAprueba*totalFilasProfesores)/totalProfEst
observacion5.3 <- (totalColumnaDesaprueba*totalFilasProfesores)/totalProfEst
observacion6.3 <- (totalColumnaNinguna*totalFilasProfesores)/totalProfEst
fila_1.3_Esperada <- c(observacion1.3,observacion2.3,observacion3.3)
fila_2.3_Esperada <- c(observacion4.3,observacion5.3,observacion6.3)


#Se crea la tabla con los datos esperados
tablaObservacioneEsperadas_3 <- as.table(rbind( fila_1.3_Esperada  , fila_2.3_Esperada ))

dimnames(tablaObservacioneEsperadas_3) <- list(tipo = c("Estudiantes","Profesores"),
                                               resultados = c("Aprueba","Desaprueba","Ninguna"))
print(tablaObservacioneEsperadas_3)
#Al tener la tabla de las frecuencias esperadas, se puede apreciar
#que todos los valores obtenidos superan las 5 observaciones, por
#lo que todas las condiciones se han cumplido para utilizar chi 
#cuadrado de homogeneidad.

#Se aplica chi-cuadrado a la tabla inicial
prueba3 <- chisq.test(tabla3)

#Se imprime el resultado
print(prueba3)

#El valor p obtenido corresponde a 0.57, lo que es mayor al nivel 
#de significación establecido de 0.05, por lo que la evidencia 
#no es suficiente para rechazar la hipótesis nula. Se concluye con 
#un 95% de confianza que los estudiantes y profesores tienen una 
#similitud de opinión respecto a la aprobación del presidente 
#Sebastián Piñera.


#### Pregunta 4 ####
# La Facultad de Ingeniería desea saber si existe diferencia significativa
# en el desempeño de los estudiantes en asignaturas críticas de primer semestre.
# Para ello, le ha entregado un archivo de datos que, para 3 asignaturas,
# indica si una muestra de 50 estudiantes aprobó o reprobó.
# ¿Qué puede concluir la Facultad? Indicación: obtenga la muestra a partir
# del archivo EP07 Datos.csv, usando la semilla 685. Considere un nivel de significación α=0,05.

# Hipótesis:
# H0: La proporción de aprobación entre las asignaturas es la misma.
# H1: La proporción de aprobación es distintas al menos para una asignatura.

# Hipótesis matemática:
# H0: proporciónAsignaturas = 0
# HA: proporciónAsignaturas != 0

# Para responder esta pregunta se usará la prueba Q de Cochran dado que 
# nos permite saber si la proporción de éxito para observaciones pareadas es la misma 
# para más de dos grupos. Además se cumplen las condiciones de una prueba Q de Cochran 
# dado que la variable es dicotómica (Aprobar o Reprobar), es una variable independiente
# y categórica, todas las observaciones son independientes entre si y el tamaño de la muestra
# multiplicado por la cantidad de niveles de la variable independiente es mayor a 24.


# Se setea el workspace
setwd("C:\\Users\\Dyllan\\Desktop\\TrabajosIME\\EP-07")
# Se lee el archivo con los datos
datos2 <- read.csv2("EP07 Datos.csv")
# Se define la semilla 
set.seed(685)
# Tomamos una muestra de 50 estudiantes
muestra <- sample_n(datos2,50)

# Llevar matriz de datos a formato largo
muestra <- muestra %>% pivot_longer ( c ( "Calculo" , "Algebra" , "Fisica" ) ,
                                      names_to = "Asignaturas" ,
                                      values_to = "Resultado" )

muestra[["Id"]] <- factor(muestra[["Id"]])
muestra[["Asignaturas"]] <- factor(muestra[["Asignaturas"]])

# Se realiza la prueba Q de Cochran.
cochran_test <- cochran.qtest(Resultado ~ Asignaturas | Id,
                              data = muestra, alfa)

print(cochran_test)

# El resultado de la prueba Q de Cochran nos da un valor p = 0.04233, por ende,
# se rechaza la hipótesis nula en favor de la alternativa,por lo que se puede afirmar
# con un 95% de confianza que en al menos una de las asignaturas los estudiantes presentan
# un desempeño diferente respecto al resto de las asignaturas.

#Cuando se utiliza Cochran se debe entregar una respuesta más detallada, es por este motivo que
#se verá si se puede utilizar la prueba de ómnibus o la de post-hoc.

#Como en este caso nuestras hipótesis solo permiten detectar si existen diferencias
# entre las proporciones de asignaturas (prueba ómnibus) y este rechazo la hipótesis nula
# se procede a utilizar una prueba post-hoc.

# Ahora bien, también se va a utilizar la corrección de Holm, ya que tiene un mayor poder estadístico
# que la de Benferroni.


prueba_post_hoc<- pairwiseMcnemar(Resultado ~ Asignaturas | Id ,
                                    data = muestra , method = "holm")

cat("Resultado prueba post hoc")

print (prueba_post_hoc)

#Al analizar los resultados se puede observar que si p.adjust (p con factor de holm )
# es mayor al p.value se puede considerar que existe una diferencia significativa
# por lo que la diferencia que se puede apreciar corresponde entre Álgebra-Cálculo y 
# Álgebra-física, ya que el p.value es menor al p de holm.
