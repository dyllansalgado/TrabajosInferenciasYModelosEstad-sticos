

#             ! Aprueba ! Desaprueba  ! Ninguna ! Total
# ------------------------------------------------
# Estudiantes:!   35    !    208      !  17     ! 260
# Profesores: !   20    !    151      !   9     ! 180
# ------------------------------------------------
# Total       !   55    !    359      !  26     ! 440


#La pregunta nos indica si existe similitud entre las variables, por lo 
#que el enunciado nos da una idea de que se debe utilizar la prueba chi-cuadrado 
#de homogeneidad,ya que nos permite saber si los datos presentan similud 
#en las proporciones.


#Hipótesis:
#H0: Los estudiantes y profesores tienen similitud de opinion respecto a la aprobación del 
#    presidente Sebastian Piñera.
#HA: Los estudiantes y profesores tienen opiniones diferentes respecto a la aprobación del
#    presidente Sebastian Piñera.

#Para utilizar chi-cuadrado de homogeneidad se deben verificar dos puntos:
# 1.Datos independientes: Suponemos que la seleccion de estudiantes y profesores fueron
# seleccionados de forma aleatoria y no se han repetido.
# 2.Ver que cada grupo tengan como mínimo 5 observaciones esperadas.

#Se crea la tabla entregada en el enunciado.
alfa <- 0.05
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


#Analisis de prueba a utilizar:
#Se analiza si en cada grupo se tienen como mínimo 5 observaciones 
#esperadas, para ver si se puede utilizar chi-cuadrado de homogeneidad.
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
#que todos los valores obtenidos superan las 5 observaciones, por lo que
#todas las condiciones se han cumplido para utilizar chi-cuadrado de 
#homogeneidad.


#Se aplica chi-cuadrado a la tabla inicial
prueba3 <- chisq.test(tabla3)

#Se imprime el resultado
print(prueba3)

#El valor p obtenido corresponde a 0.57, lo que es mayor al nivel de 
#significación establecido de 0.05, por lo que la evidencia 
#no es suficiente para rechazar la hipótesis nula. Se concluye con un 95% de 
#confianza que los estudiantes y profesores tienen una similitud de opinión respecto 
#a la aprobación del presidente Sebastián Piñera.