
#ENUNCIADO
#------------------------------------------------------------------------------
#Un equipo de investigadores del área de interacción humano-información está 
# estudiando si el área temática y el nivel de dificultad del problema de 
# información influyen en el tiempo (en segundos) que toma un usuario en 
# formular una consulta de búsqueda para resolver dicho problema. Para ello, 
# han reclutado a un grupo de participantes voluntarios, asignados 
# aleatoriamente a distintos grupos. Cada participante debe resolver tres 
# problemas de información con diferentes niveles de dificultad: baja, media y 
# alta. A su vez, cada grupo debe resolver problemas relacionados a una 
# temática diferente. Los datos recolectados contemplan las siguientes variables:

# ▪ id: identificador único de cada participante.
# ▪ area: área temática de los problemas que el participante debe responder. 
# Variable categórica con los niveles Arquitectura, Biología, Computación, 
# Economía, Física, Leyes, Literatura, Matemáticas, Música, Pedagogía,Psicología, 
# Química.
#▪ dificultad: nivel de dificultad del problema resuelto. Variable categórica 
# con los niveles Baja, Media y Alta.
#▪ tiempo: tiempo, en segundos, que toma al participante formular la consulta.


#Pregunta a responder:
#En este momento, los investigadores buscan determinar si existen diferencias 
# en el tiempo que tardan los usuarios en formular consultas para problemas con 
# diferente nivel de dificultad en el área de química.

#Hipótesis:

#H0: El tiempo que tardan en formular consultas para problemas de diferente 
# nivel de dificultad (baja, media, alta) en el área de química son los mismos o
# iguales.

#HA: El tiempo que tardan en formular consultas para problemas de diferente 
# nivel de dificultad (baja, media, alta) en el área de química son diferentes
# para al menos uno de estos niveles.

# Importar los paquetes, instalándolos de ser necesario.
if (!require(tidyverse)) {
  install.packages("tidyverse", dependencies = TRUE)
  require(tidyverse) }

if (!require(ggpubr)) {
  install.packages("ggpubr", dependencies = TRUE)
  require(ggpubr) }

if (!require(ez)) {
  install.packages("ez", dependencies = TRUE)
  require(ez) }

if (!require(nlme)) {
  install.packages("nlme", dependencies = TRUE)
  require(nlme) }

if (!require(emmeans)) {
  install.packages("emmeans", dependencies = TRUE)
  require(emmeans) }

# Se setea el workspace
setwd("C:\\Users\\Dyllan\\Desktop\\TrabajosIME\\EP-09")
# El archivo EP08 Datos.csv se le ha cambiado el formato, pasando de 
# ANSI a UTF-8

# Se lee el archivo con los datos
datos <- read.csv2("EP08 Datos.csv",encoding = "UTF-8")
# Se obtiene la área solicitada por el enunciado, que en este caso 
# corresponde a Química.
area_quimica <- datos %>% filter (area=="Química")

# Se obtienen los datos como factor para ser utilizados en las pruebas.
area_quimica[["dificultad"]] <- factor(area_quimica[["dificultad"]])
area_quimica[["id"]] <- factor(area_quimica[["id"]])
datos_seleccionados <- area_quimica %>% select(id, dificultad, tiempo)

# Se establece un nivel de significación para responder la pregunta solicitada.
alfa <- 0.05

#Una vez seleccionado los datos se debe realizar la verificación de las
# condiciones para utilizar la prueba de Anova:

#1. La escala con que se mide la variable dependiente tiene las propiedades de 
# una escala de intervalos iguales.

# Considerando que la variable dependiente y que será utilizada para responder
# el enunciado es el tiempo [S], se puede apreciar que sigue las propiedades
# de una escala de intervalos iguales.

#2. Las mediciones son independientes al interior de cada grupo.

# Este punto se comprueba, ya que el enunciado dice explícitamente que 
# la muestra corresponde a usuarios voluntarios y fueron asignados de manera 
# aleatoria a los diferentes grupos, por último también se comprueba que son 
# independientes los datos, puesto que son personas con ID únicos.

#3. Se puede suponer razonablemente que la(s) población(es) de origen sigue(n) 
# una distribución normal.


#Comprobación de normalidad mediante gráficos Q-Q.
g_QQ <- ggqqplot ( datos_seleccionados , 
                x = "tiempo", 
                y = "dificultad", 
                color = "dificultad")

g_QQ <- g_QQ + facet_wrap (~ dificultad )
g_QQ <- g_QQ + rremove("x.ticks") + rremove("x.text")
g_QQ <- g_QQ + rremove("y.ticks") + rremove("y.text")
g_QQ <- g_QQ + rremove("axis.title")
print ( g_QQ )

#Analizando los gráficos se puede observar que no hay presencia
# de valores atípicos, y los datos se centran en seguir la región aceptable de 
# normalidad presente en el gráfico. Es por este motivo que los datos si siguen 
# la normal.

#4. La matriz de varianzas-covarianzas es esférica. Es decir, las varianzas entre
# los diferentes niveles de las medidas repetidas deben ser iguales.


#Para verificar este punto se procede a utilizar la función de ezANOVA(), ya que
# nos permite obtener la prueba de esfericidad de Maunchly.

prueba_anova <- ezANOVA(data = datos_seleccionados,
                        dv = tiempo,
                        within = dificultad,
                        wid = id, 
                        return_aov = TRUE )

print(prueba_anova)
#Se muestra por consola el resultado de la esfericidad de Mauchly.
cat ("Resultado de la prueba de esfericidad de Mauchly\n\n")
print (prueba_anova[["Mauchly's Test for Sphericity"]])


# Al obtener el valor P de la prueba de esfericidad de Mauchly, y compararlo
# con el nivel de significación establecido, se obtiene que P de Mauchly= 0.41 
# es mucho mayor a 0.05 (alfa < P de Mauchly) por lo que se concluye con un 95% 
# de confianza que no se ha violado el supuesto de que las varianzas son iguales, 
# es decir que si se cumple la condición de esfericidad.

# Ahora que se han verificado los 4 puntos requeridos para utilizar la prueba
# Anova, se puede concluir respecto del valor obtenido al utilizar la función
# ezANOVA(), el cual nos da como resultado un P-value de 5.447426e-09, lo cual
# es mucho menor a nuestro nivel de significación de 0.05, quedando 
# P-value < alfa, por lo cual se rechaza la hipótesis nula en favor de la 
# hipótesis alternativa, concluyendo con un 95% de confianza, que el tiempo que 
# tardan en formular consultas para problemas de diferente nivel de dificultad 
# (baja, media, alta) en el área de química son diferentes para al menos uno de 
# estos niveles.

# Gráfico del tamaño del efecto .
gTamaño <- ezPlot ( data = datos_seleccionados, 
               dv = tiempo, 
               wid = id,
               within = dificultad,
               y_lab = " Tiempo promedio para problemas con diferente nivel 
               en [S]",
               x = dificultad )

print ( gTamaño )

#Como se ha concluido que el tiempo que tardan en formular consultas para 
# problemas de diferente nivel de dificultad (baja, media, alta) 
# en el área de química son diferentes para al menos uno de estos niveles y
# al analizar el gráfico del efecto confirma esta acción (Baja utiliza un tiempo 
# promedio mucho menor que Alta y Media), se puede utilizar una prueba 
# Post-Hoc para determinar cuál es la diferencia.

#En este caso el capítulo nos recomienda utilizar la prueba HSD de Tukey, por
# el motivo que es más poderosa que los factores de corrección de Holm y 
# Bonferroni.

# Procedimiento post-hoc HSD de Tukey .
mixto <- lme ( tiempo ~ dificultad , data = datos_seleccionados , random = ~1| id )
medias <- emmeans ( mixto , "dificultad")
tukey <- pairs ( medias , adjust = "tukey")
print(tukey)

#Al tener los resultados de la prueba de Tukey, se puede realizar la 
# comparación entre el P-adj y el nivel de significación utilizado para este
# ejercicio, el cual nos da como resultado que existen dos valores que son 
# menores a nuestro nivel de significación, en este caso corresponde a
# Alta-Baja que tiene un P-adj = 0.0001 y Baja-Media que 
# también tiene un P-adj = 0.0001, quedando P-adj < alfa(0.05). Permitiendo
# concluir con un 95% de confianza que entre Alta-Baja y Baja-Media, si existe 
# una diferencia significativa en el tiempo que tardan los usuarios para 
# formular una consulta para un problema de diferente nivel para el área de 
# química, ahora si se quiere ser más específico se puede apreciar
# que el área que presenta una mayor diferencia significativa corresponde a
# Baja.


