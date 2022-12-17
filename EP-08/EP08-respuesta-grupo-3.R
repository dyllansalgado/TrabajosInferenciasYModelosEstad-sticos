

#-------------------------------------------------------------------------------
#ENUNCIADO:
#Un equipo de investigadores del área de interacción humano-información está 
#estudiando si el área temática y el nivel de dificultad del problema de 
#información influyen en el tiempo (en segundos) que toma un usuario en
#formular una consulta de búsqueda para resolver dicho problema. Para ello, 
#han reclutado a un grupo de participantes voluntarios, asignados aleatoriamente
#a distintos grupos. Cada participante debe resolver tres problemas de 
#información con diferentes niveles de dificultad: baja, media y alta. A su vez,
#cada grupo debe resolver problemas relacionados a una temática diferente. 
#Los datos recolectados contemplan las siguientes variables:

#▪ id: identificador único de cada participante.
#▪ area: área temática de los problemas que el participante debe responder. 
# Variable categórica con los niveles Arquitectura, Biología, Computación, 
# Economía, Física, Leyes, Literatura, Matemáticas, Música, Pedagogía,Psicología, 
# Química.
#▪ dificultad: nivel de dificultad del problema resuelto. Variable categórica 
# con los niveles Baja, Media y Alta.
#▪ tiempo: tiempo, en segundos, que toma al participante formular la consulta.
#-------------------------------------------------------------------------------

#Pregunta a responder:
#En este momento, los investigadores buscan determinar si existen diferencias 
#en el tiempo que tardan los usuarios en formular una consulta para un problema 
#de dificultad media en las áreas de economía, literatura y arquitectura.


#Hipótesis:

#H0: El tiempo utilizado para formular una consulta de dificultad media para 
# las áreas de economía, literatura y arquitectura es el mismo.

#HA: El tiempo utilizado para formular una consulta de dificultad media es 
# diferente para al menos una de las áreas como pueden ser economía, literatura
# y arquitectura.

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

# Se setea el workspace
setwd("C:\\Users\\Dyllan\\Desktop\\TrabajosIME\\EP-08")
# El archivo EP08 Datos.csv se le ha cambiado el formato, pasando de 
# ANSI a UTF-8

# Se lee el archivo con los datos
datos <- read.csv2("EP08 Datos.csv",encoding = "UTF-8")
# Se seleccionan los datos de dificultad media.
dificultad_media <- datos %>% filter(dificultad == "Media")
# Se obtienen las áreas solicitadas por el enunciado.
areas_dificultad_media <- dificultad_media %>% filter (area=="Economía" | 
                                                       area == "Literatura" |
                                                       area == "Arquitectura")
#Una vez obtenido los datos se debe proceder a verificar los 4 puntos
#para utilizar la prueba de Anova:

#1.La escala con que se mide la variable dependiente tiene las propiedades 
#de una escala de intervalos iguales.

# Considerando que la variable dependiente y que será utilizada para responder
# el enunciado es el tiempo [S], se puede apreciar que sigue las propiedades
# de una escala de intervalos iguales.

#2. Las k muestras son obtenidas de manera aleatoria e independiente desde 
# la(s) población(es) de origen.

# Este punto se comprueba, ya que el enunciado dice explícitamente que 
# la muestra ha sido seleccionada de manera aleatoria y también se comprueba 
# que son independientes los datos, puesto que son personas con ID únicos.

#3. Se puede suponer razonablemente que la(s) población(es) de origen 
#sigue(n) una distribución normal.

#Se obtienen los datos para utilizarlos en el gráfico
informacionPregunta <- areas_dificultad_media %>% select(id, area, tiempo)

#Una vez seleccionadas las áreas se debe comprobar si los datos siguen la normal.

informacionPregunta[["area"]] <- factor(areas_dificultad_media[["area"]])

informacionPregunta[["instancia"]] <- factor(1:nrow(areas_dificultad_media))


# Comprobación de normalidad de las tres áreas
g <- ggqqplot ( informacionPregunta ,
                   x = "tiempo",
                   y = " area ",
                   color = "area")

g <- g + facet_wrap(~ area )
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title")
print(g)

#Analizando los gráficos se puede observar que existen muy pocos valores atípicos,
# y la gran mayoría de los datos se centra en seguir la región aceptable de normalidad
# presente en el gráfico. Es por este motivo que los datos si siguen la normal.

#4. Si las muestras provienen de más de una población, estas tienen la misma 
# varianza.

# Para comprobar que los datos tengan la misma varianza se debe utilizar la
# prueba de homocedasticidad de Levene, ya que nos permite obtener la
# homogeneidad de las varianzas o también conocida como homocedasticidad.

# H0: Las varianzas para las áreas de economía, literatura y arquitectura es el 
# mismo.
# HA: Al menos una de las varianzas de las áreas de economía, literatura y 
# arquitectura es diferente.

#Se asigna un nivel de significación para la pregunta
alfa <- 0.05

# La función ezANOVA () permite calcular la homocedasticidad, donde:
# data: son los datos a analizar.
# dv: es la variable dependiente con escala de igual intervalo.
# beetwen: variable independiente.
# wid: variable o factor con el identificador de cada instancia.
#   similar o igual a los ID.
# return_aov: Si es TRUE devuelve un obj. tipo aov.
prueba_Anova <- ezANOVA (data = informacionPregunta ,
                         dv = tiempo ,
                         between = area ,
                         wid = instancia ,
                         return_aov = TRUE )

print ( prueba_Anova )

#Al realizar la comparación del nivel de significación utilizado de 
# 0.05 con el valor P obtenido de la prueba ezANOVA pero con el resultado
# de LEVENE, que es 0.458, se puede concluir que P > alfa, por lo que se falla en 
# rechazar la hipótesis nula en favor de la hipótesis alternativa, por lo que se
# concluye con un 95% de confianza, que el valor de las varianzas son iguales.

#Asimismo, al realizar la comparación del nivel de significación utilizado de 0.05 
# con el valor P obtenido de la prueba ezANOVA, que es 8.029312e-11, se puede 
# concluir que P < alfa, por lo cual se rechaza la hipótesis nula en favor de la 
# hipótesis alternativa, concluyendo con un 95% de confianza, que el tiempo 
# utilizado para formular una consulta de dificultad media es diferente para al 
# menos una de las áreas como pueden ser economía, literatura y arquitectura.

# Gráfico del tamaño del efecto .
g2 <- ezPlot (data = informacionPregunta ,
              dv = tiempo ,
              wid = instancia ,
              between = area ,
              y_lab = " Tiempo promedio para realizar consulta de
              dificultad media en [S]",
              x = area)
print (g2)

#Como se ha concluido que el tiempo utilizado para formular una consulta 
# de dificultad media es diferente para al menos una de las áreas y al analizar
# el gráfico del efecto confirma esta acción (Economía utiliza un tiempo promedio
# mucho mayor que Arquitectura y Literatura), se puede utilizar una prueba 
# Post-Hoc para determinar cuál es la diferencia.

#En este caso el capítulo nos recomienda utilizar la prueba HSD de Tukey, por
# el motivo que es más poderosa que los factores de corrección de Holm y 
# Bonferroni.

#Se obtiene el valor de aov desde la funcion ezANOVA
anova <-prueba_Anova[["aov"]]

post_hoc <- TukeyHSD (anova ,
                      "area",
                      ordered = TRUE ,
                      conf.level = 1 - alfa )
print ( post_hoc )

#Al tener los resultados de la prueba de Tukey, se puede realizar la 
# comparación entre el P-adj y el nivel de significación utilizado para este
# ejercicio, el cual nos da como resultado que existen dos valores que son 
# menores a nuestro nivel de significación, en este caso corresponde a
# Economía-Literatura que tiene un P-adj = 0 y Economía-Arquitectura que 
# también tiene un P-adj = 0, quedando P-adj < alfa(0.05). Permitiendo
# concluir con un 95% de confianza que entre Economía-Literatura y 
# Economía-Arquitectura, si existe una diferencia significativa en el 
# tiempo que tardan los usuarios para formular una consulta para un problema de 
# dificultad media, ahora si se quiere ser más específico se puede apreciar
# que el área que presenta una mayor diferencia significativa corresponde a
# Economía.