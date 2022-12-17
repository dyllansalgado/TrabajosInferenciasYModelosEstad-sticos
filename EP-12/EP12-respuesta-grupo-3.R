
#------------------------------------------------------------------------------
# Importar los paquetes, instalándolos de ser necesario.
# if (!require(tidyverse)) {
#   install.packages("tidyverse", dependencies = TRUE)
#   require(tidyverse) }

if (!require(ggpubr)) {
  install.packages("ggpubr", dependencies = TRUE)
  require(ggpubr) }

if (!require(WRS2)) {
  install.packages("WRS2", dependencies = TRUE)
  require(WRS2) }

if (!require(dplyr)) {
  install.packages("dplyr", dependencies = TRUE)
  require(dplyr) }

if (!require(tidyverse)) {
  install.packages("tidyverse", dependencies = TRUE)
  require(tidyverse) }

if (!require(ez)) {
  install.packages("ez", dependencies = TRUE)
  require(ez) }

#Enunciados:
#1) En el trabajo de título de un estudiante del DIINF se reportan los 
#siguientes tiempos de ejecución (en milisegundos) medidos para dos versiones 
#de un algoritmo genético para resolver instancias del problema del vendedor 
#viajero disponibles en repositorios públicos. 
#¿Es uno de los algoritmos más rápido que el otro?

# Instancia A|  Tiempo A  | Instancia B | Tiempo B  |
#   102      |   842079   |     43      |  6684497  |
#   112      |   834565   |     189     |  180141   |
#   151      |   885722   |     140     |  2830464  |
#   4        |   48705    |     51      |  1252837  |
#   55       |   4428151  |     146     |  5743260  |
#   70       |   62764    |     25      |  6701654  |
#   98       |   8576989  |     2       |  1174562  |
#   135      |   37449    |     179     |  120276   |
#   7        |   48667    |     68      |  994830   |
#   183      |   1510394  |     182     |  48408    |

instancia_A <- c(102,112,151,4,55,70,98,135,7,183)
tiempo_A <- c(842079,834565,885722,48705,4428151,62764,8576989,37449,
              48667,1510394)
instancia_B <- c(43,189,140,51,146,25,2,179,68,182)
tiempo_B <- c(6684497,180141,2830464,1252837,5743260,6701654,1174562,
              120276,994830,48408)

datosA <- data.frame(instancia_A,tiempo_A)
datosB <- data.frame(instancia_B,tiempo_B)

alfa <- 0.05
#Una vez obtenidas las tablas se procede a realizar la gráfica, para ver
# el comportamiento de los datos.

graficoA <- gghistogram ( datosA , x = "tiempo_A", bins = 10 ,
                    xlab = "Tiempo A en [Ms]", ylab = "Frecuencia",
                    color = " red", fill = " red")
print(graficoA)

graficoB <- gghistogram ( datosB , x = "tiempo_B", bins = 10 ,
                          xlab = "Tiempo B en [Ms]", ylab = "Frecuencia",
                          color = "blue", fill = " blue")
print(graficoB)


#Al analizar los gráficos se puede observar que los datos tienen una distribución
# muy asimétrica y no siguen la gráfica de una distribución normal.

#Se utilizará la transformación logarítmica.
log_tiempoA <- log(tiempo_A)
log_tiempoB <- log(tiempo_B)

datosATransformados <- data.frame( datosA , log_tiempoA )
datosBTransformados <- data.frame( datosB , log_tiempoB )

# Histogramas para el tiempo después de la transformación logarítmica.
graficoDespues_A <- gghistogram(datosATransformados,
                                     x ="log_tiempoA",
                                     bins = 10 ,
                                     xlab = "tiempo en [Ms]",
                                     ylab = "Frecuencia",
                                     color = "red",
                                     fill = "red")
print(graficoDespues_A)

graficoDespues_B <- gghistogram(datosBTransformados,
                                x ="log_tiempoB",
                                bins = 10 ,
                                xlab = "tiempo en [Ms]",
                                ylab = "Frecuencia",
                                color = "black",
                                fill = "black")

print(graficoDespues_B)

# Crear una única figura con ambos histogramas.
histograma_A <- ggarrange ( graficoA , graficoDespues_A  , ncol = 2 , nrow = 1)
titulo <- text_grob (" Efecto de la transformación logarítmica tiempo A",
                       face = "bold", size = 14)
histograma_A <- annotate_figure ( histograma_A , top = titulo )
print ( histograma_A)

histograma_B <- ggarrange ( graficoB , graficoDespues_B  , ncol = 2 , nrow = 1)
titulo <- text_grob (" Efecto de la transformación logarítmica tiempo B",
                     face = "bold", size = 14)
histograma_B <- annotate_figure ( histograma_B , top = titulo )
print ( histograma_B)


#Una vez ya realizada la comparación de los gráficos se puede observar, que la
# transformación de los datos si ha funcionado. Provocando que los datos
# sigan una distribución más cercana a la normal. Pero para continuar se procede
# a efectuar una prueba de normalidad como lo es shapiro test.


normalidad_Tiempo_A <- shapiro.test ( log_tiempoA )
print (normalidad_Tiempo_A)

normalidad_Tiempo_B <- shapiro.test ( log_tiempoB )
print (normalidad_Tiempo_B)

# Como el p-value de los dos tiempos transformados es mucho mayor a nuestro 
# nivel de significación de 0.05 (en este caso P-value A=0.12 y P-value B=0.21),
# se puede suponer que los datos siguen una distribución normal.


#Como los datos ya fueron transformados se puede aplicar una prueba paramétrica
# acorde para responder la pregunta solicitada.

#En este caso el enunciado nos solicita verificar si un algoritmo es más rápido
# que el otro, por lo que la prueba que se puede utilizar para responder 
# esta pregunta corresponde a la Prueba T de Student. Pero para su uso
# se deben verificar ciertos puntos:

# 1) Las observaciones son independientes entre sí.
# R: Si son independientes, ya que cada instancia corresponde a un identificador
# único sin repetición.
# 2) Las observaciones provienen de una distribución cercana a la normal.
# R: Al realizar la transformación, se verifica que los datos si siguen una 
# distribución normal.
# 3) Las muestras son independientes entre sí.
# R: Si se verifica, ya que la instancia A es diferente de la instancia B.


#Hipótesis:
#H0: No existe una diferencia significativa en los tiempos medios de ejecución 
# entre el algoritmo A y algoritmo B para resolver instancias del problema del 
# vendedor viajero, es decir, los tiempos medios son iguales.


#HA: Si existe una diferencia significativa en los tiempos medios de ejecución 
# entre el algoritmo A y algoritmo B para resolver instancias del problema del 
# vendedor viajero, es decir, los tiempos medios son diferentes.

#Hipótesis matemática:
#H0: mediaTiempoA = mediaTiempoB
#HA: mediaTiempoA != mediaTiempoB


# Aplicar la prueba t para dos muestras independientes .
prueba <- t.test (x = log_tiempoA ,
                      y = log_tiempoB ,
                      paired = FALSE ,
                      alternative = "two.sided",
                      mu = 0 ,
                      conf.level = 1 - alfa )
print ( prueba )

#Respuesta:
#En este caso, p > α (0.3218 > 0.05), por lo que se falla en rechazar la 
# hipótesis nula en favor de la hipótesis alternativa, por lo que se concluye 
# con un 95% de confianza que no existe una diferencia significativa en los 
# tiempos medios de ejecución entre el algoritmo A y algoritmo B para resolver 
# instancias del problema del vendedor viajero, es decir el tiempo medio es 
# igual.

#Segun lo visto en clases, al realizar la transformada se debe volver nuevamente
# a la escala original y tambien concluir con respecto a ella. Se puede trabajar
# con los intervalos de confianza, ya que se puede transformar, obtenerlos y 
# deshacer la transformación. 
# Ref= https://www.cienciasinseso.com/transformacion-de-datos/
prueba[["conf.int"]]

intervaloMenor <- exp(prueba[["conf.int"]][1])

intervaloMayor <- exp(prueba[["conf.int"]][2])

conf.int_transformado <- c(intervaloMenor,intervaloMayor)

print(conf.int_transformado)

#Conclusión

#Con los resultados obtenidos en el procedimiento de volver el intervalo de
#confianza a la escala de tiempo original, se logra validar el resultado obtenido
#anteriormente, luego entonces si es posible concluir con un 95% de confianza
#que no existe una diferencia significativa en los tiempos medios de ejecución
#entre el algoritmo A y algoritmo B para resolver instancias del problema del
#vendedor viajero, es decir, el tiempo medio es igual.


#2) Analice la primera pregunta abordada en el ejercicio práctico 11, con los 
#mismos datos, utilizando un método robusto adecuado.


# Se setea el workspace
setwd("C:\\Users\\Dyllan\\Desktop\\TrabajosIME\\EP-11")
datos <- read.csv2("EP11 Datos.csv",encoding = "UTF-8")
#Se establece un nivel de significación para la primera pregunta.

alfa_2 <- 0.05

#Una vez obtenido los datos se procede a realizar la pregunta original, para
# establecer las hipótesis.

#Como grupo número 3, tenemos una gran duda respecto de que si la zona 
# en donde viven los ciudadanos, influyen en la cantidad de
# hijos vivos que la o las mujeres han tenido a lo largo de su vida en la 
# vivienda. Es por este motivo que surge la siguiente pregunta:
# ¿Dependiendo de la zona de Chile existen diferencias significativas
# en la cantidad de hijos nacidos?. Cabe destacar que se ha seleccionado 
# una Región perteneciente a la zona Sur de Chile, como lo es la Región de Los 
# Lagos y para zona Norte de Chile se ha seleccionado la Región de Tarapacá.

#Hipótesis:
#H0: No existe diferencia entre la cantidad de hijos que se tienen en promedio
# en la Región de Tarapacá (Zona Norte) y la Región de Los Lagos (Zona Sur).
#HA: Si existe una diferencia entre la cantidad de hijos que se tienen en 
# promedio en la Región de Tarapacá (Zona Norte) y la Región de Los Lagos 
# (Zona Sur).


#Hipótesis matemática:
#URT = Media de hijos nacidos en la Región de Tarapacá.
#URLG = Media de hijos nacidos en la Región de Los Lagos.
#H0: URT - URLG = 0
#HA: URT - URLG != 0

#Se selecciona una muestra aleatoria de hogares que se encuentren entre 
# 250 y 500.
datos1 <- sample_n(datos, 450)
set.seed(650)
#Se seleccionan los datos de los ciudadanos de la Región de Tarapacá.
ciudadanosRT <- datos1 %>% filter (region == "Región de Tarapacá")
#Se seleccionan los datos de los ciudadanos de la Región de Los Lagos.
ciudadanosRLG <- datos1 %>% filter (region == "Región de Los Lagos")

#Una vez seleccionada las regiones se procede a obtener solo los datos
# de interés, que en este caso son el id.vivienda y el s4.

viviendasRT <- ciudadanosRT %>% select(s4)
viviendasRLG <- ciudadanosRLG %>% select(s4)

# Pasamos a vector el dato de interés que corresponde a s4 para obtener el largo
# y añadirlos a un nuevo dataframe.
viviendasRTvectorizados  <- viviendasRT$s4
viviendasRLGvectorizados <- viviendasRLG$s4

viviendasJuntas <- c(viviendasRTvectorizados
                     ,viviendasRLGvectorizados)

region <- c(rep("Tarapacá",length(viviendasRTvectorizados)) , 
            rep("Los Lagos",length(viviendasRLGvectorizados)))

datosViviendas <- data.frame ( viviendasJuntas , region )

# Se procede a comprobar la normalidad y se puede observar que los datos
#siguen la zona de normalidad.
g <- ggqqplot ( datosViviendas , x = "viviendasJuntas", facet.by = "region",
              palette = c(" blue ", " red") , color = "region")

print ( g )
# Una vez realizada la normalidad se debe seleccionar una prueba robusta.
# En este caso se ha seleccionado la prueba de Yuen para dos muestras
# independientes, por el motivo que los datos son independientes y 
# con un ID único que es el, id de la vivienda.

# Establecer nivel de significación y cantidad de muestras a generar
# con bootstrapping .
alfa_2 <- 0.05
bootstrap <- 999

set.seed(650)
prueba_media <- pb2gen ( viviendasJuntas ~ region ,
                           data = datosViviendas ,
                           est = "mean",
                           nboot = bootstrap )

cat ("\n\ nResultado al usar la media como estimador \n\n")
print ( prueba_media )

# Aplicar prueba con la mediana
set.seed (650)

prueba_mediana <- pb2gen ( viviendasJuntas ~ region ,
                         data = datosViviendas ,
                         est = "median",
                         nboot = bootstrap )

cat ("\n\ nResultado al usar la mediana como estimador \n\n")
print ( prueba_mediana )

#Conclusión:

#En base a los resultados observados, es posible concluir que dado que el valor
#p-value obtenido (utilizando como estimador la media y la mediana) es mayor al 
#nivel de significación (0.78 para la media y 0.66 para la mediana), entonces se 
#falla en rechazar la hipótesis nula en favor de la hipótesis alternativa, por 
#lo que no existe una diferencia entre la cantidad de hijos que se tienen en 
#promedio en la Región de Tarapacá (Zona Norte) y la Región de Los Lagos 
#(Zona Sur).


#3) Analice la segunda pregunta abordada en el ejercicio práctico 11, con 
#los mismos datos, utilizando un método robusto adecuado.

#Pregunta 2 EP11

#Propongan una pregunta de investigación original, que involucre la comparación 
#de las medias de más de dos grupos independientes (más abajo se dan unos 
#ejemplos). Fijando una semilla distinta a la anterior, seleccionen una muestra 
#aleatoria de hogares (400 < n < 600) y respondan la pregunta propuesta 
#utilizando bootstrapping. Solo por ejercicio académico, aplique un análisis 
#post-hoc con bootstrapping aunque este no sea necesario.

#Pregunta propuesta.

#¿El ingreso promedio de los hogares es igual para las regiones
#Metroplitana, Tarapacá y el Maule?

#Se establecen las hipótesis nula e hipótesis alternativa.

#H0: El ingreso promedio de los hogares es igual para las regiones Metropolitana
#, Tarapacá y el Maule.
#HA: El ingreso promedio de los hogares es diferente para las regiones 
#Metropolitana, Tarapacá y el Maule.


#Se importan los datos a trabajar.
datosP3 <- read.csv2("EP11 Datos.csv",encoding = "UTF-8")
#Se fija la semilla 789
set.seed(789)
#Se obtiene una muestra de 550 datos.
datosP3Muestra <- sample_n(datosP3,550)

regionMetropolitana <- filter(datosP3Muestra, region=="Región Metropolitana de Santiago")$ytotcorh
regionMaule <- filter(datosP3Muestra, region=="Región del Maule")$ytotcorh
regionTarapaca <- filter(datosP3Muestra, region=="Región de Tarapacá")$ytotcorh

regionesIngresoPromedio <- c(regionMetropolitana,regionMaule,regionTarapaca)

nombreRegiones <- c(rep("Región Metropolitana", length (regionMetropolitana) ) , 
                    rep("Region del Maule", length (regionMaule) ) , 
                    rep ("Región de Tarapacá", length (regionTarapaca) ) )

datosJuntosP3 <- data.frame (regionesIngresoPromedio , nombreRegiones )

# Comprobar normalidad .
g3 <- ggqqplot(datosJuntosP3 , x = "regionesIngresoPromedio", facet.by = "nombreRegiones",
                   palette = c(" blue ", " red", "black") , color = "nombreRegiones")

print ( g3 )

#Se puede observar que existen muchos valores atípicos en al gráfica, por lo que
# se puede concluir que no siguen la normalidad.

#La prueba seleccionada corresponde a Yuen para múltiples grupos independientes,
# teniendo como justificación: 1)Se tienen más de dos muestras independientes y 
# 2)Al no seguir la normal se debe truncar considerando un valor gamma.

# Fijar nivel de significación.
alfaP3 <- 0.05

# Comparar los diferentes algoritmos usando medias truncadas.
cat (" Comparación entre grupos usando medias truncadas \n\n")
gamma <- 0.2

medias_truncadas <- t1way( regionesIngresoPromedio ~ nombreRegiones , 
                            data = datosJuntosP3 , tr = gamma ,
                            alpha = alfaP3 )

print (medias_truncadas)

#Conclusión
#Una vez obtenido el resultado del P-value(0.00002) usando medias truncadas, se puede 
#observar que este valor es mucho menor a nuestro nivel de significación de 0.05,
#por lo que es posible señalar con un 95% de confianza que se rechaza la 
#hipótesis nula en favor de la hipótesis alternativa, por lo que el ingreso 
#promedio de los hogares es diferente para las regiones Metropolitana, Tarapacá y 
#el Maule.

#Como nos dio que si existe una diferencia en los ingresos promedios de los 
#hogares, se puede realizar una prueba Post-Hoc para ver qué regiones tienen
#diferencias.
if( medias_truncadas$p.value < alfaP3 ) {
  cat ("\ nProcedimiento post - hoc\n\n")
  set.seed (789)
  post_hoc <- lincon ( regionesIngresoPromedio ~ nombreRegiones , 
                       data = datosJuntosP3 , tr = gamma ,
                       alpha = alfaP3 )
  print ( post_hoc )
  }

#Al analizar los resultados obtenidos de la prueba post-hoc se puede concluir que 
#que existen dos valores que son menores a nuestro nivel de significación, en 
#este caso corresponde a la Región de Tarapacá-Región del Maule que tiene un 
#P-value = 0.00073 y Región del Maule-Región Metropolitana que tiene un 
#P-value = 0.00073, quedando P-value < alfaP3(0.05). Permitiendo
#concluir con un 95% de confianza que entre la Región de Tarapacá-Región del 
#Maule y Región del Maule-Región Metropolitana, si existe una diferencia 
#significativa al ingreso promedio de los hogares, ahora si se quiere ser 
#más específico se puede apreciar que la región que presenta una mayor diferencia 
#significativa corresponde a la Región del Maule.
