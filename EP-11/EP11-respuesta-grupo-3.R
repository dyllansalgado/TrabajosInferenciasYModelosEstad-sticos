#Trabajo grupo 3
#Integrantes:
#FABRIZIO ANTONIO MONTUSCHI ALMENDRAS
#BENJAMÍN CRISTOPHER SAGREDO DURÁN
#DYLLAN IGNACIO SALGADO ESPINOZA
#MAURICIO SEBASTIÁN VALDÉS GÁLVEZ
#------------------------------------------------------------------------------
# Importar los paquetes, instalándolos de ser necesario.
# if (!require(tidyverse)) {
#   install.packages("tidyverse", dependencies = TRUE)
#   require(tidyverse) }

if (!require(ggpubr)) {
  install.packages("ggpubr", dependencies = TRUE)
  require(ggpubr) }

if (!require(boot)) {
  install.packages("boot", dependencies = TRUE)
  require(boot) }

if (!require(bootES)) {
  install.packages("bootES", dependencies = TRUE)
  require(bootES) }

if (!require(ggpubr)) {
  install.packages("ggpubr", dependencies = TRUE)
  require(ggpubr) }

if (!require(simpleboot)) {
  install.packages("simpleboot", dependencies = TRUE)
  require(simpleboot) }

if (!require(dplyr)) {
  install.packages("dplyr", dependencies = TRUE)
  require(dplyr) }

if (!require(nlme)) {
  install.packages("nlme", dependencies = TRUE)
  require(nlme) }

if (!require(emmeans)) {
  install.packages("emmeans", dependencies = TRUE)
  require(emmeans) }

if (!require(ez)) {
  install.packages("ez", dependencies = TRUE)
  require(ez) }

if (!require(tidyr)) {
  install.packages("tidyr", dependencies = TRUE)
  require(tidyr) }

# Se setea el workspace
setwd("C:\\Users\\Dyllan\\Desktop\\TrabajosIME\\EP-11")
datos <- read.csv2("EP11 Datos.csv",encoding = "UTF-8")
#Se establece un nivel de significación para la primera pregunta.

alfa <- 0.05
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
#HA: Si existe una diferencia entre la cantidad de hijos que se tienen en promedio
# en la Región de Tarapacá (Zona Norte) y la Región de Los Lagos (Zona Sur).

#Hipótesis matemática:
#URT = Media de hijos nacidos en la Región de Tarapacá.
#URLG = Media de hijos nacidos en la Región de Los Lagos.
#H0: URT - URLG = 0
#HA: URT - URLG != 0

#Se fija la semilla solicitada en el enunciado.

set.seed(650)
#Se selecciona una muestra aleatoria de hogares que se encuentren entre 
# 250 y 500.
datos1 <- sample_n(datos, 450)

#Se seleccionan los datos de los ciudadanos de la Región de Tarapacá.
ciudadanosRT <- datos1 %>% filter (region == "Región de Tarapacá")
#Se seleccionan los datos de los ciudadanos de la Región de Los Lagos.
ciudadanosRLG <- datos1 %>% filter (region == "Región de Los Lagos")

#Una vez seleccionada las regiones se procede a obtener solo los datos
# de interés, que en este caso son el id.vivienda y el s4.

viviendasRT <- ciudadanosRT %>% select(id.vivienda, s4)
viviendasRLG <- ciudadanosRLG %>% select(id.vivienda, s4)

# Pasamos a vector el dato de interés que corresponde a s4 para que pueda ser
# utilizado en las funciones de Monte Carlo más adelante.
viviendasRTvectorizados  <- viviendasRT$s4
viviendasRLGvectorizados <- viviendasRLG$s4

#Al tener todos los datos necesarios para realizar el análisis, se procede
# a definir las funciones necesarias para utilizar la prueba de Monte Carlo.

# Función para obtener una permutación.
# Argumentos :
# - i: iterador ( para llamadas posteriores ).
# - muestra_1 , muestra_2: muestras .
# Valor :
# - lista con las muestras resultantes tras la permutación.
obtiene_permutacion <- function(i, muestra_1, muestra_2) {
  n_1 <- length(muestra_1)
  combinada <- c(muestra_1, muestra_2)
  n <- length(combinada)
  permutacion <- sample(combinada, n, replace = FALSE)
  nueva_1 <- permutacion[1:n_1]
  nueva_2 <- permutacion[(n_1+1):n]
  return(list(nueva_1, nueva_2))
}

# Función para calcular la diferencia de un estadístico de interés entre las
# dos muestras .
# Argumentos :
# - muestras : lista con las muestras .
# - FUN : nombre de la función que calcula el estadístico de interés.
# Valor :
# - diferencia de un estadístico para dos muestras .
calcular_diferencia <- function(muestras, FUN) {
  muestra_1 <- muestras[[1]]
  muestra_2 <- muestras[[2]]
  diferencia <- FUN(muestra_1) - FUN(muestra_2)
  return(diferencia)
}

# Función para calcular el valor p.
# Argumentos :
# - distribucion : distribución nula del estadístico de interés.
# - valor_observado : valor del estadístico de interés para las muestras
# originales .
# - repeticiones : cantidad de permutaciones a realizar .
# - alternative : tipo de hipótesis alternativa . "two. sided " para
# hipótesis bilateral , "greater" o "les " para hipótesis unilaterales .
# Valor :
# - el valorp calculado .
calcular_valor_p <- function(distribucion, valor_observado,
                             repeticiones, alternative) {
  if(alternative == "two.sided") {
    numerador <- sum(abs(distribucion) > abs(valor_observado)) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
  }
  else if(alternative == "greater") {
    numerador <- sum(distribucion > valor_observado) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
  }
  else {
    numerador <- sum(distribucion < valor_observado) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
  }
  
  return(valor_p)
}
# Función para graficar una distribución.
# Argumentos :
# - distribucion : distribución nula del estadístico de interés.
# - ...: otros argumentos a ser entregados a gghistogram y ggqqplot .
graficar_distribucion <- function(distribucion, ...) {
  observaciones <- data.frame(distribucion)
  
  histograma <- gghistogram(observaciones, x = "distribucion",
                            xlab = "Estadístico de interés",
                            ylab = "Frecuencia", bins = 30, ...)
  
  qq <- ggqqplot(observaciones, x = "distribucion", ...)
  
  # Crear una única figura con todos los gráficos de dispersión.
  figura  <- ggarrange(histograma, qq, ncol = 2, nrow = 1)
  print(figura)
}
# Función para hacer la prueba de permutaciones .
# Argumentos :
# - muestra_1 , muestra_2: vectores numéricos con las muestras a comparar .
# - repeticiones : cantidad de permutaciones a realizar .
# - FUN : función del estadístico E para el que se calcula la diferencia .
# - alternative : tipo de hipótesis alternativa . "two.sided " para
# hipótesis bilateral , " greater " o " less " para hipótesis unilaterales .
# - plot : si es TRUE , construye el gráfico de la distribución generada .
# - ...: otros argumentos a ser entregados a graficar_distribucion .
contrastar_hipotesis_permutaciones <- function(muestra_1, muestra_2,
                                               repeticiones, FUN,
                                               alternative, plot, ...) {
  cat("Prueba de permutaciones\n\n")
  cat("Hipótesis alternativa:", alternative, "\n")
  observado <- calcular_diferencia(list(muestra_1, muestra_2), FUN)
  cat("Valor observado:", observado, "\n")
  n_1 <- length(muestra_1)
  
  # Generar permutaciones.
  permutaciones <- lapply(1:repeticiones, obtiene_permutacion, muestra_1,
                          muestra_2)
  
  # Generar la distribución.
  distribucion <- sapply(permutaciones, calcular_diferencia, FUN)
  
  # Graficar la distribución.
  if(plot) {
    graficar_distribucion(distribucion, ...)
  }
  
  # Calcular el valor p.
  valor_p <- calcular_valor_p(distribucion, observado, repeticiones,
                              alternative)
  
  cat("Valor p:", valor_p, "\n\n")
}

# Se establece la cantidad de repeticiones generalmente terminada en 9 para 
# simplificar los cómputos
R = 4999

# Se realiza la prueba de permutaciones para la media.
contrastar_hipotesis_permutaciones(viviendasRTvectorizados, 
                                   viviendasRLGvectorizados, repeticiones = R, 
                                   FUN = mean,alternative = "two.sided",
                                   plot = TRUE,color = "blue", fill = "blue")

#Respuesta pregunta 1:

# Al realizar la prueba de Monte Carlo nos da como resultado un p de 0.012, el
#cuál es  menor a nuestro nivel de significación de 0.05, quedando 
#P < alfa (0.012<0.05), por lo que se rechaza la hipótesis nula en favor 
#de la hipótesis alternativa, concluyendo con un 95% de confianza, de que 
#si existe una diferencia entre la cantidad de hijos que se tienen en 
#promedio en la Región de Tarapaca (Zona Norte) y la Región de Los Lagos 
#(Zona Sur).

#Pregunta 2:
#Propongan una pregunta de investigación original, que involucre la comparación 
# de las medias de más de dos grupos independientes (más abajo se dan unos 
# ejemplos). Fijando una semilla distinta a la anterior, seleccionen una muestra 
# aleatoria de hogares (400 < n < 600) y respondan la pregunta propuesta 
# utilizando bootstrapping. Solo por ejercicio académico, aplique un análisis 
# post-hoc con bootstrapping aunque este no sea necesario.


# Pregunta de investigación:
#A lo largo del país existen distintas familias con diferentes estilos 
# de vida cada una, por lo que se desea saber si en las distintas regiones 
# estos distintos estilos de vida ¿influyen en la cantidad de viviendas promedios 
# que poseen estas familias en un determinado sitio?. Cabe destacar que las 
# regiones seleccionadas corresponden a Región de Tarapacá (Zona Norte), 
# Región del Libertador Gral. Bernardo O’Higgins (Zona Centro) y Región de Los 
# Lagos (Zona Sur).

#Hipótesis:

#H0 = No existe diferencia en la cantidad promedio de viviendas que poseen las 
# familias en distintas regiones de Chile.
#H1 = Si existe una diferencia entre el promedio de viviendas que poseen 
# distintas familias en distintas regiones de Chile.

#Hipótesis matemática:
#UNV = Diferencia de la media de viviendas por sitio en las tres regiones elegidas.
#H0: UNV = 0
#HA: UNV != 0

#Se fija la semilla solicitada en el enunciado.
set.seed(800)

#Se asigna un alfa para la pregunta 2.
alfa2 <- 0.01
#Se selecciona una muestra aleatoria de hogares que se encuentren entre 
# 400 y 600.
datos2 <- sample_n(datos, 550)

#Se seleccionan los datos de los ciudadanos de la Región de Tarapacá.
RegionT <- datos2 %>% filter (region == "Región de Tarapacá")

#Se seleccionan los datos de los ciudadanos de la Región del Libertador Gral. 
# Bernardo O’Higgins.
RegionGBO <- datos2 %>% filter (region == 
                              "Región del Libertador Gral. Bernardo O’Higgins")

#Se seleccionan los datos de los ciudadanos de la Región de Los Lagos.
RegionLG <- datos2 %>% filter (region == "Región de Los Lagos")

#Para cada Región se obtienen los datos de estudio, que en este caso 
# corresponde a v8.

v8RegionT <- RegionT %>% select(id.vivienda, v8)
v8RegionGBO <- RegionGBO %>% select(id.vivienda, v8)
v8RegionLG <- RegionLG %>% select(id.vivienda, v8)

#Ahora se ve el largo de la fila.
n_v8RegionT<- nrow(v8RegionT)

n_v8RegionGBO <- nrow(v8RegionGBO)

n_v8RegionLG <- nrow(v8RegionLG)

#Los datos se pasan a vectores.
v8RegionTVector <- v8RegionT$v8

v8RegionGBOVector <- v8RegionGBO$v8

v8RegionLGVector <- v8RegionLG$v8

#Una vez obtenido los datos se procede a realizar la prueba de normalidad,
# ya que si aparece que los datos no siguen la normal se puede usar bootstrapping.

# Comprobar normalidad de las muestras .
print ( shapiro.test ( v8RegionTVector ) )
print ( shapiro.test ( v8RegionGBOVector ) )
print ( shapiro.test ( v8RegionLGVector ) )

#Se puede observar que los valores obtenidos por la prueba de shapiro.test,
# corresponden a 2.88e-07 para la Región de Tarapacá, 5.028e-10 para la Región
# Bernardo O´higgins y 1.157e-09 para la Región de Los Lagos, por lo que
# estos valores son mucho menores a nuestro nivel de significación de 0.01
# p-value < alfa, por lo que se puede concluir con un 99% de confianza que los 
# datos no siguen una distribución normal y se puede utilizar bootstrapping.

#Se obtienen los datos necesarios para cada par de regiones, en este caso
# se obtienen los datos de Región de Tarapacá y Bernardo O´higgins.
regionTyGBO <- c( rep ("Región de Tarapacá", n_v8RegionT) , 
        rep("Región del Libertador Gral. Bernardo O’Higgins", n_v8RegionGBO ) )
viviendasRegionTyGBO  <- c( v8RegionTVector , v8RegionGBOVector )
datosRegionTyGBO <- data.frame ( viviendasRegionTyGBO, regionTyGBO )
#Se obtienen los datos necesarios para cada par de regiones, en este caso
# se obtienen los datos de Región de Tarapacá y Los Lagos.
regionTyLG <- c( rep ("Región de Tarapacá", n_v8RegionT) , 
                 rep ("Región de Los Lagos", n_v8RegionLG))
viviendasRegionTyLG <- c( v8RegionTVector , v8RegionLGVector )
datosRegionTyGBO <- data.frame ( viviendasRegionTyLG, regionTyLG )
#Se obtienen los datos necesarios para cada par de regiones, en este caso
# se obtienen los datos de Región del Libertador Gral. Bernando
# O´higgins y Los Lagos.
regionGBOyLG <- c( rep ("Región del Libertador Gral. Bernardo O’Higgins", 
                  n_v8RegionGBO), rep ("Región de Los Lagos", n_v8RegionLG))
viviendasRegionGBOyLG <- c( v8RegionGBOVector , v8RegionLGVector )
datosRegionGBOyLG <- data.frame ( viviendasRegionGBOyLG, regionGBOyLG )

# Se obtienen las medias de cada Region para encontrar la diferencia
# observada entre los pares de regiónes
media_RegionT <- mean ( v8RegionTVector )
media_RegionGBO <- mean ( v8RegionGBOVector )
media_RegionLG <- mean ( v8RegionLGVector )

diferencia_observadaTyGBO <- media_RegionT - media_RegionGBO
diferencia_observadaTyLG <- media_RegionT - media_RegionLG
diferencia_observadaGBOyLG <-media_RegionGBO - media_RegionLG

#Fijar la cantidad B de repeticiones bootstraping.
B <- 9999
# Se crea la distribución bootstraping.
distribucion_bootstrap_TyGBO  <- two.boot( v8RegionTVector , v8RegionGBOVector ,
                                           FUN = mean , R = B )
distribucion_bootstrap_TyLG  <- two.boot( v8RegionTVector , v8RegionLGVector , 
                                          FUN = mean , R = B )
distribucion_bootstrap_GBOyLG <- two.boot( v8RegionGBOVector , v8RegionLGVector , 
                                           FUN = mean , R = B )
#Se asigna un valor nulo, en este caso como la hipótesis es igual a cero, se asigna
# el valor 0.
valor_nulo <- 0

#Una vez definido el valor nulo se procede a calcular el desplazamiento.
desplazamiento_TyGBO <- mean (distribucion_bootstrap_TyGBO[["t"]]) - valor_nulo
desplazamiento_TyLG <- mean (distribucion_bootstrap_TyLG[["t"]]) - valor_nulo
desplazamiento_GBOyLG <- mean (distribucion_bootstrap_GBOyLG[["t"]]) - valor_nulo

#Lo mismo para la distribución.
distribucion_nula_TyGBO <- distribucion_bootstrap_TyGBO[["t"]] - desplazamiento_TyGBO
distribucion_nula_TyLG <- distribucion_bootstrap_TyLG[["t"]] - desplazamiento_TyLG
distribucion_nula_GBOyLG <- distribucion_bootstrap_GBOyLG[["t"]] - desplazamiento_GBOyLG

#Luego al obtener todos estos datos se procede a calcular el valor p respecto a la comparación
# de los pares de regiones.

# Determinar el valor p_TyGBO.
p_TyGBO <- (sum(abs(distribucion_nula_TyGBO) > abs(diferencia_observadaTyGBO))+1)/(B+1)
cat (" Valor p_TyGBO:", p_TyGBO )

# Determinar el valor p_TyLG.
p_TyLG <- (sum(abs(distribucion_nula_TyLG) > abs(diferencia_observadaTyLG))+1)/(B+1)
cat (" Valor p_TyLG:", p_TyLG )

# Determinar el valor p_GBOyLG.
p_GBOyLG <- (sum(abs(distribucion_nula_GBOyLG) > abs(diferencia_observadaGBOyLG))+1)/(B+1)
cat (" Valor p_GBOyLG:", p_GBOyLG )

#Conclusión:
# Basándose en los resultados obtenidos, se puede realizar la 
# comparación entre el P y el nivel de significación (0.01), el cual nos da como 
# resultado que existen dos valores que son mayores a nuestro nivel de 
# significación. Para ser exactos el valor p_TyGBO (Tarapacá y General B.O´higgins)
# tiene un P de 0.9325 y para el valor p_TyLG (Tarapacá y Los Lagos) tiene un P
# de 0.0529, por lo que para este ejercicio se falla en rechazar la hipótesis nula 
# en favor de la hipótesis alternativa, por lo que se concluye con un 99% de 
# confianza que entre regiones del norte y sur de Chile no existe una diferencia 
# significativa entre la cantidad de viviendas por sitio, muy similar pasa con 
# las regiones de la zona norte y central, pero no así el caso entre las regiones 
# centrales y sur de Chile, ya que nos da un  valor p_GBOyLG de 0.0061 (menor a
# nivel de significación), por lo que se rechaza la hipótesis nula en favor de 
# la hipótesis alternativa, concluyendo con un 99% de confianza de que si existe 
# una diferencia entre el promedio de viviendas que poseen las regiones 
# centrales y del sur de Chile.


# Se procede a realizar una prueba Post-hoc

# Vector vacío para asignar el nombre de la Región abreviada. GBO =
# General Bernando O'higgins
mi_vector_GBO <- c()
# Llenando el vector con un bucle
for(i in 1:n_v8RegionGBO) {
  mi_vector_GBO[i] <- "Región GBO"
}
#Luego se crea un dataframe de la Región GBO, donde se tienen los datos de la región
# y su respectivo nombre.
v8RegionGBOconNombre <- cbind(v8RegionGBO,mi_vector_GBO)
dataFrameRegionGBO<- data.frame(v8RegionGBOconNombre)

# Vector vacío para asignar el nombre de la Región abreviada. T =
# Tarapacá.
mi_vector_T <- c()

# Llenando el vector con un bucle
for(i in 1:n_v8RegionT) {
  mi_vector_T[i] <- "Region T"
}
#Luego se crea un dataframe de la Región T, donde se tienen los datos de la región
# y su respectivo nombre.
v8RegionTconNombre <- cbind(v8RegionT,mi_vector_T)
dataFrameRegionT<- data.frame(v8RegionTconNombre)

# Vector vacío para asignar el nombre de la Región abreviada. LG =
# Los Lagos.
mi_vector_LG <- c()
# Llenando el vector con un bucle
for(i in 1:n_v8RegionLG) {
  mi_vector_LG[i] <- "Region LG"
}
#Luego se crea un dataframe de la Región LG, donde se tienen los datos de la región
# y su respectivo nombre.
v8RegionLGconNombre <- cbind(v8RegionLG,mi_vector_LG)
dataFrameRegionLG<- data.frame(v8RegionLGconNombre)

#Luego se unen las dos regiones de Tarapacá y General B. O'higgins.
dataFrameRegionTyGBO <- bind_rows(dataFrameRegionT, dataFrameRegionGBO)

#Por ultimo se une la región de Los Lagos
dataFrameTodasLasRegiones <- bind_rows(dataFrameRegionTyGBO , dataFrameRegionLG)


# Llevar data frame a formato largo .
datosEnDataFrame <- dataFrameTodasLasRegiones %>% pivot_longer(c("mi_vector_T", 
                                                                 "mi_vector_GBO", 
                                                                 "mi_vector_LG") ,
                                                              names_to = "Datos", 
                                                              values_to = "Región")
n_datosEnDataFrame <-nrow(datosEnDataFrame)
# # Vector vacío
mi_vector_id <- c()
for(i in 1:n_datosEnDataFrame) {
  mi_vector_id[i] <- i
}
datosEnDataFrame_conID <- cbind(datosEnDataFrame,mi_vector_id)
datosEnDataFrame_conID <- data.frame(datosEnDataFrame_conID)


# Procedimiento post-hoc HSD de Tukey .
mixto <- lme ( v8 ~ Datos , data = datosEnDataFrame_conID, random = ~1| mi_vector_id )
medias <- emmeans ( mixto , "Datos")
tukey <- pairs ( medias , adjust = "tukey")
print ( tukey )

#El resultado obtenido con la prueba Post-Hoc no ha sido el esperado.
# Como grupo se ha decidido ignorar el resultado obtenido, ya que puede
# existir un error en el cálculo o en algún dataframe creado, lo cual ha provocado
# que los resultados se encuentren erróneos. Concluir con datos equivocados
# sería un error garrafal, puesto que estos datos no serian los correctos que 
# representan a la población seleccionada. Es por este motivo que se ha decidido
# que la respuesta que responde nuestra pregunta de investigación corresponde 
# a la generada con los P value desde la línea 397 a la 411. Aun así se deja el
# desarrollo de la prueba Post Hoc para solicitar un feedback de cuál ha sido
# nuestro error en el desarrollo.

