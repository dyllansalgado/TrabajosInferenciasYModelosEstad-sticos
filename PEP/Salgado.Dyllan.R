# Importar los paquetes, instalándolos de ser necesario.
if (!require(tidyverse)) {
  install.packages("tidyverse", dependencies = TRUE)
  require(tidyverse) }

if (!require(ggpubr)) {
  install.packages("ggpubr", dependencies = TRUE)
  require(ggpubr) }

if (!require(dplyr)) {
  install.packages("dplyr", dependencies = TRUE)
  require(dplyr) }

if (!require(pwr)) {
  install.packages("pwr", dependencies = TRUE)
  require(pwr) }

#COMANDOS: %>% <-

#Designamos una semilla aleatoria
set.seed(203)
#Se designa un alfa al inicio del problema para luego ser contrastada
#en los resultados obtenidos
alfa <- 0.05

#Enunciado:
#Un equipo de nutricionistas desea estudiar la efectividad de un nuevo
#programa para bajar de peso. Para ello, han seleccionado de manera
#aleatoria a 250 hombres y 250 mujeres de entre más de un millon de pacientes
#que han completado el programa y han recolectado los siguientes datos:
#Se setea la direccion de la carpeta.
dir <- setwd("C:\\Users\\Dyllan\\Desktop\\TrabajosIME\\PEP")
#Se leen los datos y se guardan.
datos <- read.csv2("PEP1 Datos.csv", stringsAsFactors=FALSE)

#Preguntas: 
#El equipo de nutricionestas le ha solicitado responder las siguientes preguntas:

#a) ¿Es posible afirmar que los hombres pierden, en promedio, más
# de 5kg tras completar el programa?

#Respuesta:


#Al leer la pregunta se pueden establecer las hipotesis:

#H0: Los hombres pierden en promedio 5[KG] al completar el programa.
#HA: Los hombres pierden en promedio más de 5[KG] al completar el programa.

#Matematicamente hablando las hipotesis quedan:

#H0: H_antes-H-despues = 0
#HA: H_antes-H-despues > 5[KG]

#Se debe inferir respecto a la media de dos muestras pareadas.
#Se seleccionan los hombres
hombres <- datos %>% filter(Sexo == "M")
#Luego se selecciona los hombres y sus pesos antes.
hombres_antes <- hombres$Peso_antes
#Y sus pesos despues.
hombres_despues <- hombres$Peso_despues
#Como las muestras son pareadas basta con encontrar el largo solo de una muestra
largo_muestra <-length(hombres_antes)
#Se muestra en la consola el tamaño.
cat("tamaño muestra",largo_muestra)

#Ahora que se tienen los datos se debe ver las condiciones para 
#ver que prueba utilizar.

# La muestra es de más de 30 observaciones, por lo que se decide usar
# la prueba t de Student para una muestra, ya que nos dice explicitamente
# que es "igualmente adecuada cuando la muestra es grande"

# Ahora para utilizar la prueba t de Student se deben verificar 2 puntos impartes.
# 1. Las observaciones son independientes entre si.
# Este punto si se verifica, ya que los hombres seleccionados tienen un ID unico y 
# fueron seleccionados de manera aleatoria.
# 2. Las observaciones provienen de una distribución cercana a la normal.

#Para verificar el punto 2, se realizará el grafico Q-Q y la
# prueba de shapiro.test

diferencia <- hombres_antes-hombres_despues

graficoQQ <- ggqqplot(data.frame(diferencia),x = "diferencia",color= "blue")

print(graficoQQ)

#Al analizar el grafico se puede apreciar que existen muy pocos valores atipicos,
#pero la mayor cantidad de valores sigue una tendencia a la normal.
#De igualmanera se comprobará con la prueba de shapiro.test

normalidad <- shapiro.test(diferencia)

print(normalidad)

#Como nuestro p-value es mucho mayor a nuestro nivel de significación,
# se puede suponer que sigue una distribución normal.

#Ahora que se han verificado los dos requisitos, se procede a 
#utilizar la prueba T de Student

#Se establece el valor nulo que corresponde a 5[KG]

valor_nulo <- 5
#En el alternative se coloca Greater porque la hipotesis alternativa
# corresponde a mayor a.
prueba_t_student <- t.test(x= hombres_antes,
                           y= hombres_despues,
                           paired = TRUE,
                           alternative = "greater",
                           mu= valor_nulo,
                           conf.level = 1-alfa)

print(prueba_t_student)
#cambiar alternative:
#En este caso p < alfa, por lo que se rechaza la hipótesis nula en favor
# de la hipótesis alternativa, por lo que se puede concluir con un 95% 
# confianza que los hombres pierden en promedio más de 5[KG] 
# al completar el programa.


#Pregunta 2:
#¿Que poder estadistico tiene la prueba realizada?
#Para calcular el poder estadistico se deben saber algunos datos
# y se utilizara la siguiente funcion: pwr.t.test

media_hombres_antes <- mean(hombres_antes)
media_hombres_despues<-mean(hombres_despues)

media_agrupada <- (media_hombres_antes+media_hombres_despues)/500
desviacion_estandar_datos_agrupados <- sd(media_agrupada)

resultado <-pwr.t.test(n=largo_muestra,
                       sd= desviacion_estandar_datos_agrupados,
                       sig.level = 0.05,
                       power= NULL,
                       type = "paired",
                       alternative = "two.sided")
print(resultado)


#Pregunta 3:
#¿Cuantos pacientes se debería monitorear para detectar una diferencia de 
# 5[KG] con un poder estadistico del 80%y una confianza del 99%?

poder_estadistico <- 0.80
nivel_de_significacion <- 0.01

media_2  <- 5

#Se procede a utilizar la d de cohen:

d_cohen <- (media_2-media_agrupada)/desviacion_estandar_datos_agrupados

#Como se quiere buscar el valor de n O LA CANTIDAD DE PACIENTES A MONITOREAR N RECIBE EL NULL
resultado2 <- pwr.t.test(n=NULL,
                        sd= desviacion_estandar_datos_agrupados,
                        sig.level = 0.05,
                        power= poder,
                        type = "paired",
                        alternative = "two.sided")
print(resultado)