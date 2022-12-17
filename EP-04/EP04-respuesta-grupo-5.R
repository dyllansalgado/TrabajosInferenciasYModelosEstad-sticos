

# Importar los paquetes, instalándolos de ser necesario.
if (!require(tidyverse)) {
  install.packages("tidyverse", dependencies = TRUE)
  require(tidyverse) }

if (!require(ggpubr)) {
  install.packages("ggpubr", dependencies = TRUE)
  require(ggpubr) }

# Se setea el workspace (Carpeta donde se encuentran los archivos)
# (Asegurarse de cambiar la ruta de la carpeta antes de ejecutar el script, y
# que el archivo csv esté en la carpeta).

# Lectura de archivos
setwd("C:\\Users\\Dyllan\\Desktop\\TrabajosIME\\EP-04")
datos <- read.csv2("EP04 datos.csv")


############################# ---- Pregunta 1 ---- #############################

cat("\n<> PREGUNTA 1\n\n")

#El Comité Olímpico cree que el mejor tiempo medio de los atletas blancos después 
#de ingresar al programa de entrenamiento es inferior a 12,87 segundos. ¿Soportan 
#los datos esta afirmación?

#Hipótesis Nula: El mejor tiempo medio de los atletas blancos después de
#ingresar al programa de entrenamiento es igual a 12,87 segundos.
# H0: u = 12,87

#Hipótesis Alternativa: El mejor tiempo medio de los atletas blancos después de
#ingresar al programa de entrenamiento es menor de 12,87 segundos.
# H1: u < 12,87


# Seleccionar los tiempos de los atletas blancos.
muestra_b <- datos %>% filter(Raza == "Blanca")

# Tamaño de la muestra.
n_1 <- nrow(muestra_b)
cat("n =", n_1, "\n")

grados_libertad <- n_1 - 1

# La muestra es de menos de 30 observaciones, por lo que se decide usar
# la prueba t de Student para una muestra.
# Ahora para utilizar la prueba t de Student se deben verificar 2 puntos impartes.
# 1. Las observaciones son independientes entre si.
# Este punto si se verifica, ya que los atletas son diferentes, ya que son
# representados por un ID.
# 2. Las observaciones provienen de una distribución cercana a la normal.

# Verificar si la distribución se acerca a la normal (por gráfico Q-Q).
grafico_1 <- ggqqplot(data = data.frame(muestra_b),
                      x = "Previo",
                      color = "steelblue",
                      xlab = "Teorico",
                      ylab = "Muestra",
                      title = "Gráfico Q-Q Muestra Atletas Blancos vs Distr. Normal")
print(grafico_1)

# Al ver el gráfico se puede observar que se asemeja bastante a la recta los
# puntos (observaciones), y se encuentran dentro del margen aceptable para
# suponer una normalidad en el conjunto de datos. Por lo que los dos puntos a
# comprobar son correctos.

# post_b corresponde a la muestra de los tiempos posteriores de los
# deportistas blancos.
post_b <- muestra_b$Posterior

# Ahora para comprobar nuevamente la normalidad aplicamos la prueba de Shapiro-Wilk.
normalidad_1 <- shapiro.test(post_b)
cat("\n> Comprobación de la normalidad de los datos:\n")
print(normalidad_1)

# Como el p-value es mucho mayor a nuestro nivel de significación, se puede suponer 
# que sigue una distribución normal

# Aplicar la prueba de T de Student para una muestra.

# Fijar un nivel de significación.
alfa_1 <- 0.025
cat("Nivel de significancia = ", alfa_1, "\n")
# Se asigna el valor nulo.
valor_nulo_1 <- 12.87
cat("Valor nulo =", valor_nulo_1, "\n")

# Calcular el estadístico de prueba.
cat("\n> Prueba t para una muestra\n\n")

media_1 <- mean(post_b)
cat("Media =", media_1, "\n")

error_1 <- sd(post_b) / sqrt(n_1)
cat("Error estándar =", error_1, "\n")

# Al tener todos los valores anteriores, se procede a calcular el estadistico t
# de manera "manual".
t_1 <- (media_1 - valor_nulo_1) / error_1
cat("t =", t_1, "\n")

# Se calcula el valor p, con la función pt.
p <- pt(t_1, df = grados_libertad, lower.tail = TRUE)
cat("p =", p, "\n")

# Construir el intervalo de confianza.
t_critico_1 <- qt(alfa_1, df = grados_libertad, lower.tail = FALSE)
superior_1 <- media_1 + t_critico_1 * error_1
cat("\nIntervalo de confianza = (-Inf, ", superior_1, "]\n\n", sep = "")


# Aplicar la prueba de T de Student para una muestra (función t.test de R).
cat("\n> Prueba t para una muestra (función t.test en R)\n")
prueba_t_1 <- t.test(post_b,
                     alternative = "less",
                     mu = valor_nulo_1,
                     conf.level = 1 - alfa_1)
print(prueba_t_1)

############################ ---- Respuesta 1 ---- ############################

# En este caso, p > α (0.4905655 > 0.025), por lo que la evidencia no es
# suficientemente fuerte como para rechazar la hipótesis nula.

# Se concluye con un 97.5% de confianza que, tras el entrenamiento, el mejor
# tiempo medio de los atletas blancos después de ingresar al programa de
# entrenamiento pareciera ser 12,87 segundos (12.86105).

############################# ---- Pregunta 2 ---- #############################

cat("\n<> PREGUNTA 2\n\n")

# ¿Sugieren los datos que la mejor marca de los atletas orientales se reduce en
# más de 5,27 segundos tras el entrenamiento?

# Hipótesis Nula: Tras el entrenamiento, la media de las diferencias en las
# marcas de los atletas orientales es igual a 5,27 segundos.
# H0 udif = 5,27

# Hipótesis Alternativa: Tras el entrenamiento, la media de las diferencias en
# las marcas de los atletas orientales se reduce en más de 5,27 segundos.
# H1: udif > 5,27


# Se infiere sobre la media de dos muestras pareadas.

muestra_o <- datos %>% filter(Raza == "Oriental")

prev_o <- muestra_o$Previo
post_o <- muestra_o$Posterior

# Como las muestras son pareadas, basta con encontrar el largo de solo una muestra.
n_2 <- length(prev_o)
cat("n = ", n_2, "\n")

# Las muestras son de menos de 30 observaciones, por lo que se decide usar
# la prueba t de Student para dos muestras pareadas.
# Ahora para utilizar la prueba t de Student se deben verificar 2 puntos importantes.
# 1. Las observaciones son independientes entre sí.
# Este punto si se verifica, ya que los atletas son diferentes, ya que son
# representados # por un ID.
# 2. Las observaciones provienen de una distribución cercana a la normal.
# Verificar si la distribución se acerca a la normal.

# Verificar si la distribución se acerca a la normal (por gráfico Q-Q).
diferencia <- prev_o - post_o
g2 <- ggqqplot(data.frame(diferencia), x = "diferencia", color = "red")
print(g2)

# Ahora para comprobar nuevamente la normalidad aplicamos la prueba de Shapiro-Wilk.
normalidad_2 <- shapiro.test(diferencia)
cat("\n> Comprobación de la normalidad de los datos:\n")
print(normalidad_2)

# Como el p-value es mucho mayor a nuestro nivel de significación, se puede suponer 
# que sigue una distribución normal.

# Fijar un nivel de significación.
alfa_2 <- 0.05
cat("Nivel de significancia = ", alfa_2, "\n")

# Aplicar la prueba t de Student a la diferencia de medias
valor_nulo_2 <- 5.27

prueba_t_2 <- t.test(x = prev_o,
                     y = post_o,
                     paired = TRUE,
                     alternative = "greater",
                     mu = valor_nulo_2,
                     conf.level = 1 - alfa_2)
print(prueba_t_2)

############################ ---- Respuesta 2 ---- ############################

# En este caso, p > α (0.1728 > 0.05), por lo que la evidencia no es
# suficientemente fuerte como para rechazar la hipótesis nula.

# Se concluye con un 95% de confianza que, tras el entrenamiento, la media de las
# diferencias en las marcas de los atletas orientales pareciera ser igual a 5,27 segundos.
############################# ---- Pregunta 3 ---- #############################

cat("\n<> PREGUNTA 3\n\n")

# ¿Es posible afirmar que, en promedio, los atletas negros superan a los
# blancos por 0,76 segundos antes del entrenamiento?

# Hipótesis Nula: Tras el entrenamiento, los atletas negros igualan a los
# blancos, en promedio, por 0,76 segundos, antes del entrenamiento.
# H0: u = 0,76

# Hipótesis Alternativa: Tras el entrenamiento, los atletas negros superan a los
# blancos, en promedio, por 0,76 segundos, antes del entrenamiento.
# H1: u > 0,76

# Se infiere sobre la media de dos muestras independientes

muestra_n <- datos %>% filter(Raza == "Negra")

prev_n <- muestra_n$Previo
prev_b <- muestra_b$Previo

# Tamaño de las muestras.
n_3_a <- length(prev_n)
n_3_b <- length(prev_b)
cat("n1 (negros) = ", n_3_a, "\nn2 (blancos) = ", n_3_b, "\n")

# Las muestras son de menos de 30 observaciones, por lo que se decide usar
# la prueba t de Student para dos muestras pareadas.
# Ahora para utilizar la prueba t de Student se deben verificar 2 puntosimportantes.
# 1. Las observaciones son independientes entre sí.
# Este punto si se verifica, ya que los atletas son diferentes, ya que son
# representados por un ID.
# 2. Las observaciones provienen de una distribución cercana a la normal.
# Verificar si la distribución se acerca a la normal.

# Para comprobar la normalidad de las muestras aplicamos la prueba de Shapiro-Wilk.
normalidad_3a <- shapiro.test(prev_n)
cat("\n> Comprobación de la normalidad de los datos muestra A:\n")
print(normalidad_3a)
normalidad_3b <- shapiro.test(prev_b)
cat("\n> Comprobación de la normalidad de los datos muestra B:\n")
print(normalidad_3b)

# Como el p-value es mucho mayor a nuestro nivel de significación, se puede suponer 
# que sigue una distribución normal.

# Fijar un nivel de significación.
alfa_3 <- 0.05
cat("Nivel de significancia = ", alfa_3, "\n")

# Aplicar la prueba t de Student para dos muestras independientes.
valor_nulo_3 <- 0.76

prueba_t_3 <- t.test(x = prev_b,
                     y = prev_n,
                     alternative = "greater",
                     mu = valor_nulo_3,
                     paired = FALSE,
                     conf.level = 1 - alfa_3)

cat("Prueba de hipótesis:\n")
print(prueba_t_3)

# Calcular la diferencia entre las medias
media_b <- mean(prev_b)
media_n <- mean(prev_n)
diferencia_b_n <- media_b - media_n

cat("Diferencia de las medias =", abs(diferencia_b_n) , "\n")

############################ ---- Respuesta 3 ---- ############################

# En este caso, p < α (0.01665 < 0.05), por lo que se rechaza la hipótesis nula
# en favor de la hipótesis alternativa.

# La diferencia de las medias es 1.899611, valor que es mayor a los 0.76 de la
# hipótesis nula, algo que apoya la hipótesis alternativa.

# Se concluye con un 95% de confianza que, tras el entrenamiento, los atletas
# negros superan a los blancos, en promedio, por más de 0,76 segundos, antes
# del entrenamiento.