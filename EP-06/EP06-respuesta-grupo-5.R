

# Importar los paquetes, instalándolos de ser necesario.

if (!require(Hmisc)) {
  install.packages("Hmisc", dependencies = TRUE)
  require(Hmisc) }

#Se crea la tabla con los datos.

Especialidad <- c("Pediatría","Obstetricia","Dermatología","Psiquiatría","Medicina interna",
                  "Oncología","Neurología","Anestesiología","Radiología")

Mujeres <- c(54,71,35,30,45,
             44,56,21,17)

Hombres <- c(52,66,41,42,65,
             62,88,40,35)

#Se crea el dataframe con los datos.

datos <- data.frame(Especialidad,
                    Mujeres,
                    Hombres,
                    stringsAsFactors = FALSE)

####  PREGUNTA 1 ####
# Estudios previos habían determinado que la proporción de autoras 
# en la especialidad de medicina interna era de 42%. 
# ¿Respaldan estos datos tal estimación?

# Como se habla de proporción se puede utilizar el método de Wald o Wilson,
# en este caso se usará el método de Wilson por el motivo que el método de 
# Wald hoy en día es muy criticado por las importantes simplificaciones que 
# este realiza.

# Valores conocidos:
Especialidad <- datos[5,"Especialidad"]
mujeres <- datos[5,"Mujeres"]
hombres <- datos[5,"Hombres"]
tamaño <- mujeres+hombres
valor_nulo <- 0.42
# Se establece un nivel de significación de 0.05
alfa <- 0.05

# Comprobando usabilidad de método de Wilson:
# 1: Se comprueba que los datos son independientes, ya que los datos 
# corresponden a autoras y autores de artículos científico.
# 2: Éxitos y fracasos deben ser superior a 10.
# Se calculan los éxitos a partir de valor_nulo:
exitos <- valor_nulo*tamaño
# Se calculan los fracasos a partir de valor_nulo n(1 − p):
fracasos <- tamaño*(1-valor_nulo)
# El punto dos también se comprueba,ya que el éxito da: 46.2 y fracaso 63.8,
# por lo que se puede utilizar el método de Wilson.

# Al tener los valores conocidos se procede a realizar las hipótesis.
# H0: La proporción de autoras en la especialidad de medicina interna es de 42%
# HA: La proporción de autoras en la especialidad de medicina interna es distinta de 42%

# Viéndolo de forma matemática queda:
# H0: propMedicina = 42% o 0.42
# HA: propMedicina != 42% o 0.42

# Prueba de Wilson en R.
prueba <- prop.test(mujeres, n = tamaño, p = valor_nulo,
                    alternative = "two.sided", conf.level = 1-alfa)

print(prueba)

# El valor p obtenido corresponde a 0.89, que es mayor al nivel de significación de 0.05, 
# por lo que la evidencia no es suficiente para rechazar la hipótesis nula, 
# determinando con un 95% de confianza que la proporción de autoras de la especialidad de 
# Medicina Interna no es diferente de un 42% 

####  PREGUNTA 2 ####

#Según estos datos, ¿es igual la proporción de autoras en las áreas de obstetricia y radiología?

#Nos están solicitando la diferencia entre proporciones de autoras del área 
#de obstetricia y radiología por lo que se puede usar nuevamente Wald o #Wilson pero con dos proporciones.
#En este caso, al igual que la pregunta 1 se elige Wilson, provocando que el #desarrollo sea muy similar al ejercicio 1.

# Hipótesis:
# H0: La tasa de proporción de autoras en las áreas de obstetricia y radiología es igual.
# HA: La tasa de proporción de autoras en las áreas de obstetricia y radiología es diferente.

# Viéndolo de forma matemática queda:
# H0: Pobst - Pradio = 0
# HA: Pobst - Pradio != 0

#Se obtienen los datos
Especialidad_2 <- datos[2,"Especialidad"]
mujeres_obstetricia <- datos[2,"Mujeres"]
hombres_obstetricia <- datos[2,"Hombres"]
tamaño_obstetricia <- mujeres_obstetricia + hombres_obstetricia

Especialidad_3 <- datos[9,"Especialidad"]
mujeres_radiologia <- datos[9,"Mujeres"]
hombres_radiologia <- datos[9,"Hombres"]
tamaño_radiologia<- mujeres_radiologia + hombres_radiologia


# Comprobando usabilidad de método de Wilson:
# 1: Se comprueba que los datos son independientes, ya que los datos 
# corresponden a autoras y autores de artículos científico.
# 2: Éxitos y fracasos deben ser superior a 10.
# También se comprueba, ya que los éxitos son las mujeres que en este caso 
# para obstetricia y radiología son de 71 y 17, y los hombres que son los 
# fracasos son 66 y 35

#Se fijan los valores conocidos.
n <- c(tamaño_obstetricia,tamaño_radiologia)
exitos_2 <- c(mujeres_obstetricia,mujeres_radiologia)
alfa <- 0.05

# Prueba de Wilson en R.
prueba2 <- prop.test (exitos_2 , n = n , alternative = "two.sided",
                      conf.level = 1 - alfa )

print(prueba2)

# El valor p obtenido corresponde a 0.028, que es menor al nivel de 
# significación de 0.05, por lo que se rechaza la hipótesis nula en favor 
# de la hipótesis alternativa. 
# Se concluye con un 95% de confianza que, la tasa de proporción de autoras 
# en el área de obstetricia y radiología difiere entre ellas.


####  PREGUNTA 3 ####
# Suponiendo que la diferencia en la proporción de autoras en la especialidad de 
# anestesiología y la de pediatría es de 0,28. ¿A cuántos autores (hombres y mujeres) 
# deberíamos monitorear para obtener un intervalo de confianza del 95% y poder 
# estadístico de 80%, si se intenta mantener aproximadamente la  misma proporción de 
# gente estudiada en cada caso?

# Hipótesis:
# H0: La diferencia de tasa de proporción entre autoras de áreas de anestesiología y   
# pediatría es de 0.28
# HA: La diferencia de tasa de proporción entre autoras de áreas de anestesiología y   
# pediatría es diferente de 0.28

# Viéndolo de forma matemática queda:
# H0: Panest - Ppedia = 0.28
# HA: Panest - Ppedia != 0.28

# Datos conocidos
alfa <- 0.05
poder <- 0.8
diferencia <- 0.28

# Primero debemos calcular las proporciones observadas

# Tamaños de las muestras
n_anestesiologia <- datos[8,"Mujeres"] + datos[8,"Hombres"]
n_pediatria <- datos[1,"Mujeres"] + datos[1,"Hombres"]

fraccion <- n_anestesiologia / (n_anestesiologia + n_pediatria)

# Proporciones
p_anestesiologia <- datos[8,"Mujeres"]/n_anestesiologia
p_pediatria <- datos[1,"Mujeres"] / n_pediatria

diferencia_observada <- abs(p_anestesiologia - p_pediatria)

cat('La diferencia de las proporciones observadas es ', diferencia_observada, '\n\n')
# Vemos que la diferencia de las proporciones observadas es 0.1651717. 

# Necesitamos hacer un ajuste de modo que la diferencia de las proporciones tenga el 
# valor mismo que el del enunciado, es decir, 0.28. Para esto realizamos la siguiente
# operación:

p_anesNueva <- (p_anestesiologia+p_pediatria)/2 - diferencia/2
p_pediaNueva <- (p_anestesiologia+p_pediatria)/2 + diferencia/2

diferencia_esperada <- abs(p_anesNueva - p_pediaNueva)

cat('Una vez ajustadas las proporciones, comprobamos que ahora la disferencia de\nlas proporciones esperadas es ', diferencia_esperada, '\n\n')

# Usamos la prueba de Wilson para calcular el tamaño de las muestras
resultado <- bsamsize(p_anesNueva, p_pediaNueva, fraccion, alfa, poder)

print(resultado)

# Mostramos los valores enteros aproximados
cat('\nSe deben monitorear a', ceiling(resultado[1]), 'y', ceiling(resultado[2]), "autores para anestesiología y pediatría respectivamente")

# Para mantener la misma proporción con un intervalo de confianza del 95% y un poder
# Estadístico del 80% es necesario monitorear a 38 autores de anestesiología y 66
# autores de pediatría.