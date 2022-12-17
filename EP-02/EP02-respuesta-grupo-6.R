

# Pregunta número 6, respondida con análisis de los gráficos.
# ¿Van los ingresos de las mujeres de la RM incrementándose con la edad?
# Respuesta:

# Depende. Si se analiza todos los datos en conjunto, es decir, nos 
# enfocamos en las mujeres por edad y que viven en zona rural o urbana, se ve
# claramente que el aumento de ingresos va desde los 18 años aproximadamente hasta
# llegar al punto más alto, que corresponde a los 45 años aprox. y luego los ingresos
# comienzan a decaer. Podemos considerar un gráfico con las medianas, tomándolas como
# un mejor estadístico descriptivo, ya que no toma los valores atípicos, y ocurre el mismo
# fenómeno.

# Ahora, se enfocará en analizar los gráficos creados para un grupo de mujeres 
# asalariadas o en situación de empleador, con la distinción que uno de los gráficos  
# representa a mujeres que viven solamente en la zona urbana y el segundo en la zona rural. 
# Si analizamos a las mujeres que se encuentran en la 
# zona urbana, se puede decir que por cierto tramo 
# de los 18-45 años se ve una alza de sus ingresos,
# y esto es algo natural dado que a medida que uno crece
# en edad comienza a trabajar y a conseguir mejores oportunidades
# laborales, esta alza se ve en decadencia luego de alcanzar
# lo que se considera como el segundo tramo, que corresponde
# aproximadamente a las edades de 45-60 años para finalmente
# tener un ingreso con tendencia constante llegando a la edad
# de jubilación (dado que se espera que solo se viva de la
# pensión para ese rango etario). Pero si se analiza el gráfico de
# las mujeres que viven en una zona rural, no es posible
# relacionar algún incremento en sus ingresos a medida
# que avanzan en edad y esto puede ser dado que en las
# zonas rurales no se encuentran las mismas oportunidades
# laborales que se encuentran en las zonas urbanas, concatenando
# en que exista una mayor dispersión de ingresos. 
# Por lo tanto, si bien es cierto en el primer gráfico de 
# mujeres urbanas si se puede apreciar un incremento de sus 
# ingresos junto con la edad, esto no es apreciable para el 
# segundo caso de las mujeres rurales. 


#Discutir y consensuar qué medidas estadísticas (media, mediana, moda, etc.) y qué forma 
#gráfica ayudaría a responder la pregunta asignada.
#Respuestas:

# Las medidas estadísticas que nos permiten responder la pregunta asignada podría ser la 
# media, pero luego de analizar la gráfica resultante, nos hemos percatado que contenía 
# valores que se alejaban demasiado del rango de ingreso recibido con sus pares (Mujer 97 
# años tiene un ingreso de $880.000, pero sus pares no superan los $350.000 ) por lo que 
# al calcular la media de ingresos recibidos por esta edad, provoca que se genere un rango 
# atípico, ya que este cálculo no es representativo para su edad, por lo tanto, decidimos 
# utilizar la mediana dado que esta nos ayuda a normalizar nuestra gráfica y disminuir los 
# errores de observación que denotan la presencia de estos datos atípicos.

# La gráfica que nos servirá para responder la pregunta asignada corresponde al gráfico de 
# dispersión, ya que estos gráficos se caracterizan por mostrar información caso a caso y es 
# perfecto para representar las medias o medianas de cada edad, además al representar
# gráficamente todos los puntos, permiten identificar posibles asociaciones entre las
# variables involucradas, y sumado al tipo de escala de las variables (escala proporcional),
# facilita comparar los intervalos o las diferencias entre variables .

#Mencionan explícitamente todas las variables presentes en el archivo CSV que son 
#relevantes para responder la pregunta asignada, reconociendo correctamente sus tipos. 

#Respuestas:
# Las variables que son relevantes para responder la pregunta asignada, son:
# ch1: Es una variable de tipo categórica nominal.
# edad: En este estudio corresponde a una variable categórica ordinal, ya que es utilizada 
# para agrupar solamente por edades.
# sexo: Es una variable de tipo categórica nominal.
# zona: Es una variable de tipo categórica nominal.
# ytot: Es una variable de tipo numérica discreta, dado que en Chile no existen valores 
# decimales en el CLP, si el ytot se trabajara en USD será numérica continua.


############################################
# Se setea el workspace (Carpeta donde se encuentran los archivos)
# (Asegurarse de cambiar la ruta de la carpeta antes de ejecutar el script, y que el archivo 
# csv esté en la carpeta).
setwd("C:\\Users\\Dyllan\\Desktop\\TrabajosIME\\EP-02")
############################################
# Importar los paquetes, instalándolos de ser necesario.
if (!require(tidyverse)) {
  install.packages("tidyverse", dependencies = TRUE)
  require(tidyverse) }

if (!require(ggpubr)) {
  install.packages("ggpubr", dependencies = TRUE)
  require(ggpubr) }

# Leer un dataframe desde archivo csv.
datos <- read.csv2("EP02 Datos Casen 2017.csv", stringsAsFactors=FALSE)

############################################
# Tabla con las variables consideradas relevantes para la experiencia, datos de las mujeres de la RM ordenados por edad
is_m_xedad <- datos %>% filter(sexo == "Mujer") %>% group_by(edad) %>% summarise(edad, zona, ocupacion = ch1, ingresos_totales = ytot)

# Tabla con las medias y medianas de los ingresos de mujeres de zonas urbanas de la RM que trabajan, agrupados por edad
ing_m_t_urb_xedad <- is_m_xedad %>% filter(zona == "Urbano", ocupacion == "Asalariado" | ocupacion == "Patrón o empleador/ Trabajador Cuenta Propia") %>% summarise(promedio_ingresos = mean(ingresos_totales), mediana_ingresos = median(ingresos_totales))

# Tabla con las medias y medianas de los ingresos de mujeres de zonas rurales de la RM que trabajan, agrupados por edad
ing_m_t_rul_xedad <- is_m_xedad %>% filter(zona == "Rural", ocupacion == "Asalariado" | ocupacion == "Patrón o empleador/ Trabajador Cuenta Propia") %>% summarise(promedio_ingresos = mean(ingresos_totales), mediana_ingresos = median(ingresos_totales))

############################################
# Tabla con las medias y medianas de los ingresos de mujeres de la RM, agrupados por edad
ing_m_xedad <- datos %>% filter(sexo == "Mujer") %>% group_by(edad) %>% summarise(promedio_ingresos = mean(ytot), mediana_ingresos = median(ytot))

############################################

#Gráficos

# Gráfico de dispersión para los promedios de los ingresos de mujeres en la RM, por edad
g_promedios <- ggscatter(ing_m_xedad,
                         size = 1,
                         x = "edad",
                         y = "promedio_ingresos",
                         title = "Promedio ingresos mujeres por edad",
                         xlab = "Edad [años]",
                         ylab = "Promedio Ingresos [CLP]")
print(g_promedios)

# Gráfico de dispersión para las medianas de los ingresos de mujeres en la RM, por edad
g_medianas <- ggscatter(ing_m_xedad,
                        size = 1,
                        x = "edad",
                        y = "mediana_ingresos",
                        title = "Mediana ingresos mujeres por edad",
                        xlab = "Edad [años]",
                        ylab = "Mediana Ingresos [CLP]")
print(g_medianas)

# Gráfico de dispersión para las medianas de los ingresos de mujeres de zonas urbanas
# con ingreso remunerado (trabajando), por edad, en la RM
g_urb <- ggscatter(ing_m_t_urb_xedad,
                   size = 1,
                   x = "edad",
                   y = "mediana_ingresos",
                   title = "Mediana ingresos mujeres zona urbana trabajando por edad",
                   xlab = "Edad [años]",
                   ylab = "Mediana Ingresos [CLP]",
                   add = "reg.line",
                   add.params = list(color = "blue",fill = "lightgray"))
print(g_urb)

# Gráfico de dispersión para las medianas de los ingresos de mujeres de zonas rurales
# con ingreso remunerado (trabajando), por edad, en la RM
g_rul <- ggscatter(ing_m_t_rul_xedad,
                   size = 1,
                   x = "edad",
                   y = "mediana_ingresos",
                   title = "Mediana ingresos mujeres zona rural trabajando por edad",
                   xlab = "Edad [años]",
                   ylab = "Mediana Ingresos [CLP]",
                   add = "reg.line",
                   add.params = list(color = "blue",fill = "lightgray"))
print(g_rul)
