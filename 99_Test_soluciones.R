# Soluciones a los Test incluidos en el curso
# Nota: Se omiten tildes

#===============================================================================
# CURSO:   INICIACION A R PARA EL ANALISIS ESTADISTICO
# SCRIPT:  1. Breve introduccion a R
# AUTORES: Miguel A. Sorrel
# VERSION:   3.0
#===============================================================================

#vectores
# 1) Establece una semilla de aleatorizacion con valor 473 y despues crea un
# vector con nombre v que contenga 100 numeros generados aleatoriamente desde 
# una distribucion uniforme con minimo igual a 1 y maximo igual a 5. Responde a
# las siguientes cuestiones:
set.seed(473)
v <- runif(100, min = 1, max = 5)
#     a) Cual es el valor correspondiente al elemento numero 23 de v? [Sol: 2.259835]
v[23]
#     b) Cual es la media del vector v? [Sol: 2.890052]
mean(v)
#     c) Cuantos valores de v son menores a 2? [Sol: 25]
sum(v < 2)

# 2) Ahora, tras establecer una nueva semilla de aleatorizacion con valor
# 1274, extrae 100 numeros enteros aleatorios entre 1 y 5 (con remuestreo):
set.seed(1274)
w <- sample(x = 1:5, size = 100, replace = TRUE)
#     a) Cuantos elementos son iguales a 2? [Sol: 15]
table(w); sum(w == 2)
#     b) Cual es la varianza de v cuando w es mayor o igual a 3? [Sol: 1.323919]
var(v[w >= 3])

#dataframes
# 1) La base de datos incluida en R de nombre iris (?iris) contiene 
# informacion sobre las caracteristicas morfologicas de distintos tipos de 
# flores de la familia iris.
#     a) ¿Cuantas filas tiene la base de datos? [Sol: 150]
nrow(iris)
#     b) ¿Cuales es el nombre de la primera variable? [Sol: Sepal.Length]
names(iris)[1]
#     c) ¿Cuanto mide el petalo de la flor numero 33? [Sol: 1.5]
iris$Petal.Length[33]
#     d) ¿Cual es el promedio de la anchura del sepalo para la especie setosa? [Sol: 3.428]
aggregate(iris$Sepal.Width, by = list(iris$Species), mean)
#     e) ¿Cuantas plantas de la especie versicolor existen con una anchura del petalo mayor a 1cm? [Sol: 43]
table(iris$Species[iris$Petal.Width > 1])
#     f) ¿Crea una nueva variable que representen el area del sepalo. ¿Cual es la media de esta variables? [Sol: 17.82287]
iris$Sepal.Area <- iris$Sepal.Length * iris$Sepal.Width
mean(iris$Sepal.Area)

#===============================================================================
# CURSO:   INICIACION A R PARA EL ANALISIS ESTADISTICO
# SCRIPT:  2. Importacion y exportacion de datos en R
# AUTORES: Miguel A. Sorrel
# VERSION:   3.0
#===============================================================================

# 1) Realiza una serie de ejercicios con la base de datos "Gapminder.xlsx":
library(gapminder)
#     a) Cuál es el último año para el que se disponen datos? [sol. = 2007]
table(gapminder$year)
#     b) Cuantos paises estan representados en la base de datos? [sol. = 142]
length(table(gapminder$country))
#     c) Cuantos paises de Oceanía estan representados en la base de datos? [sol. = 2]
table(gapminder$continent)["Oceania"]/length(table(gapminder$year))
#     d) Cuál es el máximo de la variable lifeExp? [sol. = 82.6]
describe(gapminder$lifeExp)
#     e) Cuál es valor de la correlación entre lifeExp y gdpPercap? [sol. = 0.58]
corPlot(sapply(dat, as.numeric))

#===============================================================================
# CURSO:   INICIACION A R PARA EL ANALISIS ESTADISTICO
# SCRIPT:  3. Analisis de datos en R
# AUTORES: Miguel A. Sorrel
# VERSION:   3.0
#===============================================================================

# 1) Realiza una serie de ejercicios con la base de datos "speed_dating.csv":
speed_dating <- read.csv("speed_dating.csv", header = TRUE, stringsAsFactors = TRUE)
#     a) La variable atractivo "attr", ¿se distribute normalmente? Ten en cuenta que 
#        la función a utilizar no admite valores perdidos. Tendrás que usar complete.cases(). [Sol: No]
sd.completo <- speed_dating[complete.cases(speed_dating), ]
ks.test(sd.completo$attr, pnorm, mean(sd.completo$attr, sd(sd.completo$attr)))
#     b) ¿Qué proporción de mujeres (gender = 0) tenían como objetivo conocer gente (goal = 2)? [Sol: = 0.39927273]
prop.table(table(speed_dating$goal, speed_dating$gender), margin = 2)
#     c) ¿Hay diferencias significativas en el promedio de atractivo según género? (responde Sí/No) # [Sol: Sí]
t.test(speed_dating$attr ~ speed_dating$gender,
       alternative = "two.sided",
       conf.level = 0.95) 
#     d) ¿Cuál es la correlación entre atractivo (attr) e inteligencia (intel)? [Sol: 0.3910644]
cor(speed_dating$attr, speed_dating$intel, use = "complete.obs")
#     e) Ajusta el modelo de regresión logística empleando sólo los datos de mujeres (gender == 1). 
#        ¿Cuál de los tres predictores (attr, intel o prob) tiene asociado un coeficiente de regresión más grande? [Sol: attr]
model_m <- glm(dec ~ attr + intel + prob, 
               data = speed_dating[speed_dating$gender == 0,],
               family = "binomial")
library(tidyverse)
coefficients_m <- summary(model_m)$coefficients %>% 
  as.data.frame()
coefficients_m
coef(model_m)

#===============================================================================
# CURSO:   INICIACION A R PARA EL ANALISIS ESTADISTICO
# SCRIPT:  4. Visualizacion de datos en R
# AUTORES: Miguel A. Sorrel
# VERSION:   3.0
#===============================================================================

# 1) Realiza una serie de ejercicios con la base de datos Empleados.sav:
dat <- foreign::read.spss(file = "Empleados.sav", 
                          to.data.frame = TRUE) 
spss.date <- function(x) as.Date(x/86400, origin = "1582-10-14")
dat$fechnac <- spss.date(dat$fechnac) 
rm(spss.date) 
attach(dat) 
#     a) Genera un histograma que ilustre la variable salini, ¿tenía alguien un salario anual mayor que 100.000 cuando entró en la empresa? (Responde Sí/No) [Sol: No] 
dat %>% ggplot(aes(x = salini)) + geom_histogram()
#     b) Genera un gráfico de cajas y bigotes que represente la distribución de salario según categoría laboral. ¿En cuál de las tres categorías podemos encontrar sueldos más bajos? (Responde Administrativo/Seguridad/Directivo) [Sol: Administrativo] 
dat %>% 
  ggplot(aes(x = reorder(catlab,salario,na.rm = TRUE), y = salario)) +
  geom_boxplot(fill = "white") + 
  geom_jitter(width = 0.15, alpha = 0.20, col = "red") + 
  theme_classic(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#     c) Genera un gráfico de dispersión para representar la relación entre salario y tiempo en la empresa (tiempemp). ¿Se observa una alta correlación entre estas variables? (Responde Sí/No) [Sol: No] 
dat %>% 
  ggplot(aes(x = salario, y = tiempemp)) +
  geom_point(size = 2, shape = 21, fill = "lightsteelblue3") +
  geom_smooth(method = "lm", col = "blue4", fill = "blue1", alpha = 0.05) + # loess 
  theme_classic(base_size = 16)
#     d) Genera un gráfico de cajas y bigotes para representar la relación entre salario, sexo y categoría laboral (cat) y tiempo en la empresa (tiempemp). En todos los casos donde aplica, uno de los dos sexos tiene distribuciones con salarios más eleveados. ¿De qué grupo se trata? (Responde: Hombres/Mujeres) [Sol: Hombres] 
dat %>%
  ggplot(aes(x = as.factor(sexo), y = salario, fill = catlab)) +
  geom_boxplot() +
  labs(x = "Año", y = "GDP per capita", fill = "Continente") +
  theme_minimal()
#     e) Replica el gráfico anterior pero cambiando minoria por sexo. ¿Qué grupo de directivos tiene una distribución de salarios más elevados, los pertenecientes a una minoría o los que no? Responde Sí/No para indica el grupo de minoría. [Sol: Sí] 
dat %>%
  ggplot(aes(x = as.factor(minoria), y = salario, fill = catlab)) +
  geom_boxplot() +
  labs(x = "Año", y = "GDP per capita", fill = "Continente") +
  theme_minimal()

