#===============================================================================
# CURSO:   INICIACION A R PARA EL ANALISIS ESTADISTICO
# SCRIPT:  1. Breve introduccion a R
# AUTORES: Miguel A. Sorrel
# VERSION:   3.0
#===============================================================================

# Nota: Se omiten tildes

#===============================================================================
# 1. Tipos de objetos
#===============================================================================

#---------------
# 1.1. Vectores
#---------------

1:10 # Vector con num. enteros del 1 al 10
c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) # Equivalente al caso anterior

x <- 1:10 # Crear objeto x que contenga los num. enteros del 1 al 10
x # Imprimir objeto x
x + 1 # Sumar 1 a todos los elementos del vector x
x * 2 # Multiplicar todos los elementos del vector x por 2
y <- x^2 # Crear objeto y que contenga los elementos de x al cuadrado
y
z <- x + y # Crear objeto z que contenga la suma de los elementos de x e y
z

z[1:3] # Seleccionar los tres primeros elementos de z
z[c(1, 3, 5)] # Seleccionar los elementos en posicion 1, 3 y 5 de z

z > 50 # Comprobar si cada elemento de z es mayor que 50
z <= 20 # Comprobar si cada elemento de z es menor o igual a 20
z == 90 # Comprobar si cada elemento de z es igual a 90

z[z <= 20] # Seleccionar los elementos de z que sean menores o iguales a 20
z[x > 7] # Seleccionar los elementos de z correspondientes a los elementos de x mayores que 7
z[x > 7 & y >= 81] # Seleccionar los elementos de z correspondientes a los elementos de x mayores que 7 y los elementos de y mayores o iguales a 81
z[x > 7 | y == 4] # Seleccionar los elementos de z correspondientes a los elementos de x mayores que 7 o los elementos de y iguales a 4

mean(z) # Funcion para calcular la media
median(z) # Mediana
var(z) # Varianza
sd(z) # Desviacion tipica
min(z) # Minimo
max(z) # Maximo
sum(z) # Suma
prod(z) # Producto
length(z) # Longitud
head(z, n = 6) # Imprimir los n primeros elementos
tail(z, n = 3) # Imprimir los n ultimos elementos
?mean # Ver la documentacion/ayuda de la funcion mean

seq(from = 1, to = 1000, by = 27) # Secuencia desde 1 a 1000 en pasos de 27
set.seed(123) # Semilla para la generacion de numeros aleatorios
sample(x = 1:500, size = 20, replace = FALSE) # 20 valores aleatorios entre 1 y 500
runif(n = 10, min = 0, max = 1) # 10 valores aleatorios de una distr. uniforme
rnorm(n = 10, mean = 0, sd = 1) # 10 valores aleatorios de una distr. normal

v <- sample(x = 1:10, size = 30, replace = TRUE) # 30 valores aleatorios entre 1 y 10
table(v) # Num. de elementos con cada valor

w <- c(1, 2, 3, NA, 5) # Los NA son valores perdidos
w + 1 # No pueden hacerse operaciones con valores perdidos
mean(w) # Con que haya un solo NA, no se podran calcular estadisticos
mean(w, na.rm = TRUE) # La mayoria de funciones estandar tienen el argumento na.rm, para no considerar los valores perdidos

#~~~~~
# Ejercicios
#~~~~~

# 1) Establece una semilla de aleatorizacion con valor 473 y despues crea un
# vector con nombre v que contenga 100 numeros generados aleatoriamente desde 
# una distribucion uniforme con minimo igual a 1 y maximo igual a 5. Responde a
# las siguientes cuestiones:
#     a) ¿Cual es el valor correspondiente al elemento numero 23 de v? 
#     b) ¿Cual es la media del vector v?
#     c) ¿Cuantos valores de v son menores a 2? 

# 2) Ahora, tras establecer una nueva semilla de aleatorizacion con valor
# 1274, extrae 100 numeros enteros aleatorios entre 1 y 5 (con remuestreo):
#     a) ¿Cuantos elementos son iguales a 2? 
#     b) ¿Cual es la varianza de v cuando w es mayor o igual a 3? 

#------------------
# 1.2. Data frames
#------------------

?mtcars # Ver la documentacion/ayuda de esta base de datos incluida en R
View(mtcars) # Ver base de datos en una pestana a parte
mtcars # Ver base de datos en la consola de R
head(mtcars, n = 10) # Ver n primeras filas de la base de datos en la consola
dim(mtcars) # Dimensiones (num. filas y num. columnas) de la base de datos
str(mtcars) # Estructura de la base de datos
class(mtcars) # Tipo de objeto
colnames(mtcars) # Nombres de las variables (columnas) de mtcars
rownames(mtcars) # Nombres de las filas de mtcars

library(tidyverse)

mtcars$cyl # Seleccionar variable cyl (num. de cilindros)
mtcars %>% 
  select(cyl) # Seleccionar variable cyl (num. de cilindros)

mtcars$cyl[5:10] # Del quinto al decimo valor de la variable cyl
mtcars %>% 
  select(cyl) %>% # Seleccionar variable cyl
  slice(5:10) # Seleccionar los valores del quinto al decimo

mtcars$cyl[mtcars$am == 1] # Seleccionar los valores de cyl de los coches manuales (am = 1)
mtcars %>% 
  filter(am == 1) %>% # Seleccionar (filtrar) los coches manuales (am = 1)
  select(cyl) # Seleccionar variable cyl

sum(mtcars$am == 1) # num. de coches manuales
mtcars %>% 
  filter(am == 1) %>% # Seleccionar (filtrar) los coches manuales (am = 1)
  count() # num. de casos que cumplen las condiciones anteriores

mean(mtcars$cyl[mtcars$am == 1]) # Media de cyl de los coches manuales (am = 1)
mtcars %>% 
  filter(am == 1) %>% # Seleccionar (filtrar) los coches manuales (am = 1)
  summarise(mean(cyl)) # Calcular media de la variable cyl

mean(mtcars$cyl[mtcars$am == 0]) # Media de cyl de los coches automaticos (am = 0)
mtcars %>% 
  filter(am == 0) %>% # Seleccionar (filtrar) los coches automaticos (am = 0)
  summarise(mean(cyl)) # Calcular media de la variable cyl

mean(mtcars$cyl[mtcars$am == 1 & mtcars$carb > 1]) # Media de cyl de los coches manuales (am = 1) y con mas de un carburador (carb > 1)
mtcars %>% 
  filter(am == 1, carb > 1) %>% # Filtrar los coches manuales y con mas de un carburador
  summarise(mean(cyl)) # Calcular media de la variable cyl

mtcars[order(mtcars$wt, decreasing = TRUE),] # Organizar los datos por peso (wt) en orden descendente
mtcars %>% 
  arrange(desc(wt)) # Organizar los datos por peso (wt) en orden descendente

aggregate(mtcars$mpg, by = list(am = mtcars$am), mean) # Calcular la media de mpg (millas/galon) para los grupos de am (tipo de transmision)
mtcars %>% 
  group_by(am) %>% # Agrupar por variable am (transmision)
  summarise(mean(mpg)) # Calcular media de la variable mpg (millas/galon)

mtcars$hpwt <- mtcars$hp / mtcars$wt # Crear variable (num. caballos por libra de peso)
mtcars %>% 
  mutate(cyl, hpwt = hp / wt) # Crear variable (num. caballos por libra de peso)

#~~~~~
# Ejercicios
#~~~~~

# 1) La base de datos incluida en R de nombre iris (?iris) contiene 
# informacion sobre las caracteristicas morfologicas de distintos tipos de 
# flores de la familia iris.
#     a) ¿Cuantas filas tiene la base de datos? 
#     b) ¿Cuales es el nombre de la primera variable? 
#     c) ¿Cuanto mide el petalo de la flor numero 33? 
#     d) ¿Cual es el promedio de la anchura del sepalo para la especie setosa?
#     e) ¿Cuantas plantas de la especie versicolor existen con una anchura del petalo mayor a 1cm?
#     f) ¿Crea una nueva variable que representen el area del sepalo. ¿Cual es la media de esta variables?
