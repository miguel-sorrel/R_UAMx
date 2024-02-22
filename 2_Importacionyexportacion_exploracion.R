#===============================================================================
# CURSO:   INICIACION A R PARA EL ANALISIS ESTADISTICO
# SCRIPT:  2. Importacion y exportacion de datos en R
# AUTORES: Miguel A. Sorrel
# VERSION:   3.0
#===============================================================================

# Nota: Se omiten tildes

#===============================================================================
# 1. Importar y exportar una base de datos
#===============================================================================

# Imprescindible pulsar arriba en el menu Session > Set Working Directory > To Source File Location
getwd() # Comprobar que el directorio de trabajo es el adecuado
list.files() # Ver archivos del directorio de trabajo

#-------------------------------------
# 1.1. Archivo de texto (.txt o .dat)
#-------------------------------------

dat <- read.table(file = "Empleados.txt", # Nombre del archivo (incluyendo la extension)
                  header = TRUE, # La primera fila del archivo contiene los nombres de las variables?
                  sep = "\t") # Separador de datos ("\t" = tabulador; " " = espacio; "," = coma...)
head(dat)
# write.table(x = dat, # Guardar el objeto dat
#             file = "Empleados.txt", # Nombre de la base de datos exportada
#             sep = "\t") # Separador de datos

#-------------------
# 1.2. Archivo .csv
#-------------------

dat <- read.csv(file = "Empleados.csv", # Nombre del archivo (incluyendo la extension)
         header = TRUE) # La primera fila del archivo contiene los nombres de las variables?
head(dat)
# write.csv(x = dat, # Guardar el objeto dat
#           file = "Empleados.csv") # Nombre de la base de datos exportada

#-------------------------------
# 1.3. Archivo de Excel (.xlsx)
#-------------------------------

# install.packages("openxlsx")
library(openxlsx) # Cargar paquete para trabajar con archivos .xlsx

dat <- read.xlsx(xlsxFile = "Empleados.xlsx", # Nombre del archivo (incluyendo la extension)
                 sheet = 1, # Hoja del archivo .xlsx que queremos importar
                 detectDates = TRUE) # Detectar fechas?
head(dat)
write.xlsx(x = dat, # Guardar el objeto dat
           file = "Empleados.xlsx") # Nombre de la base de datos exportada

#-----------------------------
# 1.4. Archivo de SPSS (.sav)
#-----------------------------

library(foreign) # Cargar paquete para trabajar con archivos .sav

dat <- read.spss(file = "compartido_github/Empleados.sav", # Nombre del archivo (incluyendo la extension)
                   to.data.frame = TRUE) # Convertir en data frame?
head(dat)
spss.date <- function(x) as.Date(x/86400, origin = "1582-10-14")
dat$fechnac <- spss.date(dat$fechnac) # Transformar fechas de SPSS a fechas inteligibles
head(dat)

#===============================================================================
# 2. Explorar una base de datos
#===============================================================================

dat <- read.table(file = "Empleados.txt", # Nombre del archivo (incluyendo la extension)
                  header = TRUE, # La primera fila del archivo contiene los nombres de las variables?
                  sep = "\t") # Separador de datos ("\t" = tabulador; " " = espacio; "," = coma...)

# informacion sobre los datos
nrow(dat) # Num. de casos de la base de datos
ncol(dat) # Num. de variables de la base de datos
dim(dat) # Dimensiones de la base de datos
head(dat) # Imprimir primeras filas de la base de datos en la consola
tail(dat) # Imprimir últimas filas de la base de datos en la consola
View(dat) # Observar base de datos en una nueva pestana
fix(dat) # Posibilidad de editar la base de datos a mano
names(dat) # Nombre de las variables

str(dat) # Nombre y tipo de las variables de la base de datos
head(dat$sexo)
head(ifelse(dat$sexo == "Hombre", "Masculino", "Femenino"))
head(factor(ifelse(dat$sexo == "Hombre", "Masculino", "Femenino")))
factor(ifelse(dat$minoria == "Sí", "Sí", "No"))

dat <- read.table(file = "Empleados.txt", # Nombre del archivo (incluyendo la extension)
                  header = TRUE, # La primera fila del archivo contiene los nombres de las variables?
                  sep = "\t", # Separador de datos ("\t" = tabulador; " " = espacio; "," = coma...)
                  stringsAsFactors = TRUE)  
str(dat)

# identificar valores perdidos
sum(!complete.cases(dat)) # Num. de personas con algun valor perdido
which(!complete.cases(dat)) # Personas con algun valor perdido
dat[!complete.cases(dat), ] # Mostrar esas personas
dat.completo <- dat[complete.cases(dat),] # Coger unicamente las personas sin valores perdidos

cor(cbind("salario" = dat$salario, "salini" = dat$salini, "edad" = dat$edad))
cor(cbind("salario" = dat$salario, "salini" = dat$salini, "edad" = dat$edad), use = "complete.obs")
cor(cbind("salario" = dat.completo$salario, "salini" = dat.completo$salini, "edad" = dat.completo$edad))

# descripcion variables
table(dat$sexo) # distribucion de frecuencias
summary(dat$sexo) # resumen (factor)
barplot(table(dat$sexo)) # grafico de barras

table(dat$salario) # distribucion de frecuencias
summary(dat$salario) # resumen (numeric)
hist(dat$salario) # histograma
plot(dat$salini, dat$salario) # diagrama de dispersion

summary(dat) # resumen (datos)

library(psych) # Contiene funciones utiles para explorar bases de datos
psych::describe(dat) # Analisis descriptivo de todas las variables (cuidado con variables categoricas!)
head(dat)
head(sapply(dat, as.numeric)) # convertimos todas las variables en numeric
cor(sapply(dat, as.numeric)) # matriz de correlaciones
psych::corPlot(sapply(dat, as.numeric)) # mapa de calor correlaciones

#~~~~~
# Ejercicios
#~~~~~

# 1) Realiza una serie de ejercicios con la base de datos "Gapminder.xlsx":
#     a) ¿Cual es el último año para el que se disponen datos?
#     b) ¿Cuantos paises estan representados en la base de datos?
#     c) ¿Cuantos paises de Oceanía estan representados en la base de datos?
#     d) ¿Cual es el máximo de la variable lifeExp? 
#     e) ¿Cual es valor de la correlación entre lifeExp y gdpPercap? 

