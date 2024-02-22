#===============================================================================
# CURSO:   INICIACION A R PARA EL ANALISIS ESTADISTICO
# SCRIPT:  3. Analisis de datos en R
# AUTORES: Miguel A. Sorrel
# VERSION:   3.0
#===============================================================================

# Nota: Se omiten tildes

#===============================================================================
# 0. Importar la base de datos y cargar paquetes utiles
#===============================================================================

library(foreign) # Cargar paquete para trabajar con archivos .sav
library(ggpubr) # Visualizacion de datos
library(tidyverse) # Manejar y visualizar datos
library(car) # Funciones utiles para estadistica inferencial
library(psych) # Estadística descriptiva
library(effectsize) # Tamanos del efecto
library(rstatix) # Contrastes estadisticos (ANOVA)
library(gapminder) # Base de datos ilutrativa

# Imprescindible pulsar arriba en el menu Session > Set Working Directory > To Source File Location
getwd() # Comprobar que el directorio de trabajo es el adecuado
list.files() # Ver archivos del directorio de trabajo
dat <- foreign::read.spss(file = "Empleados.sav", # Nombre del archivo (incluyendo la extension)
                          to.data.frame = TRUE) # Convertir en data frame?
spss.date <- function(x) as.Date(x/86400, origin = "1582-10-14")
dat$fechnac <- spss.date(dat$fechnac) # Transformar fechas de SPSS a fechas inteligibles
rm(spss.date) # Eliminar la funcion spss.date del Entorno
attach(dat) # Guardar las variables en la memoria RAM

#===============================================================================
# 1. Comprobar supuestos
#===============================================================================

#----------------------------
# 1.1. Normalidad univariada
#----------------------------

ks.test(x = salario, pnorm, mean(salario), sd(salario)) # Test de Kolmogorov-Smirnov
shapiro.test(x = salario) # Test de Shapiro-Wilk
hist(salario)
ggpubr::ggdensity(salario, xlab = "Salario") # Grafico de densidad
ggpubr::ggqqplot(salario) # Grafico Q-Q

#-----------------------------------------------
# 1.2. Homocedasticidad (igualdad de varianzas)
#-----------------------------------------------

bartlett.test(salario ~ catlab) # Test de Bartlett
car::leveneTest(salario ~ catlab) # Test de Levene (desviaciones normalidad)

#===============================================================================
# 2. Estadistica descriptiva
#===============================================================================

summary(dat) # Resumen estadistico de todas las variables
psych::describe(dat) # Resumen estadistico de todas las variables (cuidado con variables categoricas)

psych::describe(salario) # Estadisticos descriptivos de variable continua
quantile(salario, probs = c(0.25, 0.5, 0.75)) # Percentiles
summary(catlab) # Frecuencias de una variable categorica
table(catlab) # Equivalente al caso anterior

table(sexo, catlab) # Tabla de contingencia
margin.table(table(sexo, catlab), margin = 1) # Frecuencias marginales de filas
margin.table(table(sexo, catlab), margin = 2) # Frecuencias marginales de columnas
prop.table(table(sexo, catlab)) # Proporciones sobre el total
prop.table(table(sexo, catlab), margin = 1) # Proporciones por filas
prop.table(table(sexo, catlab), margin = 2) # Proporciones por columnas

#===============================================================================
# 3. Analisis con una variable
#===============================================================================

#------------------------------
# 3.1. Una variable dicotomica
#------------------------------

table(minoria) # Frecuencias
prop.table(table(minoria)) # Proporciones

# Prueba binomial
res <- binom.test(x = sum(minoria == "Sí"), # Frecuencia
                  n = length(minoria), # Num. de casos
                  p = 0.40, # Proporcion de prueba
                  alternative = "two.sided", # Tipo de contraste (bilateral o unilateral izq. o der.)
                  conf.level = 0.95) # Intervalo de confianza (1 - alfa)
res
effectsize::effectsize(res)

#----------------------------
# 3.2. Una variable continua 
#----------------------------

psych::describe(salario) # Estadisticos descriptivos
ks.test(x = salario, pnorm, mean(salario), sd(salario)) # Test de Kolmogorov-Smirnov
shapiro.test(x = salario) # Test de Shapiro-Wilk
ggpubr::ggdensity(salario, xlab = "Salario") # Grafico de densidad
ggpubr::ggqqplot(salario) # Grafico Q-Q
hist(salario) # Histograma
boxplot(salario) # Grafico de cajas y bigotes

# Prueba t para una muestra (si hay normalidad)
res <- t.test(salario, # Variable de interes
              mu = 40000, # Media de contraste
              alternative = "two.sided", # Tipo de contraste (bilateral o unilateral izq. o der.)
              conf.level = 0.95) # Intervalo de confianza (1 - alfa)
res
round(res$p.value, 2)

# Prueba de Wilcoxon para una muestra (si no hay normalidad)
res <- wilcox.test(salario, # Variable de interes
                   mu = 40000, # Media de contraste
                   alternative = "two.sided", # Tipo de contraste (bilateral o unilateral izq. o der.)
                   conf.level = 0.95) # Intervalo de confianza (1 - alfa)
round(res$p.value, 2)

#===============================================================================
# 4. Analisis con dos variables
#===============================================================================

#--------------------------------------------
# 4.1. Dos variables categoricas
#--------------------------------------------

prop.table(table(minoria, catlab)) 

# Prueba chi-cuadrado de independencia
res <- chisq.test(x = table(minoria, catlab))
res
round(res$p.value, 2)
effectsize::effectsize(res) # Tamano del efecto (V de Cramer)

# Prueba de McNemar
prop.table(table(sexo, minoria))
res <- mcnemar.test(table(sexo, minoria)) # McNemar solo sirve para tablas de contingencia 2x2
res
round(res$p.value, 2)
effectsize::effectsize(res) # Tamano del efecto (g de Cohen)

#------------------------------
# 4.2. Dos variables continuas
#------------------------------

psych::describe(salario) # Estadisticos descriptivos
ks.test(x = salario, pnorm, mean(salario), sd(salario)) # Test de Kolmogorov-Smirnov
shapiro.test(x = salario) # Test de Shapiro-Wilk
ggpubr::ggdensity(salario, xlab = "Salario") # Grafico de densidad
ggpubr::ggqqplot(salario) # Grafico Q-Q
hist(salario) # Histograma

plot(salario, salini) # Grafico de dispersion

# Correlacion de Pearson (si hay normalidad)
res <- cor.test(x = salario, y = salini, # Variables de interes
                method = "pearson", # Correlacion de Pearson
                conf.level = 0.95) # Intervalo de confianza (1 - alfa)
res

# Correlacion de Spearman (si no hay normalidad)
res <- cor.test(x = salario, y = salini, # Variables de interes
                method = "spearman", # Correlacion de Spearman
                conf.level = 0.95) # Intervalo de confianza (1 - alfa)
res

# Prueba t para muestras relacionadas (si hay normalidad)
res <- t.test(x = salario, y = salini, # Variables de interes
              alternative = "greater", # Tipo de contraste (bilateral o unilateral izq. o der.)
              paired = TRUE, # Indicar que las variables estan emparejadas/relacionadas
              conf.level = 0.95) # Intervalo de confianza (1 - alfa)
res
effectsize::effectsize(res) # Tamano del efecto (d de Cohen)

# Prueba de Wilcoxon para muestras relacionadas (si no hay normalidad)
res <- wilcox.test(x = salario, y = salini, # Variables de interes
                   alternative = "greater", # Tipo de contraste (bilateral o unilateral izq. o der.)
                   paired = TRUE, # Indicar que las variables estan emparejadas/relacionadas
                   conf.level = 0.95) # Intervalo de confianza (1 - alfa)
res
effectsize::effectsize(res) # Tamano del efecto (correlacion biserial)

#---------------------------------------------
# 4.3. Una variable continua y una categórica
#---------------------------------------------

psych::describe(salario) # Estadisticos descriptivos
ks.test(x = salario, pnorm, mean(salario), sd(salario)) # Test de Kolmogorov-Smirnov
shapiro.test(x = salario) # Test de Shapiro-Wilk
ggpubr::ggdensity(salario, xlab = "Salario") # Grafico de densidad
ggpubr::ggqqplot(salario) # Grafico Q-Q
hist(salario) # Histograma

aggregate(salario, by = list(sexo), mean) # Media de la variable continua en los dos grupos
boxplot(salario ~ sexo) # Grafico de cajas y bigotes
bartlett.test(salario ~ sexo) # Test de Bartlett
car::leveneTest(salario ~ sexo) # Test de Levene

# Prueba t para muestras independientes (si hay normalidad)
res <- t.test(formula = salario ~ sexo, # Formula de interes (continua ~ dicotomica)
              alternative = "two.sided", # Tipo de contraste (bilateral o unilateral izq. o der.)
              var.equal = FALSE, # Varianzas iguales? (Fijarse en el test de Bartlett y el de Levene)
              conf.level = 0.95) # Intervalo de confianza (1 - alfa)
res
effectsize::effectsize(res) # Tamano del efecto (d de Cohen)

# Prueba de Wilcoxon para muestras independientes (si no hay normalidad)
res <- wilcox.test(formula = salario ~ sexo, # Formula de interes (continua ~ dicotomica)
                   alternative = "two.sided", # Tipo de contraste (bilateral o unilateral izq. o der.)
                   conf.level = 0.95) # Intervalo de confianza (1 - alfa)
res
effectsize::effectsize(res) # Tamano del efecto (correlacion biserial)

#===============================================================================
# 5. Analisis con dos o mas variables
#===============================================================================

#------------
# 5.1. ANOVA
#------------

psych::describe(salario) # Estadisticos descriptivos
ks.test(x = salario, pnorm, mean(salario), sd(salario)) # Test de Kolmogorov-Smirnov
shapiro.test(x = salario) # Test de Shapiro-Wilk
ggpubr::ggdensity(salario, xlab = "Salario") # Grafico de densidad
ggpubr::ggqqplot(salario) # Grafico Q-Q
hist(salario) # Histograma

aggregate(salario, by = list(sexo, catlab), mean) # Media de la variable continua en los dos grupos
boxplot(salario ~ catlab * sexo) # Grafico de cajas y bigotes
car::leveneTest(salario ~ catlab * sexo) # Test de Levene

# ANOVA A-CA
res <- aov(salario ~ catlab)
summary(res) # Tabla del ANOVA
effectsize::effectsize(res) # Tamano del efecto (eta-cuadrado)
TukeyHSD(res, conf.level = 0.95) # Comparaciones por pares (prueba de Tukey)

# ANOVA A-MR
dat1 <- dat %>% 
  mutate(sueldo = salini, tiempo = "Inicio")
dat2 <- dat %>% 
  mutate(sueldo = salario, tiempo = "Actual")
new_dat <- rbind(dat1, dat2)
res <- rstatix::anova_test(data = new_dat, # Base de datos
                           dv = sueldo, # Variable dependiente
                           wid = id, # Identificador de caso
                           within = tiempo) # Factor (grupos)
res
res$ges # Tamano del efecto (eta-cuadrado)
rstatix::pairwise_t_test(data = new_dat, 
                         formula = sueldo ~ tiempo, 
                         paired = TRUE, 
                         p.adjust.method = "BH") # Comparaciones por pares

# ANOVA AB-CA
res <- aov(salario ~ catlab * sexo)
summary(res) # Tabla del ANOVA
effectsize::effectsize(res) # Tamano del efecto (eta-cuadrado)
TukeyHSD(res, conf.level = 0.95) # Comparaciones por pares (prueba de Tukey)
boxplot(salario ~ catlab*sexo, cex.axis = 0.60,
        col = c("lightblue", "lightblue", "lightblue",
                                       "lightyellow", "lightyellow", "lightyellow"), dat = new_dat)

# ANOVA AB-CA-MR
res <- rstatix::anova_test(data = new_dat, # Base de datos
                           dv = sueldo, # Variable dependiente
                           wid = id, # Identificador de caso
                           between = catlab, # Factor inter-grupos
                           within = tiempo) # Factor intra-grupos
res
res$ges # Tamano del efecto (eta-cuadrado)
boxplot(sueldo ~ tiempo * catlab, cex.axis = 0.60,
        col = c("lightblue", "lightyellow"), dat = new_dat)
library(tidyverse) # para usar el operador %>%
new_dat %>%
  group_by(tiempo) %>%
  rstatix::t_test(data =., sueldo ~ catlab) %>%
  rstatix::adjust_pvalue(method = "bonferroni") %>%
  rstatix::add_significance("p.adj")
new_dat %>%
  group_by(catlab) %>%
  rstatix::t_test(data =., sueldo ~ tiempo) %>%
  rstatix::adjust_pvalue(method = "bonferroni") %>%
  rstatix::add_significance("p.adj")

#-----------------------
# 5.2. Regresion lineal
#-----------------------

psych::describe(salario) # Estadisticos descriptivos
ks.test(x = salario, pnorm, mean(salario), sd(salario)) # Test de Kolmogorov-Smirnov
shapiro.test(x = salario) # Test de Shapiro-Wilk
ggpubr::ggdensity(salario, xlab = "Salario") # Grafico de densidad
ggpubr::ggqqplot(salario) # Grafico Q-Q
hist(salario) # Histograma

psych::corPlot(data.frame(salario, salini, educ, as.numeric(catlab), as.numeric(sexo)))

# Coeficientes no estandarizados
res1 <- lm(salario ~ salini + educ + as.numeric(catlab) + as.numeric(sexo), dat = dat)
res1
summary(res1)
round(coef(res1), 3)
plot(res1)

# Coeficientes estandarizados
res2 <- lm(scale(salario) ~ scale(salini) + scale(educ) + scale(as.numeric(catlab)) + scale(as.numeric(sexo)))
res2
summary(res2)
round(coef(res2), 3)
plot(res2)

# Podemos testar este modelo en nuevos datos
new_data <- data.frame(
  salini = c(12450, 17250, 15750),
  educ = c(2, 4, 8),
  catlab = c(7, 12, 18),
  sexo = c(1,1, 2)
)
predict(res1, new_data, type = "response")

#--------------------------
# 5.3. Regresion logistica
#--------------------------

url <- "https://raw.githubusercontent.com/miguel-sorrel/R_UAMx/main/speed_dating.csv"
speed_dating <- read.csv(url)
speed_dating <- speed_dating[ ,c("gender", "goal", "dec", "attr", "intel", "prob")] 
head(speed_dating)
# id: caso
# gender: genero (0 = mujer, 1 = hombre)
# goal: intenciones (1 = noche divertida, 2 = conocer gente, 3 = conseguir una cita, 4 =relaci?n seria, 5 = para decir que lo hice, 6 = otro)
# dec: si decide tener otra cita (0 = no, 1 = s?)
# attr: de 0 a 10 puntuacion que asigna en atractivo
# intel: de 0 a 10 puntuacion que asigna en inteligencia
# prob: de 0 a 10 para indicar si piensa que tambien resulto atractiva a la otra persona

# Binomial general linear model para modelar las decisiones de los hombres 
model_m <- glm(dec ~ attr + intel + prob, 
               data = speed_dating[speed_dating$gender == 1, ],
               family = "binomial")
coefficients_m <- summary(model_m)$coefficients %>% 
  as.data.frame()
coefficients_m
round(coefficients_m$`Pr(>|z|)`, 2)
coef(model_m)
exp(coef(model_m)) # Odds ratio
# Un punto mas de atractivo fisico aumenta las probabilidades en un 115%
# Un punto mas en inteligencia disminuye las probabilidades en un 7%
# Un punto mas en la percepcion de interes reciproco aumenta las probabilidades en un 39%

# Podemos testar este modelo en nuevos datos
new_data <- data.frame(
  attr = c(1, 5, 9),
  intel = c(2, 4, 8),
  prob = c(5, 7, 9)
)
predict(model_m, new_data, type = "response")

#~~~~~
# Ejercicios
#~~~~~

# 1) Realiza una serie de ejercicios con la base de datos "speed_dating.csv":
#     a) La variable atractivo "attr", ¿se distribute normalmente? Ten en cuenta que 
#        la función a utilizar no admite valores perdidos. Tendrás que usar complete.cases(). 
#     b) ¿Qué proporción de mujeres (gender = 0) tenían como objetivo conocer gente (goal = 2)?
#     c) ¿Hay diferencias significativas en el promedio de atractivo según género? (responde Sí/No)
#     d) ¿Cuál es la correlación entre atractivo (attr) e inteligencia (intel)?
#     e) Ajusta el modelo de regresión logística empleando sólo los datos de mujeres (gender == 1). 
#        ¿Cuál de los tres predictores (attr, intel o prob) tiene asociado un coeficiente de regresión más grande?
