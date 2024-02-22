#===============================================================================
# CURSO:   INICIACION A R PARA EL ANALISIS ESTADISTICO
# SCRIPT:  4. Visualizacion de datos en R
# AUTORES: Miguel A. Sorrel
# VERSION:   3.0
#===============================================================================

# Nota: Se omiten tildes

#===============================================================================
# 0. Importar la base de datos y cargar paquetes utiles
#===============================================================================

library(tidyverse)
library(ggplot2)

dat <- foreign::read.spss(file = "Empleados.sav", # Nombre del archivo (incluyendo la extension)
                          to.data.frame = TRUE) # Convertir en data frame?
spss.date <- function(x) as.Date(x/86400, origin = "1582-10-14")
dat$fechnac <- spss.date(dat$fechnac) # Transformar fechas de SPSS a fechas inteligibles
rm(spss.date) # Eliminar la funcion spss.date del Entorno

#----------------------------
# 1. Una variable continua
#----------------------------

dat %>%
  ggplot(aes(x = salario)) + 
  geom_histogram(fill = "lightblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = mean(dat$salario), color = "red", linetype = "dashed", size = 1) +
  scale_x_continuous(limits = c(0, 150000), breaks = seq(0, 150000, by = 30000)) +
  ylim(0, 150) +
  labs(title = "Distribución de salario",
       x = "Salario",
       y = "Frecuencia") +
  theme_classic(base_size = 10, base_family = "serif") 

#------------------------------
# 2. Una variable categorica
#------------------------------

# Categoria laboral
dat %>%
  count(catlab) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = reorder(catlab, -n), y = n, fill = n)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "blue", high = "red") + 
  geom_text(aes(label = n), vjust = -0.5, size = 3) +  
  labs(title = "Distribución Categoría laboral", 
       x = "Categoría laboral", 
       y = "Número de observaciones") +
  theme_classic(base_size = 10, base_family = "serif")

#---------------------------------------------
# 3. Una variable continua y una o más categoricas
#---------------------------------------------

# Salario x Categoria laboral x sexo

dat %>% 
  ggplot(aes(x = reorder(catlab, salario,na.rm = TRUE), y = salario, fill = sexo)) +
  geom_boxplot() +
  geom_hline(yintercept = mean(salario), color = "blue", linetype = "dashed", size = 1) +
  scale_y_continuous(limits = c(0, 150000)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribución Salario por Categoría laboral y Sexo", 
       x = "Categoría laboral", 
       y = "Número de observaciones") +
  theme_classic(base_size = 10, base_family = "serif")

#------------------------------
# 4. Dos variables continuas y dos categoricas
#------------------------------

# Salario y Salario inicial x Categoria laboral y Sexo

dat %>% 
  ggplot(aes(x = salario, y = salini, color = catlab, shape = sexo)) +
  geom_point(size = 2) +
  scale_shape_manual(values=c(3, 16))+
  scale_x_continuous(limits = c(0, 150000), breaks = seq(0, 150000, by = 30000)) +
  scale_y_continuous(limits = c(0, 150000), breaks = seq(0, 150000, by = 30000)) +
  labs(title = "Gráfico de dispersión salario-salario inicial", 
       x = "Salario", 
       y = "Salario inicial") +
  theme_classic(base_size = 10, base_family = "serif")

#~~~~~
# Ejercicios
#~~~~~

# 1) Realiza una serie de ejercicios con la base de datos Empleados.sav:
#     a) Genera un histograma que ilustre la variable salini, ¿tenía alguien un salario anual mayor que 100.000 cuando entró en la empresa? (Responde Sí/No)
#     b) Genera un gráfico de cajas y bigotes que represente la distribución de salario según categoría laboral. ¿En cuál de las tres categorías podemos encontrar sueldos más bajos? (Responde Administrativo/Seguridad/Directivo)
#     c) Genera un gráfico de dispersión para representar la relación entre salario y tiempo en la empresa (tiempemp). ¿Se observa una alta correlación entre estas variables? (Responde Sí/No)
#     d) Genera un gráfico de cajas y bigotes para representar la relación entre salario, sexo y categoría laboral (cat) y tiempo en la empresa (tiempemp). En todos los casos donde aplica, uno de los dos sexos tiene distribuciones con salarios más eleveados. ¿De qué grupo se trata? (Responde: Hombres/Mujeres)
#     e) Replica el gráfico anterior pero cambiando minoria por sexo. ¿Qué grupo de directivos tiene una distribución de salarios más elevados, los pertenecientes a una minoría o los que no? Responde Sí/No para indica el grupo de minoría.
