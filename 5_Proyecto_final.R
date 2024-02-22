# Importa la base de datos "riasec.txt" disponible en https://github.com/miguel-sorrel/R_UAMx
# Las variables que contiene son las siguientes:
## id: identificación del sujeto
## gender: Género
## age: Edad en años
## familysize: Respuesta a ""Incluyéndote a ti, ¿cuántos hijos tuvo tu madre?"
## country: País de nacimiento (todos United States)
## major: Carrera cursada en la universidad - Psychology, Engineering, Other       
## riasec: puntuaciones en el cuestionario RIASEC para medir vocación profesional (https://openpsychometrics.org/tests/RIASEC/): Realistic, Investigative, Artistic, Social, Enterprising, Conventional 
## tipi: puntuaciones en el cuestionario TIPI (https://gosling.psy.utexas.edu/scales-weve-developed/ten-item-personality-measure-tipi/): Openess, Conscientiousness, Extraversion, Agreeableness y Neuroticism
# La base de datos es una selección de los datos disponibles en https://openpsychometrics.org/_rawdata/

# Prepara la respuesta a las siguientes preguntas para poder meter esas respuestas en el Test Proyecto final
# 1. ¿Cómo debe establecerse el argumento "sep" en read.table() para leer adecuadamente los datos?
# 2. ¿Cuántas filas contiene la base de datos?
# 3. ¿Cuántas personas han estudiado Psicología? 
# 4. ¿Cuál es la puntuación en la escala Realistic del cuestionario RIASEC de la persona que ocupa la fila 50?
# 5. ¿Cuál es promedio en la escala Investigative del cuestionario RIASEC?
# 6. ¿Cuántas personas tienen una puntuación mayor de 5 en la escala Agreeableness deñ cuestionario TIPI?
# 7. Crea una nueva variable que represente la suma de Agreeableness-TIPI y Extraversion-TIPI, ¿cuál es la desviación tipica de esa variable?
# 8. ¿Cuál es la correlación entre las escalas Artistic y Conventional del cuestionario RIASEC?
# 9. ¿Cuál es la mediana para la variable familysize? 
# 10. De las personas que estudiaron Ingeniería, ¿qué proporción son hombres?
# 11. Las variables categóricas major y gender, ¿están relacionadas? Responde Sí/No
# 12. ¿Cuál es tamaño del efecto V de Cramer asociado al constraste de hipótesis llevado a cabo en el apartado anterior?
# 13. Ejecuta un ANOVA para determinar el efecto de gender y major sobre riasec.social. ¿Cuál es el valor del estadístico F para el efecto de intervención?
# 14. Continúa con el ANOVA del apartado anterior. ¿Hay diferencias estadísticamente significativas entre los hombres y mujeres que estudian psicología? Responde Sí/No
# 15. Ejecuta una regresión lineal para predecir riasec.investigative a partir de age, familysize, tipi.neuroticism y tipi.openess. ¿Cuántos de los predictores tienen un coeficiente de regresión asociado significativo? Responde 1, 2, 3 o 4
# 16. Continúa con el modelo de regresión del apartado anterior. ¿Cuál es el coeficiente de regression sin estandarizar asociado con tipi.openess? 
# 17. Sobre este gráfico:ggplot(riasec, aes(x = riasec$major, y = riasec$riasec.conventional)) + geom_boxplot(). Queremos diferenciar por a los sujetos según su género. ¿Cuál de las siguientes opciones podemos emplear? 
#     a) Añadir fill = gender a aes()
#     b) Cambiar geom_boxplot() por geom_bar()
#     c) Agregar alpha = 0.50 a geom_boxplot()