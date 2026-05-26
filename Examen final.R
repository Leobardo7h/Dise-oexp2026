
# NOMBRE: Leobardo Ruiz Juarez 
# MATRÍCULA: 2141782 



#============================================================
# EXAMEN DE DISEÑOS EXPERIMENTALES
# Prueba t independiente
# Problema 1: Crecimiento de plántulas con y sin cercado
# ============================================================



# ------------------------------------------------------------
# 1. Cargar paquetes
# ------------------------------------------------------------



# install.packages("tidyverse")
# install.packages("car")



library(tidyverse)
library(car)



# ------------------------------------------------------------
# 2. Crear el conjunto de datos
# ------------------------------------------------------------



datos <- data.frame(
  tratamiento = rep(c("Cercado", "Sin cercado"), each = 25),
  altura_cm = c(
    34.8, 35.2, 36.1, 35.7, 36.5,
    34.9, 35.6, 36.2, 35.4, 36.0,
    35.8, 36.4, 35.1, 35.9, 36.3,
    34.7, 35.5, 36.1, 35.3, 36.6,
    35.7, 36.2, 35.0, 35.8, 36.4,
    
    28.1, 27.9, 29.0, 28.4, 29.1,
    27.8, 28.6, 29.3, 28.2, 28.7,
    28.5, 29.2, 27.7, 28.8, 29.0,
    27.6, 28.3, 29.1, 28.0, 29.4,
    28.5, 28.9, 27.8, 28.6, 29.2
  )
)



# ------------------------------------------------------------
# 3. Revisar estructura de los datos
# ------------------------------------------------------------



str(datos)
head(datos)



# Respuesta del estudiante:
# 
# Número de grupos comparados:2
#



# ------------------------------------------------------------
# 4. Estadística descriptiva por grupo
# ------------------------------------------------------------



datos %>%
  group_by(tratamiento) %>%
  summarise(
    n = n(),
    media = mean(altura_cm),
    desviacion_estandar = sd(altura_cm),
    minimo = min(altura_cm),
    maximo = max(altura_cm)
  )



# Respuesta del estudiante:
# ¿Qué grupo presentó mayor promedio? R: El grupo cercado
# 
# ¿La diferencia observada parece relevante desde el punto de vista forestal?
# R: Si es relevante un crecimiento promedio mayor en las zonas cercadas ya que 
# se evidencia la mejoria de crecimiento al implementar proteccion contra ganado



# ------------------------------------------------------------
# 5. Gráfico exploratorio
# ------------------------------------------------------------



ggplot(datos, aes(x = tratamiento, y = altura_cm)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, size = 2) +
  labs(
    title = "Altura de plántulas por tratamiento",
    x = "Tratamiento",
    y = "Altura de plántulas (cm)"
  ) +
  theme_minimal()



# Respuesta del estudiante:
# Describa la distribución de los datos en cada grupo: El grupo cercado tiende a ser
# mas homogeneo o constante, con alturas mayores a las plantulas sin cercado las cuales 
# son mas dispersas.
# ------------------------------------------------------------
# 6. Prueba de normalidad Shapiro-Wilk
# ------------------------------------------------------------



by(datos$altura_cm, datos$tratamiento, shapiro.test)



# Respuesta del estudiante:
# Valor p para el grupo Cercado: 0.3812
# 
# Valor p para el grupo Sin cercado:0.2437
# 
# ¿Se cumple el supuesto de normalidad?
# Asi es se cumple la normalidad ambos al ser mayor a 0.05 estan 
# dentro de la campana que compone la normalidad



# ------------------------------------------------------------
# 7. Evaluación de homogeneidad de varianzas
# ------------------------------------------------------------



leveneTest(altura_cm ~ tratamiento, data = datos)



# Respuesta del estudiante:
# Valor p de la prueba de Levene:0.9628
# 
# ¿Se cumple el supuesto de homogeneidad de varianzas?
# Asi es nuevamente cae dentro de la campana de la normalidad al ser mayor a 0.05
# siendo asi las varianzas se consideran similares o iguales



# ------------------------------------------------------------
# 8. Aplicar prueba t independiente
# ------------------------------------------------------------



resultado_t <- t.test(
  altura_cm ~ tratamiento,
  data = datos,
  var.equal = TRUE
)



resultado_t



# Respuesta del estudiante:
# Estadístico t: 45.666
# 
# Grados de libertad:48
# 
# Valor p: 2.2e-16
# 
# Intervalo de confianza: 6.8638-7.4961
#



# ------------------------------------------------------------
# 9. Interpretación estadística
# ------------------------------------------------------------



# Respuesta del estudiante:
# ¿Se rechaza o no se rechaza la hipótesis nula?. Debido a los datos analizados y pruebas hechas
# se rechaza la hipotesis nula debido a que el valor de p es mucho menor que 0.05 lo cual quiere decir
# que si existen diferencias estadisticas significativas para rechazar la hipotesis nula
#
# ¿Existe diferencia estadísticamente significativa entre tratamientos?
# R: Desde luego existe diferencias estadisticamente significativas como ya lo mencionamos debido al valor de p



# ------------------------------------------------------------
# 10. Interpretación forestal
# ------------------------------------------------------------



# Respuesta del estudiante:
# Explique qué significa el resultado en el contexto del crecimiento de plántulas:
# Dado todas las pruebas realizadas y los resultados obtenidos podriamos afirmar que 
# las plantulas que cuentan con una barrera que las divide de el ganado cuenta con una mayor tasa de crecimeinto
# quiere decir que para optimizar aun mas el crecimiento de las plantulas se debe realizar este procedimiento separando
# las plantulas de pinus pseudostrobus del ganado aumentando el tamaño promedio de estas al realizarlo.

