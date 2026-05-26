# Correlación Spearman

resp <- data.frame(
  Tiempo = c(12, 15, 17, 18, 20, 21, 22, 16),
  Edad   = c(14, 25, 20, 35, 45, 30, 60, 95)
)

# Crear rangos
resp$Rango_Tiempo <- rank(resp$Tiempo, ties.method = "first")
resp$Rango_Edad   <- rank(resp$Edad, ties.method = "first")

# Gráfica de rangos
plot(resp$Rango_Tiempo, resp$Rango_Edad,
     col = "red",
     pch = 19,
     cex = 1.2,
     xlab = "Rango variable tiempo",
     ylab = "Rango edad")

# Prueba de correlación Spearman
cor.test(resp$Rango_Tiempo, resp$Rango_Edad, method = "spearman")


# Tau de Kendall --------------------------------------------------------


tau <- data.frame(
  A = c(1, 2, 3, 4, 5, 6),
  B = c(3, 1, 4, 2, 6, 5)
)

# Correlación Kendall
cor.test(tau$A, tau$B, method = "kendall")

# Gráfica
plot(tau$A, tau$B,
     col = "blue",
     pch = 19,
     cex = 1.4,
     xlab = "Evaluación Med. A",
     ylab = "Evaluación Med. B") 


