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