# Datos climaticos de cumbres

cumbres <- read.csv("cumbres.csv", header = T)
boxplot(cumbres$TEMP,
        col = "pink",
        ylim = c(10, 35),
        xlab = "Cumbres",
        ylab = "Temperatura")
abline(h=21.8, col ="indianred", lwd =3, lty ="dashed") # media observada
abline(h=mean(cumbres$TEMP), col = "violet", lwd=3, lty= "solid")
mean(cumbres$TEMP) #media historica

t.test(mu=21.8, cumbres$TEMP) 