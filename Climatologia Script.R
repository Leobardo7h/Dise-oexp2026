# MAGT
# 14/04/2026
# Datos climatologicos
# Cumbres NL, Villagran Tamps, Carranza Coahuila

library(lubridate)
clima <- read.csv("CLIMATOLOGIA.csv", header =T)
clima$Localidad <- as.factor(clima$Localidad)
clima$Fecha <- as.Date(clima$Fecha, format = "%d/%m/%Y")
clima$fechaHora <- as.POSIXct (paste(clima$Fecha, clima$Hora),
                               format = "%Y-%m-%d %H:%M:%S")

boxplot(clima$TA ~ clima$Localidad,
        col= "indianred",
        ylab = "TA (C°)",
        xlab = "Localidades",
        ylim = c(10,35),
        outcol = "indianred", #marcar outliers en rojo
        outpch = 19) #cambiar la forma a circulo relleno

tapply(clima$TA, clima$Localidad, mean)

# Temperatura en el curso de la semana

Cumbres <- subset(clima,Localidad == "Cumbres")
Cumbres<- Cumbres[order(Cumbres$fechaHora),]

plot(Cumbres$FechaHora, Cumbres$TEMP,
     type = "l",
     col = "indianred",
     lwd =2,
     xaxt = "n",
     xlab = "Localidad EMAs",
     ylab= "Temperatura (°C)",
     main = "Serie de tiempo variable Temp")

# Normalidad de datos
tapply(clima)


# ANOVA

clim.aov <- aov(clima$TEMP ~ clima$Localidad)
summary(clim.aov)

# Aceptamos H1, por lo tanto realizamos una prueba de Tukey
TukeyHSD(clim.aov)

# Homogeneidad de varianza
tapply(clima$TEMP, clima$Localidad, var)
bartlett.test(clima$TEMP ~ clima$Localidad) 