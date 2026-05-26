# Muestras dependientes

inventario <- read.csv("inventario.csv", header = T)
iventario$Fecha <- as.factor(inventario$Fecha)
boxplot(inventario$Kilogramo ~ inventario$Fecha,
        xlab= "Año",
        ylab= "Semilla",
        col= "lightgreen",
        ylim= c(40, 60))

tapply(inventario$Kilogramo, inventario$Fecha, mean)

T2012 <- subset(inventario$Kilogramo, inventario$Fecha == "T2012")
T2014 <- subset(inventario$Kilogramo, inventario$Fecha == "T2014")

t.test(T2012, T2014, paired = T) 