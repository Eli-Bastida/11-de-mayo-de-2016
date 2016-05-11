# 11-de-mayo-de-2016
### SES: suavizante exponencial simple ###

install.packages("fpp")
library (fpp)
install.packages("foreing")
library (foreing)
tasas <- read.csv("C:\\Users\\SALA-C9\\Desktop\\tasas inegi.csv")
tasasdes <- ts (tasas [,2], start = 2005, frequency = 4)
tasasdes
mod1 <- ses (tasasdes, alpha = 0.2, initial = "simple", h = 4) ## h periodos que queremos que nos pronostique
mod2 <- ses (tasasdes, alpha = 0.6, initial = "simple", h = 4)
mod3 <- ses (tasasdes, alpha = 1, initial = "simple", h = 4)
plot (mod1, plot.conf= F, ylab = "desocupados", xlab= "año", main= "", fcol = "white", type = "o")

## ajustes
lines(fitted(mod1), col= "blue", type="o") ## tipe para el tipo de grafica
lines(fitted(mod2), col= "red", type="o")
lines(fitted(mod3), col= "green", type="o")

names (mod1)
ajus1 <- mod1$fitted
ajus2 <- mod2$fitted
ajus3 <- mod3$fitted
resmod1 <- (tasasdes-ajus1)
resmod2 <- (tasasdes-ajus2)
resmod3 <- (tasasdes-ajus3)
ajustados <- data.frame(tasasdes, ajus1,ajus2, ajus3, resmod1, resmod2, resmod3)

res1 <- (resmod1-mod1$residuals)
res2 <- (resmod2-mod2$residuals)
res3 <- (resmod3-mod3$residuals)
res1
residuales <- data.frame(res1, res2,res3)

##### EJERCICIO ##### 
## elegir dos series de tiempo son tendencia ni estacionalidad
## serie 1, alpha = .1, .6 y .9
## serie 2, alpha = .0001, .5, .987
## graficar las series de tiempo
## clacular el mae, rmse, mape, sse
## decidir en cada una de las series cual ses ocuparian
## con base a los mae, rmse, mape y sse, argumentan porque 
## cren que es mejor utilizar un alpha cercano a cero o cercano a 1

tpregen <- ts (tasas [,3], start = 2005, frequency = 4)
plot(tpregen)
tinfolab <- ts (tasas [,6], start = 2005, frequency = 4)
plot(tinfolab)

serie1a <- ses (tpregen, alpha = 0.1, initial = "simple", h = 4) ## h periodos que queremos que nos pronostique
serie1b <- ses (tpregen, alpha = 0.6, initial = "simple", h = 4)
serie1c <- ses (tpregen, alpha = 0.9, initial = "simple", h = 4)
serie2a <- ses (tinfolab, alpha = 0.0001, initial = "simple", h = 4) ## h periodos que queremos que nos pronostique
serie2b <- ses (tinfolab, alpha = 0.5, initial = "simple", h = 4)
serie2c <- ses (tinfolab, alpha = 0.987, initial = "simple", h = 4)

plot (serie1a, plot.conf= F, ylab = "Tasa de Presión General", xlab= "año", main= "", fcol = "white", type = "o")
lines(fitted(serie1a), col= "blue", type="o") ## tipe para el tipo de grafica
lines(fitted(serie1b), col= "red", type="o")
lines(fitted(serie1c), col= "green", type="o")

plot (serie2a, plot.conf= F, ylab = "Tasa de Informalidad Laboral", xlab= "año", main= "", fcol = "white", type = "o")
lines(fitted(serie2a), col= "blue", type="o") ## tipe para el tipo de grafica
lines(fitted(serie2b), col= "red", type="o")
lines(fitted(serie2c), col= "green", type="o")

tpgmeanf <- meanf(tpregen, 4) ## 4 puntos en la grafica, 
View (tpgmeanf)
plot(tpgmeanf)
evaluandos1 <- accuracy(tpgmeanf)
evaluandos1

tilmeanf <- meanf(tinfolab, 4) ## 4 puntos en la grafica, 
plot(tilmeanf)
evaluandos2 <- accuracy(tilmeanf)
evaluandos2

tilmeanf <- meanf(tinfolab, 4) ## 4 puntos en la grafica, 
plot(tilmeanf)
evaluandos2 <- accuracy(tilmeanf)
evaluandos2
