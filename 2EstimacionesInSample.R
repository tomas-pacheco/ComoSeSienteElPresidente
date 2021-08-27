#########################################
#                                       #
#     ¿Cómo se siente el presidente?    # 
#                                       # 
#       Kleiman, Pacheco y Riquelme     #  
#                                       #
#       2. Estimaciones in-sample       #
#########################################

# Ante cualquier error de código o duda, comunicarse con ariquelme@udesa.edu.ar

# Definimos la ventana in-sample y out-of-sample.

in.sample <- data[1:425,]

out.of.sample <- data[426:483,]

## Estimación de los modelos ##

# Estimamos un modelo ARIMA. 

arima.1 <- auto.arima(in.sample[,2])

summary(arima.1)

arima.to.table <- arima(in.sample[,2],order = c(5,0,1))


# Validaremos el modelo chequeando que los residuos sean ruido blanco.

act1 <- checkresiduals(arima.1, lag = 13)

test <- round(act1$p.value, 4)

# Como H0 es ausencia de autocorrelación y no rechazamos la hipótesis
# podemos decir que los residuos no están correlacionados.
# Modelo valido, pval = 0.1405

# Estimamos un modelo ETS.

ets.1 <- ets(in.sample[,2])

# Verificamos que los residuos sean RB.

act2 <- Box.test(ets.1$residuals, lag = 13, type = c("Ljung-Box"))

test2 <- round(act2$p.value, 4)

# Como H0 es ausencia de autocorrelación y rechazamos la hipótesis
# podemos decir que los residuos están correlacionados.
# Modelo no valido, pval = 0.000004111937.

# Estimamos un modelo ARIMAX 

# Usaremos el sentimiento de Alberto Fernández. 
# como variable explicada e introduciremos al resto de nuestras variables
# como regresores exógenos.

# Utilizamos el mismo modelo ARIMA seleccionado previamente.

arimax.1 <- Arima(in.sample[,2], order = c(5,0,1),
                  xreg = as.matrix(in.sample[,3:length(in.sample)]))

arimax.to.table <- arima(in.sample[,2],order = c(5,0,1),
                         xreg = as.matrix(in.sample[,4:length(in.sample)]))

act3 <- Box.test(arimax.1$residuals, lag = 13, type = c("Ljung-Box"))

test3 <- round(act3$p.value, 4)

# El p-valor nos da 0.6359, con lo cual validamos el modelo.

# Estimamos un modelo ADL. 

library(ARDL)
library(dynlm)

# Creamos las dummies. 

# Seleccionamos el orden del ADL. 

order.adl.dl <- auto_ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + casosarg + muertosarg +
                            vacunasarg + maxtemp + mintemp + muertes.arg.rel + casos.arg.rel, 
                          data = in.sample[,-1], max_order = 5)


# Ahora estimamos el modelo.

adl.dl <- ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                 casosarg + muertosarg + vacunasarg + maxtemp + 
                 mintemp + muertes.arg.rel + casos.arg.rel, 
               data = in.sample[,-1], order = as.vector(order.adl.dl$best_order))

# Estimamos el ADL pero con la función dynlm.

adl <- adl.dl$full_formula
adl.1 <- dynlm(adl, data = adl.dl$data)

# Comprobamos que tiene los mismos coeficientes.

identical(adl.dl$coefficients, adl.1$coefficients)

# Test.

act4 <- Box.test(adl.1$residuals, lag = 13, type = c("Ljung-Box"))
test4 <- round(act4$p.value, 4)

# El p-valor nos da 0.0083.

model <- lm(rnorm(100,0,1) ~ rnorm(100,20,3))

stargazer(arima.to.table, arimax.to.table, model, adl.1,
          align = TRUE, 
          dep.var.labels = c("SentIndex", "SentIndex", "SentIndex", "SentIndex"),
          keep.stat = c("n", "ll", "rsq"),
          no.space = TRUE,
          type = "latex")

# Continuamos estimando un modelo VAR.

library(vars)

# Nos quedamos con el período in-sample que tiene todas las variables estacionarias.

reservas.est <- diff(in.sample[,5])

dolar.est <- diff(in.sample[,8])

in.sample.d <- cbind(in.sample[-1,-c(5,8)],reservas.est, dolar.est)

in.sample.d <- in.sample.d[,2:15]

# Elegimos el orden.

var.d <- VARselect(in.sample.d, type ="trend")

var.d$selection

# El orden de rezagos óptimos, de acuerdo al criterio de FPE, es 6.

# Estimamos:

# VER: si estimamos el var con constante, tendencia, las dos o ninguna

var.dl <- VAR(in.sample.d, p = 6, type = "trend")

# Vemos si podemos validar el modelo.

serial.test(var.dl) 

# Rechazamos la hipótesis nula de que los residuos están
# incorrelacionados. Por lo tanto, no podemos validar el modelo. 

# Ahora lo que haremos es interpretar económicamente al modelo VAR.

# Comenzaremos estimando funciones de impulso respuesta.

variables.var <- colnames(in.sample.d)

set.seed(444)

sent.fir <- irf(var.dl, impulse = variables.var, 
                response = "sentsmooth", n.ahead = 7,
                ortho = FALSE, runs = 1000)

# Hacemos una función que devuelve un df para cada una de las covariables.

fir.cases <- function(variable){
  t <- as.data.frame(cbind(seq(0,7,1), sent.fir$irf[[variable]],
                           sent.fir$Lower[[variable]],
                           sent.fir$Upper[[variable]]))
  colnames(t) <- c("day","irf", "lower", "upper")
  return(t)
}

# Graficamos.

ggplot(fir.cases("twfav") , aes(y = irf, x = day)) +
  theme_bw() + 
  xlim(0,7) +
  ylim(-0.01,0.01)+
  geom_hline(aes(yintercept = 0), size = 1, color = colores[1])+
  geom_line(aes(y = upper , x = day), size = 1, color = colores[1],
            linetype = "dashed") +
  geom_line(aes(y = irf, x = day), size = 1, color = "black") +
  geom_line(aes(y = lower, x = day), size = 1, color = colores[1], 
            linetype = "dashed")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("IRF de Cantidad de Favoritos") + 
  labs(x = "95 % Bootstrap CI, 1000 runs", 
       y = "Sentimiento del presidente", 
       caption = "Fuente: elaboración propia")

ggplot(fir.cases("twret") , aes(y = irf, x = day)) +
  theme_bw() + 
  xlim(0,7) +
  ylim(-0.01,0.01)+
  geom_hline(aes(yintercept = 0), size = 1, color = colores[2])+
  geom_line(aes(y = upper , x = day), size = 1, color = colores[2],
            linetype = "dashed") +
  geom_line(aes(y = irf, x = day), size = 1, color = "black") +
  geom_line(aes(y = lower, x = day), size = 1, color = colores[2], 
            linetype = "dashed")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("IRF de Cantidad de Favoritos") + 
  labs(x = "95 % Bootstrap CI, 1000 runs", 
       y = "Sentimiento del presidente", 
       caption = "Fuente: elaboración propia")

e1 <- ggplot(fir.cases("tasaint") , aes(y = irf, x = day)) +
  theme_bw() + 
  xlim(0,7) +
  ylim(-0.025,0.025)+
  geom_hline(aes(yintercept = 0), size = 1, color = colores[3])+
  geom_line(aes(y = upper , x = day), size = 1, color = colores[3],
            linetype = "dashed") +
  geom_line(aes(y = irf, x = day), size = 1, color = "black") +
  geom_line(aes(y = lower, x = day), size = 1, color = colores[3], 
            linetype = "dashed")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("IRF de tasa de interés") +
  labs(x = "95 % Bootstrap CI, 1000 runs", 
       y = "Sentimiento del presidente", 
       caption = "Fuente: elaboración propia")

ggplot(fir.cases("basemon") , aes(y = irf, x = day)) +
  theme_bw() + 
  xlim(0,7) +
  ylim(-0.025,0.025)+
  geom_hline(aes(yintercept = 0), size = 1, color = colores[4])+
  geom_line(aes(y = upper , x = day), size = 1, color = colores[4],
            linetype = "dashed") +
  geom_line(aes(y = irf, x = day), size = 1, color = "black") +
  geom_line(aes(y = lower, x = day), size = 1, color = colores[4], 
            linetype = "dashed")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("IRF de Base Monetaria") + 
  labs(x = "95 % Bootstrap CI, 1000 runs", 
       y = "Sentimiento del presidente", 
       caption = "Fuente: elaboración propia")

ggplot(fir.cases("casosarg") , aes(y = irf, x = day)) +
  theme_bw() + 
  xlim(0,7) +
  ylim(-0.025,0.025)+
  geom_hline(aes(yintercept = 0), size = 1, color = colores[5])+
  geom_line(aes(y = upper , x = day), size = 1, color = colores[5],
            linetype = "dashed") +
  geom_line(aes(y = irf, x = day), size = 1, color = "black") +
  geom_line(aes(y = lower, x = day), size = 1, color = colores[5], 
            linetype = "dashed")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("IRF de Casos COVID-19 en Argentina") +
  labs(x = "95 % Bootstrap CI, 1000 runs", 
       y = "Sentimiento del presidente", 
       caption = "Fuente: elaboración propia")

e2 <- ggplot(fir.cases("muertosarg") , aes(y = irf, x = day)) +
  theme_bw() + 
  xlim(0,7) +
  ylim(-0.0025,0.0025)+
  geom_hline(aes(yintercept = 0), size = 1, color = colores[6])+
  geom_line(aes(y = upper , x = day), size = 1, color = colores[6],
            linetype = "dashed") +
  geom_line(aes(y = irf, x = day), size = 1, color = "black") +
  geom_line(aes(y = lower, x = day), size = 1, color = colores[6], 
            linetype = "dashed")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("IRF de muertes") +
  labs(x = "95 % Bootstrap CI, 1000 runs", 
       y = "Sentimiento del presidente", 
       caption = "Fuente: elaboración propia")

ggplot(fir.cases("vacunasarg") , aes(y = irf, x = day)) +
  theme_bw() + 
  xlim(0,7) +
  ylim(-0.02,0.02)+
  geom_hline(aes(yintercept = 0), size = 1, color = colores[1])+
  geom_line(aes(y = upper , x = day), size = 1, color = colores[1],
            linetype = "dashed") +
  geom_line(aes(y = irf, x = day), size = 1, color = "black") +
  geom_line(aes(y = lower, x = day), size = 1, color = colores[1], 
            linetype = "dashed")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("IRF de Vacunas Aplicadas")+
  labs(x = "95 % Bootstrap CI, 1000 runs", 
       y = "Sentimiento del presidente", 
       caption = "Fuente: elaboración propia")

ggplot(fir.cases("maxtemp") , aes(y = irf, x = day)) +
  theme_bw() + 
  xlim(0,7) +
  ylim(-0.02,0.02)+
  geom_hline(aes(yintercept = 0), size = 1, color = colores[1])+
  geom_line(aes(y = upper , x = day), size = 1, color = colores[1],
            linetype = "dashed") +
  geom_line(aes(y = irf, x = day), size = 1, color = "black") +
  geom_line(aes(y = lower, x = day), size = 1, color = colores[1], 
            linetype = "dashed")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("IRF de Temperatura Máxima") +
  labs(x = "95 % Bootstrap CI, 1000 runs", 
       y = "Sentimiento del presidente", 
       caption = "Fuente: elaboración propia")

e6 <- ggplot(fir.cases("mintemp") , aes(y = irf, x = day)) +
  theme_bw() + 
  xlim(0,7) +
  ylim(-0.02,0.02)+
  geom_hline(aes(yintercept = 0), size = 1, color = colores[2])+
  geom_line(aes(y = upper , x = day), size = 1, color = colores[2],
            linetype = "dashed") +
  geom_line(aes(y = irf, x = day), size = 1, color = "black") +
  geom_line(aes(y = lower, x = day), size = 1, color = colores[2], 
            linetype = "dashed")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("IRF de temp. mínima") +
  labs(x = "95 % Bootstrap CI, 1000 runs", 
       y = "Sentimiento del presidente", 
       caption = "Fuente: elaboración propia")

e3 <- ggplot(fir.cases("casos.arg.rel") , aes(y = irf, x = day)) +
  theme_bw() + 
  xlim(0,7) +
  ylim(-6,6)+
  geom_hline(aes(yintercept = 0), size = 1, color = colores[3])+
  geom_line(aes(y = upper , x = day), size = 1, color = colores[3],
            linetype = "dashed") +
  geom_line(aes(y = irf, x = day), size = 1, color = "black") +
  geom_line(aes(y = lower, x = day), size = 1, color = colores[3], 
            linetype = "dashed")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("IRF de casos relativos") +
  labs(x = "95 % Bootstrap CI, 1000 runs", 
       y = "Sentimiento del presidente", 
       caption = "Fuente: elaboración propia")


e4 <- ggplot(fir.cases("muertes.arg.rel") , aes(y = irf, x = day)) +
  theme_bw() + 
  xlim(0,7) +
  ylim(-3.5,3)+
  geom_hline(aes(yintercept = 0), size = 1, color = colores[4])+
  geom_line(aes(y = upper , x = day), size = 1, color = colores[4],
            linetype = "dashed") +
  geom_line(aes(y = irf, x = day), size = 1, color = "black") +
  geom_line(aes(y = lower, x = day), size = 1, color = colores[4], 
            linetype = "dashed")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("IRF de muertes relativas")+
  labs(x = "95 % Bootstrap CI, 1000 runs", 
       y = "Sentimiento del presidente", 
       caption = "Fuente: elaboración propia")

ggplot(fir.cases("reservas.est") , aes(y = irf, x = day)) +
  theme_bw() + 
  xlim(0,7) +
  ylim(-0.005,0.005)+
  geom_hline(aes(yintercept = 0), size = 1, color = colores[5])+
  geom_line(aes(y = upper , x = day), size = 1, color = colores[5],
            linetype = "dashed") +
  geom_line(aes(y = irf, x = day), size = 1, color = "black") +
  geom_line(aes(y = lower, x = day), size = 1, color = colores[5], 
            linetype = "dashed")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("IRF de Reservas del BCRA") +
  labs(x = "95 % Bootstrap CI, 1000 runs", 
       y = "Sentimiento del presidente", 
       caption = "Fuente: elaboración propia")

e5 <- ggplot(fir.cases("dolar.est") , aes(y = irf, x = day)) +
  theme_bw() + 
  xlim(0,7) +
  ylim(-0.01,0.01)+
  geom_hline(aes(yintercept = 0), size = 1, color = colores[6])+
  geom_line(aes(y = upper , x = day), size = 1, color = colores[6],
            linetype = "dashed") +
  geom_line(aes(y = irf, x = day), size = 1, color = "black") +
  geom_line(aes(y = lower, x = day), size = 1, color = colores[6], 
            linetype = "dashed")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("IRF del tipo de cambio") +
  labs(x = "95 % Bootstrap CI, 1000 runs", 
       y = "Sentimiento del presidente", 
       caption = "Fuente: elaboración propia")

# Exportamos las que tienen resultados interesantes.

library(gridExtra)

grid.arrange(e2, e3, e4, nrow = 1, ncol = 3)

ggsave(file="FirEpid.eps", width=6.5, height=4, dpi=300)

grid.arrange(e1, e5,e6 ,nrow = 1, ncol = 3)

ggsave(file="FirResto.eps", width=6.5, height=4, dpi=300)

# Descomposición de la varianza.

var.decomp <- fevd(var.dl, n.ahead=7)

library(reshape)

vd <- as.data.frame(cbind(seq(1,7,1),
                          var.decomp$sentsmooth[,14],
                          var.decomp$sentsmooth[,13],
                          var.decomp$sentsmooth[,12],
                          var.decomp$sentsmooth[,11],
                          var.decomp$sentsmooth[,10],
                          var.decomp$sentsmooth[,9],
                          var.decomp$sentsmooth[,8],
                          var.decomp$sentsmooth[,7],
                          var.decomp$sentsmooth[,6],
                          var.decomp$sentsmooth[,5],
                          var.decomp$sentsmooth[,4],
                          var.decomp$sentsmooth[,3],
                          var.decomp$sentsmooth[,2],
                          var.decomp$sentsmooth[,1]))

colnames(vd) <- c("id", "Dólar", "Res. BCRA", "Casos Arg.Rel.", 
                  "Muertes Arg. Rel.", "Temp. Min.", "Temp. Max.",
                  "Vacunas Arg.", "Muertes Arg.", "Casos Arg.", "Base Mon.",
                  "Tasa Int.", "Retweets", "Favoritos", "Sentimiento AF")

vd_panel <- melt(vd, id = c("id"))


ggplot(vd_panel, aes(fill=variable, y=value, x=id)) + 
  geom_bar(position="fill", stat="identity") + 
  theme_minimal() + 
  scale_x_discrete(limits=1:7, labels = c("1", "2", "3", "4",
                                          "5", "6", "7")) +
  scale_fill_manual("", values = c("Dólar" = colores2[8], 
                                   "Res. BCRA" = colores2[7], 
                                   "Casos Arg.Rel." = colores2[6], 
                                   "Muertes Arg. Rel." = colores2[5],
                                   "Temp. Min." = colores2[4], 
                                   "Temp. Max." = colores2[3], 
                                   "Vacunas Arg." = colores2[2], 
                                   "Muertes Arg." = colores2[1],
                                   "Casos Arg." = colores[6], 
                                   "Base Mon." = colores[5], 
                                   "Tasa Int." = colores[4], 
                                   "Retweets" = colores[3],
                                   "Favoritos" = colores[2], 
                                   "Sentimiento AF" = colores[1]))+
  labs(x = "Horizonte", 
       y = "Porcentaje explicado", 
       title = "Descomposición de la varianza",
       subtitle = "Sentimiento del presidente",
       caption = "Fuente: elaboración propia") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggsave(file="VarDecomp.eps", width=5.5, height=4, dpi=300)

# Ahora, vamos a estimar un modelo FAVAR. Comenzamos aplicando la técnica de componentes
# principales a nuestras variables explicativas.

PCA1 <- prcomp(in.sample[,3:15], scale =TRUE) 

# Veremos los autovalores para evaluar qué cantidad de componentes utilizaremos.

PCA1$sdev^2

# Como hay 4 componentes cuyo autovalor es superior a 1, los usaremos.

library(dplyr)

PC.is <- scale(in.sample[,3:15])%*%PCA1$rotation

library(factoextra)

eigr <- matrix(NA, 13, 4)
eig <- as.matrix(get_eig(PCA1))

eigr[,1] <- c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6",
              "PC7", "PC8", "PC9", "PC10", "PC11", "PC12", "PC13")
eigr[,2] <- round(eig[,1],4)
eigr[,3] <- round(eig[,2],4)
eigr[,4] <- round(eig[,3],4)

colnames(eigr) <- c("Componente", "Autovalor",
                    " Porcentaje de varianza explicada",
                    "Porcentaje de varianza explicada acumulada")
stargazer(eigr, type = "text")

# Graficamos. 

eigr <- as.data.frame(eigr)
eigr <- cbind(eigr[,2:ncol(eigr)], as.double(rownames(eigr)))
colnames(eigr) <- c("eigen", "varexp", "cumvarexp", "pc")
eigr$varexp <- as.double(eigr$varexp)
eigr$cumvarexp <- as.double(eigr$cumvarexp)


e1 <- ggplot(eigr, aes(x = pc,  y = varexp)) + 
  geom_line(aes(x = pc, y = varexp), size = 1, color = colores[1]) + 
  theme_minimal()+
  labs(x = "Componente principal", 
       y = "Proporción de varianza", 
       title = "Varianza explicada",
       #subtitle = "Sentimiento del presidente",
       caption = "  ") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) + 
  scale_x_continuous(limits = c(1, 13), n.breaks = 13)


e2 <- ggplot(eigr, aes(x = pc,  y = varexp)) + 
  geom_line(aes(x = pc, y = cumvarexp), size = 1, color = colores[3]) + 
  theme_minimal()+
  labs(x = "Componente principal", 
       y = "Proporción de varianza", 
       title = "Varianza explicada acumulada",
       #subtitle = "Sentimiento del presidente",
       caption = "Fuente: elaboración propia") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) + 
  scale_x_continuous(limits = c(1, 13), n.breaks = 13) 

# Guardamos los gráficos 

grid.arrange(e1, e2, nrow = 1, ncol = 2)

ggsave(file="ComponentesP1.eps", width=6.5, height=4, dpi=300)

# Aplicamos los componentes principales a toda la base de datos (no solo el período in-sample).

PC <- scale(data[,3:15])%*%PCA1$rotation

# Ahora elegimos el orden.

favar.data <- cbind(in.sample[,2], PC.is[,1:4])

VARselect(favar.data, lag.max = 10, type = c("both"))

# Con el mismo criterio que antes, seleccionamos p=8.

favar <- VAR(favar.data, p= 8, type = "both")
summary(favar)

# Probamos autocorrelación en los residuos.

serial.test(favar) 

favar$varresult$X

# No rechazamos al 1%. Exportamos la tabla.

stargazer(var.dl$varresult$sentsmooth, favar$varresult$X,
          align = TRUE, 
          dep.var.labels = c("SentIndex", "SentIndex"),
          omit = c("sd1", 
                   "sd2", "sd3"),
          keep.stat = c("n", "ll", "rsq"),
          no.space = TRUE,
          type = "latex")


