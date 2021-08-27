#########################################
#                                       #
#     ¿Cómo se siente el presidente?    # 
#                                       # 
#       Kleiman, Pacheco y Riquelme     #  
#                                       #
#           3. Pronósticos              #
#########################################

# Ante cualquier error de código o duda, comunicarse con ariquelme@udesa.edu.ar

#### h = 1 ####

#### Esquema fijo ####

data1 <- ts(data, frequency = 365, start = c(2019,12))
PC <- ts(PC[,1:4], frequency = 365, start = c(2019,12))

pr.f.h1 <- ts(matrix(0, 58, 6), frequency = 365, start=c(2020,11))
colnames(pr.f.h1) <- c("ARIMA", "ARIMAX", "ETS", "ADL","VAR", "FAVAR")

h <- 1


for(i in 1:58){
  temp <- window(data1[,2], start = c(2019,12), end = 2020.192 + (i-1)/365)
  temp2 <- window(data1[,3:15], start = c(2019,12), end = 2020.192 + (i-1)/365)
  temp3 <- window(PC, start = c(2019,12), end = 2020.192 + (i-1)/365)
  
  # ARIMA 
  f1 <- Arima(temp, model=arima.1)
  forecast <- forecast(f1,h = h)
  pr.f.h1[i,1] <- forecast$mean[h]
  
  # ARIMAX
  f2 <- Arima(temp,model=arima.1,xreg=temp2)
  forecast2 <- forecast(f2$fitted,h=h)
  pr.f.h1[i,2] <- forecast2$mean[h]
  
  # ETS 
  f3 <- ets(temp, model=ets.1, use.initial.values=TRUE)
  pre <- forecast(f3, n.ahead=h)
  pr.f.h1[i,3] <- pre$mean[h]
  
  # FAVAR
  f4 <- VAR(cbind(temp, temp3), p = 6, type= "trend")
  forecast4<-forecast(f4, h=h)
  pr.f.h1[i,6] <- forecast4$forecast$temp$mean[h]
}

# ADL. Seleccionamos el orden. 

order.adl.dl <- auto_ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                            casosarg + muertosarg + vacunasarg + maxtemp + 
                            mintemp + muertes.arg.rel + casos.arg.rel, 
                          data = in.sample[,-1], max_order = 8)

count <- 2

for(i in 1:58){
  temp2 <-window(data1[,-1],start = c(2019,12), end = 2020.192 + (i-1)/365)
  colnames(temp2) <- c("sentsmooth","twfav", "twret", "reservasbcra", "tasaint", "basemon", "tcdolar", "casosarg",          
                       "muertosarg", "vacunasarg", "maxtemp", "mintemp",
                       "muertes.arg.rel", "casos.arg.rel")
  
  # El orden del ADL es el conseguido al aplicar el modelo al período in-sample.
  
  adl.dl <- ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                   casosarg + muertosarg + vacunasarg + maxtemp + 
                   mintemp + muertes.arg.rel + casos.arg.rel, 
                 data = temp2, order = as.vector(order.adl.dl$best_order))
  
  # Estimamos el ADL pero con la función dynlm.
  
  adl<-adl.dl$full_formula
  adl.1 <- dynlm(adl, data = temp2)
  forecast2 <- predict(adl.1$fitted.values,h=1)
  pr.f.h1[i,4] <- forecast2$mean[h]
  print(count) 
  count = count + 1
}

# Realizamos los pronósticos con el VAR. Construimos la serie 
# diferenciando las variables que son I(1).

reservas.est <- diff(data1[,5])

dolar.est <- diff(data1[,8])

data.diff<-cbind(data1[-1,-c(1,5,8)])

# Pronosticamos.

for(i in 1:58){ 
  temp2 <- window(data.diff, start = 2019.033, end = 2020.192 + (i-1)/365)
  f5 <- VAR(temp2, p = 6, type = "trend")
  forecast1<- forecast(f5, h=h)
  pr.f.h1[i,5] <- forecast1$forecast$data1..1...c.1..5..8...sentsmooth$mean[h]
}

# Dado que los loops tardan mucho tiempo en correr, descargaremos una base de datos con los
# valores pronosticados desde Github.

pr.f.h1 <- read.csv("https://raw.githubusercontent.com/tomas-pacheco/ComoSeSienteElPresidente/main/Forecasts/pr.f.h1.csv")
pr.f.h1<-ts(pr.f.h1,frequency = 365, start = c(2020,11))

# Graficamos los pronósticos.

autoplot(ts.union(out.of.sample[,2], pr.f.h1[,1], pr.f.h1[,2],pr.f.h1[,3],
                  pr.f.h1[,4],pr.f.h1[,5], pr.f.h1[,6]), size = 0.7) +
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX",
                                           "ETS","ADL","VAR", "FAVAR"), 
                     values = c("#4b4b4b", colores[1], colores[2],colores[3],
                                colores[4], colores[5], colores[6]))+
  theme_minimal() +  
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Tiempo", 
       y = "Sentimiento del presidente", 
       title = "Pronósticos con esquema fijo",
       subtitle = "Un paso adelante (h=1)",
       caption = "Fuente: elaboración propia") +
  guides(colour = guide_legend(nrow = 1))

#### Esquema recursivo ####

data1 <- ts(data, frequency = 365, start = c(2019,12))

PC<-ts(PC[,1:4], frequency = 365, start = c(2019,12))

pr.rec.h1 <- ts(matrix(0, 58, 6), frequency = 365, start=c(2020,11))
colnames(pr.rec.h1) <- c("ARIMA", "ARIMAX", "ETS", "ADL","VAR","FAVAR")

h <- 1

for(i in 1:58){
  temp<-window(data1[,2], start = c(2019,12), end = 2020.192 + (i-1)/365)
  temp2 <-window(data1[,3:15], start = c(2019,12), end = 2020.192 + (i-1)/365)
  temp3 <- window(PC, start = c(2019,12), end = 2020.192 + (i-1)/365)
  
  # ARIMA 
  f1 <- auto.arima(temp)
  forecast <- forecast(f1,h = h)
  pr.rec.h1[i,1] <- forecast$mean[h]
  
  #ARIMAX
  f2 <- Arima(temp,model=auto.arima(temp), newxreg=temp2)
  forecast2 <- forecast(f2$fitted,h=h)
  pr.rec.h1[i,2] <- forecast2$mean[h]
  
  #ETS 
  f3 <- ets(temp)
  pre <- forecast(f3, n.ahead=h)
  pr.rec.h1[i,3] <- pre$mean[h]
  
  # FAVAR
  a<-VARselect(cbind(temp, temp3), lag.max = 13, type = "const")$selection[1]
  f4 <- VAR(cbind(temp, temp3), p = a, type= "trend")
  forecast4<-forecast(f4, h=h)
  pr.rec.h1[i,6] <- forecast4$forecast$temp$mean[h] 
}

# ADL.

count <- 2
for(i in 1:58){
  temp2 <-window(data1[,-1], start = c(2019,12), end = 2020.192 + (i-1)/365)
  colnames(temp2) <- c("sentsmooth","twfav", "twret", "reservasbcra", "tasaint", "basemon", "tcdolar", "casosarg",          
                       "muertosarg", "vacunasarg", "maxtemp", "mintemp","muertes.arg.rel", "casos.arg.rel")
  
  # Actualizamos el orden del ADL
  
  order.adl.dl <- auto_ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                              casosarg + muertosarg + vacunasarg + maxtemp + 
                              mintemp + muertes.arg.rel + casos.arg.rel, 
                            data = temp2, max_order = 5)
  
  adl.dl <- ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                   casosarg + muertosarg + vacunasarg + maxtemp + 
                   mintemp + muertes.arg.rel + casos.arg.rel, 
                 data = temp2, order = as.vector(order.adl.dl$best_order))
  
  # Estimamos el ADL pero con la función dynlm
  
  adl<-adl.dl$full_formula
  adl.1 <- dynlm(adl, data = adl.dl$data)
  forecast2 <- predict(adl.1$fitted.values,h=1)
  pr.rec.h1[i,4] <- forecast2$mean[h]
  print(count) 
  count = count + 1
}

# VAR.

# anterior 

reservas.est <- diff(data1[,5])

dolar.est <- diff(data1[,8])

data.diff<-cbind(data1[-1,-c(1,5,8)], reservas.est, dolar.est)

data.diff  <- ts(data.diff, frequency = 365, start = c(2019,12))

prueba <- c()

for(i in 1:58){ 
  temp2<-window(data.diff, start = c(2019,12), end = 2020.192 + (i-1)/365)
  a<-VARselect(temp2, lag.max = 10, type = "trend")$selection[1]
  prueba <- cbind(prueba,a)
  f5 <- VAR(temp2, p = a, type = "trend")
  forecast1<- forecast(f5, h=h)
  pr.rec.h1[i,5] <- forecast1$forecast$data1..1...c.1..5..8...sentsmooth$mean[h]
}

table(prueba)

# Importamos los pronósticos.

pr.rec.h1 <- read.csv("https://raw.githubusercontent.com/tomas-pacheco/ComoSeSienteElPresidente/main/Forecasts/pr.rec.h1.csv")
pr.rec.h1 <- ts(pr.rec.h1, frequency = 365, start = c(2020,11))

# Gráficamos.

autoplot(ts.union(out.of.sample[,2], pr.rec.h1[,1], pr.rec.h1[,2],pr.rec.h1[,3],
                  pr.rec.h1[,4],pr.rec.h1[,5], pr.rec.h1[,6]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX",
                                           "ETS","ADL","VAR", "FAVAR"), 
                     values = c("#4b4b4b", colores[1], colores[2],colores[3],
                                colores[4], colores[5], colores[6]))+
  theme_minimal() +  
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Tiempo", 
       y = "Sentimiento del presidente", 
       title = "Pronósticos con esquema recursivo",
       subtitle = "Un paso adelante (h=1)",
       caption = "Fuente: elaboración propia") +
  guides(colour = guide_legend(nrow = 1))


#### Esquema rolling ####

data1 <- ts(data, frequency = 365, start = c(2019,12))
PC<-ts(PC[,1:4], frequency = 365, start = c(2019,12))

pr.rol.h1 <- ts(matrix(0, 58, 6), frequency = 365, start=c(2020,11))
colnames(pr.rec.h1) <- c("ARIMA", "ARIMAX", "ETS", "ADL","VAR","FAVAR")

h<-1

for(i in 1:58){
  temp<-window(data1[,2], start = 2019.030 + (i-1)/365, end = 2020.192 + (i-1)/365)
  temp2 <-window(data1[,3:15], start = 2019.030 + (i-1)/365, end = 2020.192 + (i-1)/365)
  temp3 <- window(PC, start = 2019.030 + (i-1)/365, end = 2020.192 + (i-1)/365)
  
  # ARIMA 
  f1 <- auto.arima(temp)
  forecast <- forecast(f1,h = h)
  pr.rol.h1[i,1] <- forecast$mean[h]
  
  # ARIMAX
  f2 <- Arima(temp,model=auto.arima(temp), newxreg=temp2)
  forecast2 <- forecast(f2$fitted,h=h)
  pr.rol.h1[i,2] <- forecast2$mean[h]
  
  # ETS 
  f3 <- ets(temp)
  pre <- forecast(f3, n.ahead=h)
  pr.rol.h1[i,3] <- pre$mean[h]
  
  # FAVAR
  a<-VARselect(cbind(temp, temp3), lag.max = 13, type = "const")$selection[1]
  f4 <- VAR(cbind(temp, temp3), p = a, type= "trend")
  forecast4<-forecast(f4, h=h)
  pr.rol.h1[i,6] <- forecast4$forecast$temp$mean[h] 
}

#ADL. 

count <- 2

for(i in 1:58){
  temp2 <-window(data1[,-1], start = 2019.030 + (i-1)/365, end = 2020.192 + (i-1)/365)
  colnames(temp2) <- c("sentsmooth","twfav", "twret", "reservasbcra", "tasaint", "basemon", "tcdolar", "casosarg",          
                       "muertosarg", "vacunasarg", "maxtemp", "mintemp", "muertes.arg.rel", "casos.arg.rel")
  
  # Actualizamos el orden del ADL
  
  order.adl.dl <- auto_ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                              casosarg + muertosarg + vacunasarg + maxtemp + 
                              mintemp + muertes.arg.rel + casos.arg.rel, 
                            data = temp2, max_order = 5)
  
  adl.dl <- ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                   casosarg + muertosarg + vacunasarg + maxtemp + 
                   mintemp + muertes.arg.rel + casos.arg.rel, 
                 data = temp2, order = as.vector(order.adl.dl$best_order))
  
  # Estimamos el ADL pero con la función dynlm
  
  adl<-adl.dl$full_formula
  adl.1 <- dynlm(adl, data = adl.dl$data)
  forecast2 <- predict(adl.1$fitted.values,h=1)
  pr.rol.h1[i,4] <- forecast2$mean[h]
  print(count) 
  count = count + 1
}

# VAR.

reservas.est <- diff(data1[,5])

dolar.est <- diff(data1[,8])

data.diff<-cbind(data1[-1,-1], reservas.est, dolar.est)

h <- 1

for(i in 1:58){ 
  temp2<-window(data.diff, start = 2019.033 + (i-1)/365, end = 2020.192 + (i-1)/365)
  a<-VARselect(data.diff, lag.max = 13, type = "const")$selection[1]
  f5 <- VAR(temp2, p = a, type = "trend")
  forecast1<- forecast(f5, h=h)
  pr.rol.h1[i,5] <- forecast1$forecast$data1..1...1..sentsmooth$mean[h]
}

# Importamos.

pr.rol.h1 <- read.csv("https://raw.githubusercontent.com/tomas-pacheco/ComoSeSienteElPresidente/main/Forecasts/pr.rol.h1.csv")
pr.rol.h1 <- ts(pr.rol.h1, frequency = 365, start = c(2020,11))

# Graficamos.

autoplot(ts.union(out.of.sample[,2], pr.rol.h1[,1], pr.rol.h1[,2],pr.rol.h1[,3],
                  pr.rol.h1[,4],pr.rol.h1[,5], pr.rol.h1[,6]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX",
                                           "ETS","ADL","VAR", "FAVAR"), 
                     values = c("#4b4b4b", colores[1], colores[2],colores[3],
                                colores[4], colores[5], colores[6]))+
  theme_minimal() +  
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Tiempo", 
       y = "Sentimiento del presidente", 
       title = "Pronósticos con esquema 'rolling'",
       subtitle = "Un paso adelante (h=1)",
       caption = "Fuente: elaboración propia") +
  guides(colour = guide_legend(nrow = 1))


#### h = 2 ####

#### Esquema fijo ####

# Con esta cantidad de pasos adelante, perdemos la primera observación.

data1 <- ts(data, frequency = 365, start = c(2019,12))
PC<-ts(PC[,1:4], frequency = 365, start = c(2019,12))

pr.f.h2 <- ts(matrix(0, 57, 6), frequency = 365, start=c(2020,11))
colnames(pr.f.h2) <- c("ARIMA", "ARIMAX", "ETS", "ADL","VAR","FAVAR")

h<-2

for(i in 1:57){
  temp<-window(data1[,2], start = c(2019,12), end = 2020.192 + (i-1)/365)
  temp2 <-window(data1[,3:15], start = c(2019,12), end = 2020.192 + (i-1)/365)
  temp3 <- window(PC, start = c(2019,12), end = 2020.192 + (i-1)/365)
  
  # ARIMA 
  f1 <- Arima(temp, model=arima.1)
  forecast <- forecast(f1,h = h)
  pr.f.h2[i,1] <- forecast$mean[h]
  
  # ARIMAX
  f2 <- Arima(temp,model=arima.1,xreg=temp2)
  forecast2 <- forecast(f2$fitted,h=h)
  pr.f.h2[i,2] <- forecast2$mean[h]
  
  # ETS 
  f3 <- ets(temp, model=ets.1, use.initial.values=TRUE)
  pre <- forecast(f3, n.ahead=h)
  pr.f.h2[i,3] <- pre$mean[h]
  
  # FAVAR
  a<-VARselect(cbind(temp, temp3), lag.max = 13, type = "const")$selection[1]
  f4 <- VAR(cbind(temp, temp3), p = a, type= "trend")
  forecast4<-forecast(f4, h=h)
  pr.f.h2[i,6] <- forecast4$forecast$temp$mean[h]
  
}

# ADL.

order.adl.dl <- auto_ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                            casosarg + muertosarg + vacunasarg + maxtemp + 
                            mintemp + muertes.arg.rel + casos.arg.rel, 
                          data = in.sample[,-1], max_order = 5)

count <- 2
for(i in 1:57){
  temp2 <-window(data1[,-1],start = c(2019,12), end = 2020.192 + (i-1)/365)
  colnames(temp2) <- c("sentsmooth","twfav", "twret", "reservasbcra", "tasaint", "basemon", "tcdolar", "casosarg",          
                       "muertosarg", "vacunasarg", "maxtemp", "mintemp",
                       "muertes.arg.rel", "casos.arg.rel")
  
  # El orden del ADL es el conseguido al aplicar el modelo al período in-sample 
  
  adl.dl <- ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                   casosarg + muertosarg + vacunasarg + maxtemp + 
                   mintemp + muertes.arg.rel + casos.arg.rel, 
                 data = temp2, order = as.vector(order.adl.dl$best_order))
  
  # Estimamos el ADL pero con la función dynlm
  
  adl<-adl.dl$full_formula
  adl.1 <- dynlm(adl, data = temp2)
  forecast2 <- predict(adl.1$fitted.values,h=h)
  pr.f.h2[i,4] <- forecast2$mean[h]
  print(count) 
  count = count + 1
}

# VAR.

reservas.est <- diff(data1[,5])

dolar.est <- diff(data1[,8])

data.diff<-cbind(data1[-1,-1], reservas.est, dolar.est)

for(i in 1:57){ 
  temp2<-window(data.diff, start = c(2019,12), end = 2020.192 + (i-1)/365)
  f5 <- VAR(temp2, p = 6, type = "trend")
  forecast1<- forecast(f5, h=h)
  pr.f.h2[i,5] <- forecast1$forecast$data1..1...1..sentsmooth$mean[h]
}

# Importamos.

pr.f.h2 <- read.csv("https://raw.githubusercontent.com/tomas-pacheco/ComoSeSienteElPresidente/main/Forecasts/pr.f.h2.csv")
pr.f.h2 <- ts(pr.f.h2, frequency = 365, start = c(2020,11))

# Graficamos.

autoplot(ts.union(out.of.sample[2:58,2], pr.f.h2[,1], pr.f.h2[,2],pr.f.h2[,3],
                  pr.f.h2[,4],pr.f.h2[,5], pr.f.h2[,6]), size = 0.7) +
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX",
                                           "ETS","ADL","VAR", "FAVAR"), 
                     values = c("#4b4b4b", colores[1], colores[2],colores[3],
                                colores[4], colores[5], colores[6]))+
  theme_minimal() +  
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Tiempo", 
       y = "Sentimiento del presidente", 
       title = "Pronósticos con esquema fijo",
       subtitle = "Dos pasos adelante (h=2)",
       caption = "Fuente: elaboración propia") +
  guides(colour = guide_legend(nrow = 1))


#### Esquema recursivo ####

data1 <- ts(data, frequency = 365, start = c(2019,12))
PC<-ts(PC[,1:4], frequency = 365, start = c(2019,12))

pr.rec.h2 <- ts(matrix(0, 57, 6), frequency = 365, start=c(2020,11))
colnames(pr.rec.h2) <- c("ARIMA", "ARIMAX", "ETS", "ADL","VAR", "FAVAR")

h<-2

for(i in 1:57){
  temp<-window(data1[,2], start = c(2019,12), end = 2020.192 + (i-1)/365)
  temp2 <-window(data1[,3:15], start = c(2019,12), end = 2020.192 + (i-1)/365)
  temp3 <- window(PC, start = c(2019,12), end = 2020.192 + (i-1)/365)
  
  # ARIMA 
  f1 <- auto.arima(temp)
  forecast <- forecast(f1,h = h)
  pr.rec.h2[i,1] <- forecast$mean[h]
  
  # ARIMAX
  f2 <- Arima(temp,model=f1, newxreg=temp2)
  forecast2 <- forecast(f2$fitted,h=h)
  pr.rec.h2[i,2] <- forecast2$mean[h]
  
  # ETS 
  f3 <- ets(temp)
  pre <- forecast(f3, n.ahead=h)
  pr.rec.h2[i,3] <- pre$mean[h]
  
  # FAVAR
  a<-VARselect(cbind(temp, temp3), lag.max = 13, type = "const")$selection[1]
  f4 <- VAR(cbind(temp, temp3), p = a, type= "trend")
  forecast4<-forecast(f4, h=h)
  pr.rec.h2[i,6] <- forecast4$forecast$temp$mean[h]
}

# ADL.

count <- 2

for(i in 1:57){
  temp2 <-window(data1[,-1], start = c(2019,12), end = 2020.192 + (i-1)/365)
  colnames(temp2) <- c("sentsmooth","twfav", "twret", "reservasbcra", "tasaint", "basemon", "tcdolar", "casosarg",          
                       "muertosarg", "vacunasarg", "maxtemp", "mintemp","muertes.arg.rel", "casos.arg.rel")
  
  # Actualizamos el orden del ADL
  
  order.adl.dl <- auto_ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                              casosarg + muertosarg + vacunasarg + maxtemp + 
                              mintemp +  muertes.arg.rel + casos.arg.rel, 
                            data = temp2, max_order = 5)
  
  adl.dl <- ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                   casosarg + muertosarg + vacunasarg + maxtemp + 
                   mintemp  + muertes.arg.rel + casos.arg.rel, 
                 data = temp2, order = as.vector(order.adl.dl$best_order))
  
  # Estimamos el ADL pero con la función dynlm
  
  adl<-adl.dl$full_formula
  adl.1 <- dynlm(adl, data = adl.dl$data)
  forecast2 <- predict(adl.1$fitted.values,h=h)
  pr.rec.h2[i,4] <- forecast2$mean[h]
  print(count) 
  count = count + 1
}

# VAR.

reservas.est <- diff(data1[,5])

dolar.est <- diff(data1[,8])

data.diff<-cbind(data1[-1,-1], reservas.est, dolar.est)

for(i in 1:57){ 
  temp2 <-window(data.diff, start = c(2019,12), end = 2020.192 + (i-1)/365)
  a<-VARselect(data.diff, lag.max = 13, type = "trend")
  b<-a$selection
  c<-b[1]
  f5 <- VAR(temp2, p = c, type = "trend")
  forecast1<- forecast(f5, h=h)
  pr.rec.h2[i,5] <- forecast1$forecast$data1..1...1..sentsmooth$mean[h]
}

# Importamos.

pr.rec.h2 <- read.csv("https://raw.githubusercontent.com/tomas-pacheco/ComoSeSienteElPresidente/main/Forecasts/pr.rec.h2.csv")
pr.rec.h2 <- ts(pr.rec.h2, frequency = 365, start = c(2020,11))

# Graficamos.

autoplot(ts.union(out.of.sample[2:58,2], pr.rec.h2[,1], pr.rec.h2[,2],pr.rec.h2[,3],
                  pr.rec.h2[,4],pr.rec.h2[,5], pr.rec.h2[,6]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX",
                                           "ETS","ADL","VAR", "FAVAR"), 
                     values = c("#4b4b4b", colores[1], colores[2],colores[3],
                                colores[4], colores[5], colores[6]))+
  theme_minimal() +  
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Tiempo", 
       y = "Sentimiento del presidente", 
       title = "Pronósticos con esquema recursivo",
       subtitle = "Dos pasos adelante (h=2)",
       caption = "Fuente: elaboración propia") +
  guides(colour = guide_legend(nrow = 1))


#### Esquema rolling ####

data1 <- ts(data, frequency = 365, start = c(2019,12))
PC<-ts(PC[,1:4], frequency = 365, start = c(2019,12))

pr.rol.h2 <- ts(matrix(0, 57, 6), frequency = 365, start=c(2020,11))
colnames(pr.rol.h2) <- c("ARIMA", "ARIMAX", "ETS", "ADL","VAR","FAVAR")

h<-2

for(i in 1:57){
  temp<-window(data1[,2], start = 2019.030 + (i-1)/365, end = 2020.192 + (i-1)/365)
  temp2 <-window(data1[,3:15], start = 2019.030 + (i-1)/365, end = 2020.192 + (i-1)/365)
  temp3 <- window(PC, start = 2019.030 + (i-1)/365, end = 2020.192 + (i-1)/365)
  
  # ARIMA 
  f1 <- auto.arima(temp)
  forecast <- forecast(f1,h = h)
  pr.rol.h2[i,1] <- forecast$mean[h]
  
  # ARIMAX
  f2 <- Arima(temp,model=auto.arima(temp), newxreg=temp2)
  forecast2 <- forecast(f2$fitted,h=h)
  pr.rol.h2[i,2] <- forecast2$mean[h]
  
  # ETS 
  f3 <- ets(temp)
  pre <- forecast(f3, n.ahead=h)
  pr.rol.h2[i,3] <- pre$mean[h]
  
  # FAVAR
  a<-VARselect(cbind(temp, temp3), lag.max = 13, type = "const")$selection[1]
  f4 <- VAR(cbind(temp, temp3), p = a, type= "trend")
  forecast4<-forecast(f4, h=h)
  pr.rol.h2[i,6] <- forecast4$forecast$temp$mean[h]
  
}

# ADL.

count <- 2

for(i in 1:57){
  temp2 <-window(data1[,-1], start = 2019.030 + (i-1)/365, end = 2020.192 + (i-1)/365)
  colnames(temp2) <- c("sentsmooth","twfav", "twret", "reservasbcra", "tasaint", "basemon", "tcdolar", "casosarg",          
                       "muertosarg", "vacunasarg", "maxtemp", "mintemp", "muertes.arg.rel", "casos.arg.rel")
  
  # Actualizamos el orden del ADL
  
  order.adl.dl <- auto_ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                              casosarg + muertosarg + vacunasarg + maxtemp + 
                              mintemp  + muertes.arg.rel + casos.arg.rel, 
                            data = temp2, max_order = 5)
  
  adl.dl <- ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                   casosarg + muertosarg + vacunasarg + maxtemp + 
                   mintemp + muertes.arg.rel + casos.arg.rel, 
                 data = temp2, order = as.vector(order.adl.dl$best_order))
  
  # Estimamos el ADL pero con la función dynlm
  adl<-adl.dl$full_formula
  adl.1 <- dynlm(adl, data = adl.dl$data)
  forecast2 <- predict(adl.1$fitted.values,h=h)
  pr.rol.h2[i,4] <- forecast2$mean[h]
  print(count) 
  count = count + 1
}

# VAR.

reservas.est <- diff(data1[,5])

dolar.est <- diff(data1[,8])

data.diff<-cbind(data1[-1,-1], reservas.est, dolar.est)

for(i in 1:57){ 
  temp2<-window(data.diff, start = 2019.033 + (i-1)/365, end = 2020.192 + (i-1)/365)
  a<-VARselect(data.diff, lag.max = 13, type = "const")
  b<-a$selection
  c<-b[1]
  f5 <- VAR(temp2, p = c, type = "trend")
  forecast1<- forecast(f5, h=h)
  pr.rol.h2[i,5] <- forecast1$forecast$data1..1...1..sentsmooth$mean[h]
}

# Importamos.

pr.rol.h2 <- read.csv("https://raw.githubusercontent.com/tomas-pacheco/ComoSeSienteElPresidente/main/Forecasts/pr.rol.h2.csv")
pr.rol.h2 <- ts(pr.rol.h2, frequency = 365, start = c(2020,11))

# Graficamos.

autoplot(ts.union(out.of.sample[2:58,2], pr.rol.h2[,1], pr.rol.h2[,2],pr.rol.h2[,3],
                  pr.rol.h2[,4],pr.rol.h2[,5], pr.rol.h2[,6]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX",
                                           "ETS","ADL","VAR", "FAVAR"), 
                     values = c("#4b4b4b", colores[1], colores[2],colores[3],
                                colores[4], colores[5], colores[6]))+
  theme_minimal() +  
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Tiempo", 
       y = "Sentimiento del presidente", 
       title = "Pronósticos con esquema 'rolling'",
       subtitle = "Dos pasos adelante (h=2)",
       caption = "Fuente: elaboración propia") +
  guides(colour = guide_legend(nrow = 1))


#### h = 7 ####

#### Esquema fijo ####

# Perdemos las primeras seis observaciones.

data1 <- ts(data, frequency = 365, start = c(2019,12))
PC<-ts(PC[,1:4], frequency = 365, start = c(2019,12))

pr.f.h7 <- ts(matrix(0, 52, 6), frequency = 365, start=c(2020,11))
colnames(pr.f.h7) <- c("ARIMA", "ARIMAX", "ETS", "ADL","VAR","FAVAR")

h <- 7

for(i in 1:52){
  temp<-window(data1[,2], start = c(2019,12), end = 2020.192 + (i-1)/365)
  temp2 <-window(data1[,3:15], start = c(2019,12), end = 2020.192 + (i-1)/365)
  temp3 <- window(PC, start = c(2019,12), end = 2020.192 + (i-1)/365)
  
  # ARIMA 
  f1 <- Arima(temp, model=arima.1)
  forecast <- forecast(f1,h = 7)
  pr.f.h7[i,1] <- forecast$mean[7]
  
  # ARIMAX
  f2 <- Arima(temp,model=arima.1,xreg=temp2)
  forecast2 <- forecast(f2$fitted,h=h)
  pr.f.h7[i,2] <- forecast2$mean[h]
  
  # ETS 
  f3 <- ets(temp, model=ets.1, use.initial.values=TRUE)
  pre <- forecast(f3, n.ahead=h)
  pr.f.h7[i,3] <- pre$mean[h]
  
  # FAVAR
  a<-VARselect(cbind(temp, temp3), lag.max = 13, type = "const")$selection[1]
  f4 <- VAR(cbind(temp, temp3), p = a, type= "trend")
  forecast4<-forecast(f4, h=h)
  pr.f.h7[i,6] <- forecast4$forecast$temp$mean[h]
}

# ADL.


order.adl.dl <- auto_ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                            casosarg + muertosarg + vacunasarg + maxtemp + 
                            mintemp + muertes.arg.rel + casos.arg.rel|
                            mes01 + mes02 + mes03 +  mes04 + mes05 + mes06 +
                            mes07 + mes08 + mes09 +  mes10 + mes11, 
                          data = in.sample[,-1], max_order = 5)
count <- 2

for(i in 1:52){
  temp2 <-window(data1[,-1],start = c(2019,12), end = 2020.192 + (i-1)/365)
  colnames(temp2) <- c("sentsmooth","twfav", "twret", "reservasbcra", "tasaint", "basemon", "tcdolar", "casosarg",          
                       "muertosarg", "vacunasarg", "maxtemp", "mintemp",
                       "muertes.arg.rel", "casos.arg.rel")
  
  # El orden del ADL es el conseguido al aplicar el modelo al período in sample 
  
  adl.dl <- ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                   casosarg + muertosarg + vacunasarg + maxtemp + 
                   mintemp + muertes.arg.rel + casos.arg.rel, 
                 data = temp2, order = as.vector(order.adl.dl$best_order))
  
  # Estimamos el ADL pero con la función dynlm
  
  adl<-adl.dl$full_formula
  adl.1 <- dynlm(adl, data = temp2)
  forecast2 <- predict(adl.1$fitted.values,h=h)
  pr.f.h7[i,4] <- forecast2$mean[h]
  print(count) 
  count = count + 1
}

# VAR.

reservas.est <- diff(data1[,5])

dolar.est <- diff(data1[,8])

data.diff<-cbind(data1[-1,-1], reservas.est, dolar.est)

h<-7

for(i in 1:52){ 
  temp2<-window(data.diff, start = c(2019,12), end = 2020.192 + (i-1)/365)
  f5 <- VAR(temp2, p = 6, type = "trend")
  forecast1<- forecast(f5, h=h)
  pr.f.h7[i,5] <- forecast1$forecast$data1..1...1..sentsmooth$mean[h]
}

# Importamos.

pr.f.h7 <- read.csv("https://raw.githubusercontent.com/tomas-pacheco/ComoSeSienteElPresidente/main/Forecasts/pr.f.h7.csv")
pr.f.h7 <- ts(pr.f.h7, frequency = 365, start = c(2020,11))

# Graficamos.

autoplot(ts.union(out.of.sample[7:58,2], pr.f.h7[,1], pr.f.h7[,2], pr.f.h7[,3],
                  pr.f.h7[,4], pr.f.h7[,5], pr.f.h7[,6]), size = 0.7) +
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX",
                                           "ETS","ADL","VAR", "FAVAR"), 
                     values = c("#4b4b4b", colores[1], colores[2],colores[3],
                                colores[4], colores[5], colores[6]))+
  theme_minimal() +  
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Tiempo", 
       y = "Sentimiento del presidente", 
       title = "Pronósticos con esquema fijo",
       subtitle = "Siete pasos adelante (h=7)",
       caption = "Fuente: elaboración propia") +
  guides(colour = guide_legend(nrow = 1))


#### Esquema recursivo ####

data1 <- ts(data, frequency = 365, start = c(2019,12))
PC<-ts(PC[,1:4], frequency = 365, start = c(2019,12))

pr.rec.h7 <- ts(matrix(0, 52, 6), frequency = 365, start=c(2020,11))
colnames(pr.rec.h7) <- c("ARIMA", "ARIMAX", "ETS", "ADL","VAR","FAVAR")

h<-7

for(i in 1:52){
  temp<-window(data1[,2], start = c(2019,12), end = 2020.192 + (i-1)/365)
  temp2 <-window(data1[,3:15], start = c(2019,12), end = 2020.192 + (i-1)/365)
  temp3 <- window(PC, start = c(2019,12), end = 2020.192 + (i-1)/365)
  
  # ARIMA 
  f1 <- auto.arima(temp)
  forecast <- forecast(f1,h = h)
  pr.rec.h7[i,1] <- forecast$mean[h]
  
  # ARIMAX
  f2 <- Arima(temp,model=f1, newxreg=temp2)
  forecast2 <- forecast(f2$fitted,h=h)
  pr.rec.h7[i,2] <- forecast2$mean[h]
  
  # ETS 
  f3 <- ets(temp)
  pre <- forecast(f3, n.ahead=h)
  pr.rec.h7[i,3] <- pre$mean[h]
  
  # FAVAR
  a<-VARselect(cbind(temp, temp3), lag.max = 13, type = "const")$selection[1]
  f4 <- VAR(cbind(temp, temp3), p = a, type= "trend")
  forecast4<-forecast(f4, h=h)
  pr.rec.h7[i,6] <- forecast4$forecast$temp$mean[h]
  
}

# ADL.

count <- 2

for(i in 1:52){
  temp2 <-window(data1[,-1], start = c(2019,12), end = 2020.192 + (i-1)/365)
  colnames(temp2) <- c("sentsmooth","twfav", "twret", "reservasbcra", "tasaint", "basemon", "tcdolar", "casosarg",          
                       "muertosarg", "vacunasarg", "maxtemp", "mintemp","muertes.arg.rel", "casos.arg.rel")
  
  # Actualizamos el orden del ADL
  
  order.adl.dl <- auto_ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                              casosarg + muertosarg + vacunasarg + maxtemp + 
                              mintemp  + muertes.arg.rel + casos.arg.rel, 
                            data = temp2, max_order = 5)
  
  adl.dl <- ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                   casosarg + muertosarg + vacunasarg + maxtemp + 
                   mintemp  + muertes.arg.rel + casos.arg.rel, 
                 data = temp2, order = as.vector(order.adl.dl$best_order))
  
  # Estimamos el ADL pero con la función dynlm
  
  adl<-adl.dl$full_formula
  adl.1 <- dynlm(adl, data = adl.dl$data)
  forecast2 <- predict(adl.1$fitted.values,h=h)
  pr.rec.h7[i,4] <- forecast2$mean[h]
  print(count) 
  count = count + 1
}

# VAR.

reservas.est <- diff(data1[,5])

dolar.est <- diff(data1[,8])

data.diff<-cbind(data1[-1,-1], reservas.est, dolar.est)

for(i in 1:52){ 
  temp2<-window(data.diff, start = c(2019,12), end = 2020.192 + (i-1)/365)
  a<-VARselect(data.diff, lag.max = 13, type = "const")
  b<-a$selection
  c<-b[1]
  f5 <- VAR(temp2, p = c, type = "trend")
  forecast1<- forecast(f5, h=h)
  pr.rec.h7[i,5] <- forecast1$forecast$data1..1...1..sentsmooth$mean[h]
}

# Importamos.

pr.rec.h7 <- read.csv("https://raw.githubusercontent.com/tomas-pacheco/ComoSeSienteElPresidente/main/Forecasts/pr.rec.h7.csv")
pr.rec.h7 <- ts(pr.rec.h7, frequency = 365, start = c(2020,11))

# Graficamos.

autoplot(ts.union(out.of.sample[7:58,2], pr.rec.h7[,1], pr.rec.h7[,2],pr.rec.h7[,3],
                  pr.rec.h7[,4],pr.rec.h7[,5], pr.rec.h7[,6]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX",
                                           "ETS","ADL","VAR", "FAVAR"), 
                     values = c("#4b4b4b", colores[1], colores[2],colores[3],
                                colores[4], colores[5], colores[6]))+
  theme_minimal() +  
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Tiempo", 
       y = "Sentimiento del presidente", 
       title = "Pronósticos con esquema recursivo",
       subtitle = "Siete pasos adelante (h=7)",
       caption = "Fuente: elaboración propia") +
  guides(colour = guide_legend(nrow = 1))


#### Esquema rolling ####

data1 <- ts(data, frequency = 365, start = c(2019,12))
PC<-ts(PC[,1:4], frequency = 365, start = c(2019,12))

pr.rol.h7 <- ts(matrix(0, 52, 6), frequency = 365, start=c(2020,11))
colnames(pr.rol.h7) <- c("ARIMA", "ARIMAX", "ETS", "ADL","VAR", "FAVAR")

h<-7

for(i in 1:52){
  temp<-window(data1[,2], start = 2019.030 + (i-1)/365, end = 2020.192 + (i-1)/365)
  temp2 <-window(data1[,3:15], start = 2019.030 + (i-1)/365, end = 2020.192 + (i-1)/365)
  temp3 <- window(PC, start = 2019.030 + (i-1)/365, end = 2020.192 + (i-1)/365)
  
  # ARIMA 
  f1 <- auto.arima(temp)
  forecast <- forecast(f1,h = h)
  pr.rol.h7[i,1] <- forecast$mean[h]
  
  # ARIMAX
  f2 <- Arima(temp,model=f1, newxreg=temp2)
  forecast2 <- forecast(f2$fitted,h=h)
  pr.rol.h7[i,2] <- forecast2$mean[h]
  
  # ETS 
  f3 <- ets(temp)
  pre <- forecast(f3, n.ahead=h)
  pr.rol.h7[i,3] <- pre$mean[h]
  
  # FAVAR
  a<-VARselect(cbind(temp, temp3), lag.max = 13, type = "const")$selection[1]
  f4 <- VAR(cbind(temp, temp3), p = a, type= "trend")
  forecast4<-forecast(f4, h=h)
  pr.rol.h7[i,6] <- forecast4$forecast$temp$mean[h]
}

# ADL.

count <- 2

for(i in 1:52){
  temp2 <-window(data1[,-1], start = 2019.030 + (i-1)/365, end = 2020.192 + (i-1)/365)
  colnames(temp2) <- c("sentsmooth","twfav", "twret", "reservasbcra", "tasaint", "basemon", "tcdolar", "casosarg",          
                       "muertosarg", "vacunasarg", "maxtemp", "mintemp", "muertes.arg.rel", "casos.arg.rel")
  
  # Actualizamos el orden del ADL
  
  order.adl.dl <- auto_ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                              casosarg + muertosarg + vacunasarg + maxtemp + 
                              mintemp  + muertes.arg.rel + casos.arg.rel, 
                            data = temp2, max_order = 5)
  
  adl.dl <- ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                   casosarg + muertosarg + vacunasarg + maxtemp + 
                   mintemp  + muertes.arg.rel + casos.arg.rel, 
                 data = temp2, order = as.vector(order.adl.dl$best_order))
  
  # Estimamos el ADL pero con la función dynlm
  
  adl<-adl.dl$full_formula
  adl.1 <- dynlm(adl, data = adl.dl$data)
  forecast2 <- predict(adl.1$fitted.values,h=1)
  pr.rol.h7[i,4] <- forecast2$mean[h]
  print(count) 
  count = count + 1
}

# VAR.

reservas.est <- diff(data1[,5])

dolar.est <- diff(data1[,8])

data.diff<-cbind(data1[-1,-1], reservas.est, dolar.est)

for(i in 1:52){ 
  temp2<-window(data.diff, start = 2019.033 + (i-1)/365, end = 2020.192 + (i-1)/365)
  a<-VARselect(data.diff, lag.max = 13, type = "const")
  b<-a$selection
  c<-b[1]
  f5 <- VAR(temp2, p = c, type = "trend")
  forecast1<- forecast(f5, h=h)
  pr.rol.h7[i,5] <- forecast1$forecast$data1..1...1..sentsmooth$mean[h]
}


# Importamos.

pr.rol.h7 <- read.csv("https://raw.githubusercontent.com/tomas-pacheco/ComoSeSienteElPresidente/main/Forecasts/pr.rol.h7.csv")
pr.rol.h7 <- ts(pr.rol.h7, frequency = 365, start = c(2020,11))

# Graficamos.

autoplot(ts.union(out.of.sample[7:58,2], pr.rol.h7[,1], pr.rol.h7[,2],pr.rol.h7[,3],
                  pr.rol.h7[,4],pr.rol.h7[,5], pr.rol.h7[,6]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX",
                                           "ETS","ADL","VAR", "FAVAR"), 
                     values = c("#4b4b4b", colores[1], colores[2],colores[3],
                                colores[4], colores[5], colores[6]))+
  theme_minimal() +  
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Tiempo", 
       y = "Sentimiento del presidente", 
       title = "Pronósticos con esquema 'rolling'",
       subtitle = "Siete pasos adelante (h=7)",
       caption = "Fuente: elaboración propia") +
  guides(colour = guide_legend(nrow = 1))


#### Pronósticos con Bagging ####

# En esta sección haremos los pronósticos usando la técnica de bagging.
# Extraeremos aleatoriamente 100 muestras bootstrap.
# No haremos pronósticos con el modelo ADL dada la restriccion computacional.

# Comenzamos extrayendo las muestras bootstrap.

library(quantmod)

# Plantamos la semilla para poder replicar resultados.

set.seed(444)

# Aquí extraemos las muestras de las variables de nuestra base.

for (i in 2:15) {
  a <- bld.mbb.bootstrap(data[,i], 1000) %>% as.data.frame() %>% ts(start=c(2019,12), frequency=365)
  assign(paste("data.bag.",i,sep = ""),ts(a, frequency = 365, start = c(2019,12)))
}

# Aquí extraemos las muestras de los componentes principales.

for (i in 1:4) {
  b <- bld.mbb.bootstrap(PC[,i], 1000) %>% as.data.frame() %>% ts(start=c(2019,12), frequency=365)
  assign(paste("PC.bag.",i,sep = ""),ts(b, frequency = 365, start = c(2019,12)))
}

# Ahora continuamos con los pronósticos.


#### h = 1 ####

#### Esquema fijo ####

pr.f.h1.b <- matrix(nrow=58,ncol=5, NA)

h <- 1

for(i in 1:58){
  pr.arima <- matrix(nrow=1,ncol=1000,NA)
  pr.arimax <- matrix(nrow=1,ncol=1000,NA)
  pr.ets <- matrix(nrow=1,ncol=1000,NA)
  
  for (j in 1:1000) {
    temp <-window(data.bag.2[,j], start = c(2019,12), end = 2020.192 + (i-1)/365)
    data.var.bag.ex <- cbind(data.bag.3[,j],data.bag.4[,j],data.bag.5[,j],data.bag.6[,j],
                             data.bag.7[,j],data.bag.8[,j],data.bag.9[,j],data.bag.10[,j],
                             data.bag.11[,j],data.bag.12[,j],data.bag.13[,j],data.bag.14[,j],
                             data.bag.15[,j])
    temp2 <- window(data.var.bag.ex, start = c(2019,12), end = 2020.192 + (i-1)/365)
    
    # ARIMA 
    f1 <- Arima(temp, model=arima.1)
    forecast <- forecast(f1,h = h)
    pr.arima[1,j] <- forecast$mean[h]
    
    # ARIMAX
    f2 <- Arima(temp,model=arima.1,xreg=temp2)
    forecast2 <- forecast(f2$fitted,h=h)
    pr.arimax[1,j] <- forecast2$mean[h]
    
    # ETS 
    f1 <- ets(temp, model=ets.1, use.initial.values=TRUE)
    pre <- forecast(f1, n.ahead=h)
    pr.ets[1,j] <- pre$mean[h]
    
    print(j)
  } 
  pr.f.h1.b[i,1] <- mean(pr.arima)
  pr.f.h1.b[i,2] <- mean(pr.arimax)
  pr.f.h1.b[i,3] <- mean(pr.ets)
  print(i)
}


# Realizamos los pronósticos con el VAR.

for(i in 1:58){
  pr.var <- matrix(nrow=1,ncol=1000,NA)
  for (j in 1:1000) {
    data.var.bag.ex <- window(cbind(data.bag.2[,j],data.bag.3[,j],data.bag.5[,j],data.bag.6[,j],data.bag.7[,j], data.bag.9[,j],data.bag.10[,j],data.bag.11[,j],data.bag.12[,j],data.bag.13[,j],data.bag.14[,j],data.bag.15[,j]), start = c(2019,12), end = 2020.192 + (i-1)/365)
    var.bag.ex.diff<- window(cbind(diff(data.bag.4[,j]), diff(data.bag.8[,j])), start = c(2019,12), end = 2020.192 + (i-1)/365)
    f5 <- VAR(cbind(data.var.bag.ex[-1,],var.bag.ex.diff), p = 6, type = "trend")
    pr.var[1,j] <- forecast(f5)$forecast$data.var.bag.ex..1....data.bag.2...j.$mean[h]
    print(j)
  }
  pr.f.h1.b[i,5] <- mean(pr.var)
  print(paste("VAMOS:", i, "PERIODOS"))
}


# Realizamos los pronósticos para el FAVAR.

for(i in 1:58){
  pr.favar <- matrix(nrow=1,ncol=1000,NA)
  for (j in 1:1000){
    temp <-window(data.bag.2[,j], start = c(2019,12), end = 2020.192 + (i-1)/365)
    temp3 <- window(cbind(PC.bag.1[,j],PC.bag.2[,j],PC.bag.3[,j],PC.bag.4[,j]),
                    start = c(2019,12), end = 2020.192 + (i-1)/365)
    
    # FAVAR
    f4 <- VAR(cbind(temp, temp3[,1]), p = 6, type= "trend")
    forecast4<-forecast(f4, h=h)
    pr.favar[1,j] <- forecast4$forecast$temp$mean[h]
    print(j)
  } 
  
  pr.f.h1.b[i,4] <- mean(pr.favar,na.rm = TRUE)
  print(i)
}


# Importamos los pronósticos ya hechos.

pr.f.h1.b <- read.csv("https://raw.githubusercontent.com/tomas-pacheco/ComoSeSienteElPresidente/main/Forecasts/pr.f.h1.b.csv")
pr.f.h1.b <- ts(pr.f.h1.b, frequency = 365, start = c(2020,11))

# Graficamos.

autoplot(ts.union(out.of.sample[1:58,2], pr.f.h1.b[,1], pr.f.h1.b[,2], pr.f.h1.b[,3], pr.f.h1.b[,4], pr.f.h1.b[,5]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX",
                                           "ETS","VAR", "FAVAR"), 
                     values = c("#4b4b4b", colores[1], colores[2],colores[3],
                                colores[5], colores[6]))+
  theme_minimal() +  
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Tiempo", 
       y = "Sentimiento del presidente", 
       title = "Pronósticos 'bagged' con esquema fijo",
       subtitle = "Un paso adelante (h=1)",
       caption = "Fuente: elaboración propia") +
  guides(colour = guide_legend(nrow = 1))


#### Esquema recursivo ####

pr.rec.h1.b <- matrix(nrow=58,ncol=5, NA)

h <- 1

for(i in 1:58){
  pr.arima <- matrix(nrow=1,ncol=1000,NA)
  pr.arimax <- matrix(nrow=1,ncol=1000,NA)
  pr.ets <- matrix(nrow=1,ncol=1000,NA)
  pr.favar <- matrix(nrow=1,ncol=1000,NA)
  
  # Estimamos los modelos que luego aplicaremos a las muestras bootstrap
  
  temp<-window(data1[,2], start = c(2019,12), end = 2020.192 + (i-1)/365)
  temp2 <-window(data1[,3:15], start = c(2019,12), end = 2020.192 + (i-1)/365)
  temp3 <- window(PC, start = c(2019,12), end = 2020.192 + (i-1)/365)
  
  # ARIMA 
  arima.1 <- auto.arima(temp)
  
  
  # ETS 
  ets.1 <- ets(temp)
  
  # FAVAR
  favar.1 <-VARselect(cbind(temp, temp3), lag.max = 13, type = "trend")$selection[1]
  
  
  
  for (j in 1:1000) {
    temp <-window(data.bag.2[,j], start = c(2019,12), end = 2020.192 + (i-1)/365)
    data.var.bag.ex <- cbind(data.bag.3[,j],data.bag.4[,j],data.bag.5[,j],data.bag.6[,j],
                             data.bag.7[,j],data.bag.8[,j],data.bag.9[,j],data.bag.10[,j],
                             data.bag.11[,j],data.bag.12[,j],data.bag.13[,j],data.bag.14[,j],
                             data.bag.15[,j])
    temp2 <- window(data.var.bag.ex, start = c(2019,12), end = 2020.192 + (i-1)/365)
    temp3 <- window(cbind(PC.bag.1[,j],PC.bag.2[,j],PC.bag.3[,j],
                          PC.bag.4[,j]), start = c(2019,12), end = 2020.192 + (i-1)/365)
    # ARIMA 
    f1 <- Arima(temp, model = arima.1)
    forecast <- forecast(f1,h = h)
    pr.arima[1,j] <- forecast$mean[h]
    
    # ARIMAX
    f2 <- Arima(temp,model=arima.1,newxreg=temp2)
    forecast2 <- forecast(f2$fitted,h=h)
    pr.arimax[1,j] <- forecast2$mean[h]
    
    # ETS 
    f1 <- ets(temp, model = ets.1, use.initial.values = TRUE)
    pre <- forecast(f1, n.ahead=h)
    pr.ets[1,j] <- pre$mean[h]
    
    # FAVAR
    f4 <- VAR(cbind(temp, temp3), p = favar.1, type= "trend")
    forecast4<-forecast(f4, h=h)
    pr.favar[1,j] <- forecast4$forecast$temp$mean[h]
    
    print(j)
  } 
  
  pr.rec.h1.b[i,1] <- mean(pr.arima)
  pr.rec.h1.b[i,2] <- mean(pr.arimax)
  pr.rec.h1.b[i,3] <- mean(pr.ets)
  pr.rec.h1.b[i,4] <- mean(pr.favar, na.rm = TRUE)
  print(i)
}

# VAR.

for(i in 1:58){
  pr.var <- matrix(nrow=1,ncol=1000,NA)
  
  # Estimamos el modelo que luego aplicaremos a las muestras bootstrap
  
  temp<-window(data1[,2], start = c(2019,12), end = 2020.192 + (i-1)/365)
  temp3 <- window(PC, start = c(2019,12), end = 2020.192 + (i-1)/365)
  
  # VAR
  var.1 <-VARselect(cbind(temp, temp3), lag.max = 13, type = "trend")$selection[1]
  
  
  for (j in 1:1000){
    data.var.bag.ex <- window(cbind(data.bag.2[,j],data.bag.3[,j],data.bag.5[,j],data.bag.6[,j],data.bag.7[,j], data.bag.9[,j],data.bag.10[,j],data.bag.11[,j],data.bag.12[,j],data.bag.13[,j],data.bag.14[,j],data.bag.15[,j]), start = c(2019,12), end = 2020.192 + (i-1)/365)
    var.bag.ex.diff<- window(cbind(diff(data.bag.4[,j]), diff(data.bag.8[,j])), start = c(2019,12), end = 2020.192 + (i-1)/365)
    f5 <- VAR(cbind(data.var.bag.ex[-1,],var.bag.ex.diff), p =var.1, type = "trend")
    pr.var[1,j] <- forecast(f5)$forecast$data.var.bag.ex..1....data.bag.2...j.$mean[h]
    print(j)
  }
  pr.rec.h1.b[i,5] <- mean(pr.var)
  print(paste("VAMOS:", i, "PERIODOS"))
}


# Importamos.

pr.rec.h1.b <- read.csv("https://raw.githubusercontent.com/tomas-pacheco/ComoSeSienteElPresidente/main/Forecasts/pr.rec.h1.b.csv")
pr.rec.h1.b <- ts(pr.rec.h1.b, frequency = 365, start = c(2020,11))

# Graficamos.

autoplot(ts.union(out.of.sample[1:58, 2], pr.rec.h1.b[,1], pr.rec.h1.b[,2], pr.rec.h1.b[,3], pr.rec.h1.b[,4], pr.rec.h1.b[,5]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX",
                                           "ETS","VAR", "FAVAR"), 
                     values = c("#4b4b4b", colores[1], colores[2],colores[3],
                                colores[5], colores[6]))+
  theme_minimal() +  
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Tiempo", 
       y = "Sentimiento del presidente", 
       title = "Pronósticos 'bagged' con esquema recursivo",
       subtitle = "Un paso adelante (h=1)",
       caption = "Fuente: elaboración propia") +
  guides(colour = guide_legend(nrow = 1))


#### Esquema rolling ####


pr.rol.h1.b <- matrix(nrow=58,ncol=5, NA)

h <- 1

for(i in 1:58){
  pr.arima <- matrix(nrow=1,ncol=1000,NA)
  pr.arimax <- matrix(nrow=1,ncol=1000,NA)
  pr.ets <- matrix(nrow=1,ncol=1000,NA)
  pr.favar <- matrix(nrow=1,ncol=1000,NA)
  
  # Estimamos los modelos que luego aplicaremos a las muestras bootstrap
  
  temp<-window(data1[,2], start = 2019.030 + (i-1)/365, end = 2020.192 + (i-1)/365)
  temp2 <-window(data1[,3:15], start = 2019.030 + (i-1)/365, end = 2020.192 + (i-1)/365)
  temp3 <- window(PC, start = 2019.030 + (i-1)/365, end = 2020.192 + (i-1)/365)
  
  # ARIMA 
  arima.1 <- auto.arima(temp)
  
  
  # ETS 
  ets.1 <- ets(temp)
  
  # FAVAR
  favar.1 <-VARselect(cbind(temp, temp3), lag.max = 13, type = "trend")$selection[1]
  
  
  for (j in 1:1000) {
    temp <-window(data.bag.2[,j], start = 2019.030 + (i-1)/365, end = 2020.192 + (i-1)/365)
    data.var.bag.ex <- cbind(data.bag.3[,j],data.bag.4[,j],data.bag.5[,j],data.bag.6[,j],
                             data.bag.7[,j],data.bag.8[,j],data.bag.9[,j],data.bag.10[,j],
                             data.bag.11[,j],data.bag.12[,j],data.bag.13[,j],data.bag.14[,j],
                             data.bag.15[,j])
    temp2 <- window(data.var.bag.ex, start = 2019.030 + (i-1)/365, end = 2020.192 + (i-1)/365)
    temp3 <- window(cbind(PC.bag.1[,j],PC.bag.2[,j],PC.bag.3[,j],PC.bag.4[,j]),
                    start = 2019.030 + (i-1)/365, end = 2020.192 + (i-1)/365)
    # ARIMA 
    f1 <- Arima(temp, model = arima.1)
    forecast <- forecast(f1,h = h)
    pr.arima[1,j] <- forecast$mean[h]
    
    # ARIMAX
    f2 <- Arima(temp,model = arima.1,newxreg=temp2)
    forecast2 <- forecast(f2$fitted,h=h)
    pr.arimax[1,j] <- forecast2$mean[h]
    
    # ETS 
    f1 <- ets(temp, model = ets.1, use.initial.values = TRUE)
    pre <- forecast(f1, n.ahead=h)
    pr.ets[1,j] <- pre$mean[h]
    
    # FAVAR
    f4 <- VAR(cbind(temp, temp3), p = favar.1, type= "trend")
    forecast4<-forecast(f4, h=h)
    pr.favar[1,j] <- forecast4$forecast$temp$mean[h]
    
    print(j)
  } 
  
  pr.rol.h1.b[i,1] <- mean(pr.arima)
  pr.rol.h1.b[i,2] <- mean(pr.arimax)
  pr.rol.h1.b[i,3] <- mean(pr.ets)
  pr.rol.h1.b[i,4] <- mean(pr.favar, na.rm = TRUE)
  print(i)
}

# VAR.

for(i in 1:58){
  pr.var <- matrix(nrow=1,ncol=1000,NA)
  
  # Estimamos el modelos que luego aplicaremos a las series bootstrap
  temp<-window(data1[,2], start = 2019.030 + (i-1)/365, end = 2020.192 + (i-1)/365)
  temp3 <- window(PC, start = 2019.030 + (i-1)/365, end = 2020.192 + (i-1)/365)
  
  # VAR
  var.1 <-VARselect(cbind(temp, temp3), lag.max = 13, type = "trend")$selection[1]
  
  for (j in 1:1000) {
    data.var.bag.ex <- window(cbind(data.bag.2[,j],data.bag.3[,j],data.bag.5[,j],
                                    data.bag.6[,j],data.bag.7[,j], data.bag.9[,j],
                                    data.bag.10[,j],data.bag.11[,j],data.bag.12[,j],
                                    data.bag.13[,j],data.bag.14[,j],data.bag.15[,j]), 
                              start = 2019.030 + (i-1)/365, end = 2020.192 + (i-1)/365)
    var.bag.ex.diff.1<- window(cbind(data.bag.4[,j], data.bag.8[,j]), start = 2019.030 + (i-1)/365, end = 2020.192 + (i-1)/365)
    var.bag.ex.diff <- diff(var.bag.ex.diff.1)
    f5 <- VAR(cbind(data.var.bag.ex[-1,],var.bag.ex.diff), p = var.1, type = "trend")
    pr.var[1,j] <- forecast(f5)$forecast$data.var.bag.ex..1....data.bag.2...j.$mean[h]
    print(j)
  }
  pr.rol.h1.b[i,5] <- mean(pr.var)
  print(paste("VAMOS:", i, "PERIODOS"))
}


# Importamos.

pr.rol.h1.b <- read.csv("https://raw.githubusercontent.com/tomas-pacheco/ComoSeSienteElPresidente/main/Forecasts/pr.rol.h1.b.csv")
pr.rol.h1.b <- ts(pr.rol.h1.b, frequency = 365, start = c(2020,11))

# Graficamos.

autoplot(ts.union(out.of.sample[,2], pr.rol.h1.b[,1], pr.rol.h1.b[,2], 
                  pr.rol.h1.b[,3], pr.rol.h1.b[,4], pr.rol.h1.b[,5]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX",
                                           "ETS","VAR", "FAVAR"), 
                     values = c("#4b4b4b", colores[1], colores[2],colores[3],
                                colores[5], colores[6]))+
  theme_minimal() +  
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Tiempo", 
       y = "Sentimiento del presidente", 
       title = "Pronósticos 'bagged' con esquema 'rolling'",
       subtitle = "Un paso adelante (h=1)",
       caption = "Fuente: elaboración propia") +
  guides(colour = guide_legend(nrow = 1))


#### h = 2 ####

#### Esquema fijo ####

pr.f.h2.b <- matrix(nrow=57,ncol=5, NA)

h <- 2

for(i in 1:57){
  pr.arima <- matrix(nrow=1,ncol=1000,NA)
  pr.arimax <- matrix(nrow=1,ncol=1000,NA)
  pr.ets <- matrix(nrow=1,ncol=1000,NA)
  pr.favar <-  matrix(nrow=1,ncol=1000,NA)
  
  for (j in 1:1000) {
    temp<-window(data.bag.2[,j], start = c(2019,12), end = 2020.192 + (i-1)/365)
    data.var.bag.ex <- cbind(data.bag.3[,j],data.bag.4[,j],data.bag.5[,j],
                             data.bag.6[,j],data.bag.7[,j],data.bag.8[,j],data.bag.9[,j],
                             data.bag.10[,j],data.bag.11[,j],data.bag.12[,j],
                             data.bag.13[,j],data.bag.14[,j],data.bag.15[,j])
    temp2 <-window(data.var.bag.ex, start = c(2019,12), end = 2020.192 + (i-1)/365)
    temp3 <- window(cbind(PC.bag.1[,j],PC.bag.2[,j],PC.bag.3[,j],PC.bag.4[,j]),
                    start = c(2019,12), end = 2020.192 + (i-1)/365)
    
    # ARIMA 
    f1 <- Arima(temp, model=arima.1)
    forecast <- forecast(f1,h = h)
    pr.arima[1,j] <- forecast$mean[h]
    
    # ARIMAX
    f2 <- Arima(temp,model=arima.1,xreg=temp2)
    forecast2 <- forecast(f2$fitted,h=h)
    pr.arimax[1,j] <- forecast2$mean[h]
    
    # ETS 
    f1 <- ets(temp, model=ets.1, use.initial.values=TRUE)
    pre <- forecast(f1, n.ahead=h)
    pr.ets[1,j] <- pre$mean[h]
    
    # FAVAR
    f4 <- VAR(cbind(temp, temp3), p = 8, type= "trend")
    forecast4<-forecast(f4, h=h)
    pr.favar[1,j] <- forecast4$forecast$temp$mean[h]
    print(j)
  } 
  pr.f.h2.b[i,1] <- mean(pr.arima)
  pr.f.h2.b[i,2] <- mean(pr.arimax)
  pr.f.h2.b[i,3] <- mean(pr.ets)
  pr.f.h2.b[i,4] <- mean(pr.favar)
  print(i)
}

# VAR.

for(i in 1:57){
  pr.var <- matrix(nrow=1,ncol=1000,NA)
  for (j in 1:1000){
    data.var.bag.ex <- window(cbind(data.bag.2[,j],data.bag.3[,j],data.bag.5[,j],
                                    data.bag.6[,j],data.bag.7[,j], data.bag.9[,j],
                                    data.bag.10[,j],data.bag.11[,j],data.bag.12[,j],
                                    data.bag.13[,j],data.bag.14[,j],data.bag.15[,j]),
                              start = c(2019,12), end = 2020.192 + (i-1)/365)
    var.bag.ex.diff<- window(cbind(diff(data.bag.4[,j]), diff(data.bag.8[,j])),
                             start = c(2019,12), end = 2020.192 + (i-1)/365)
    f5 <- VAR(cbind(data.var.bag.ex[-1,], var.bag.ex.diff), p = 6, type = "trend")
    pr.var[1,j] <- forecast(f5)$forecast$data.var.bag.ex..1....data.bag.2...j.$mean[h]
    print(j)
  }
  pr.f.h2.b[i,5] <- mean(pr.var)
  print(paste("VAMOS:", i, "PERIODOS"))
}

# Importamos.

pr.f.h2.b <- read.csv("https://raw.githubusercontent.com/tomas-pacheco/ComoSeSienteElPresidente/main/Forecasts/pr.f.h2.b.csv")
pr.f.h2.b <- ts(pr.f.h2.b, frequency = 365, start = c(2020,11))

# Graficamos.

autoplot(ts.union(out.of.sample[2:58,2], pr.f.h2.b[,1], pr.f.h2.b[,2], pr.f.h2.b[,3],
                  pr.f.h2.b[,4], pr.f.h2.b[,5]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX",
                                           "ETS","VAR", "FAVAR"), 
                     values = c("#4b4b4b", colores[1], colores[2],colores[3],
                                colores[5], colores[6]))+
  theme_minimal() +  
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Tiempo", 
       y = "Sentimiento del presidente", 
       title = "Pronósticos 'bagged' con esquema fijo",
       subtitle = "Dos pasos adelante (h=2)",
       caption = "Fuente: elaboración propia") +
  guides(colour = guide_legend(nrow = 1))


#### Esquema recursivo ####

pr.rec.h2.b <- matrix(nrow=57,ncol=5, NA)

h <- 2

for(i in 1:57){
  pr.arima <- matrix(nrow=1,ncol=1000,NA)
  pr.arimax <- matrix(nrow=1,ncol=1000,NA)
  pr.ets <- matrix(nrow=1,ncol=1000,NA)
  pr.favar <-  matrix(nrow=1,ncol=1000,NA)
  
  # Estimamos los modelos que luego aplicaremos a las muestras bootstrap
  
  temp<-window(data1[,2], start = c(2019,12), end = 2020.192 + (i-1)/365)
  temp2 <-window(data1[,3:15], start = c(2019,12), end = 2020.192 + (i-1)/365)
  temp3 <- window(PC, start = c(2019,12), end = 2020.192 + (i-1)/365)
  
  # ARIMA 
  arima.1 <- auto.arima(temp)
  
  
  # ETS 
  ets.1 <- ets(temp)
  
  # FAVAR
  favar.1 <-VARselect(cbind(temp, temp3), lag.max = 13, type = "trend")$selection[1]
  
  for (j in 1:1000){
    temp<-window(data.bag.2[,j], start = c(2019,12), end = 2020.192 + (i-1)/365)
    data.var.bag.ex <- cbind(data.bag.3[,j],data.bag.4[,j],data.bag.5[,j],data.bag.6[,j],data.bag.7[,j],
                             data.bag.8[,j],data.bag.9[,j],data.bag.10[,j],data.bag.11[,j],data.bag.12[,j],
                             data.bag.13[,j],data.bag.14[,j],data.bag.15[,j])
    temp2 <-window(data.var.bag.ex, start = c(2019,12), end = 2020.192 + (i-1)/365)
    temp3 <- window(cbind(PC.bag.1[,j],PC.bag.2[,j],PC.bag.3[,j],PC.bag.4[,j]),
                    start = c(2019,12), end = 2020.192 + (i-1)/365)
    
    # ARIMA 
    f1 <- Arima(temp, model = arima.1)
    forecast <- forecast(f1,h = h)
    pr.arima[1,j] <- forecast$mean[h]
    
    #ARIMAX
    f2 <- Arima(temp,model=arima.1,newxreg=temp2)
    forecast2 <- forecast(f2$fitted,h=h)
    pr.arimax[1,j] <- forecast2$mean[h]
    
    #ETS 
    f1 <- ets(temp, model = ets.1, use.initial.values = TRUE)
    pre <- forecast(f1, n.ahead=h)
    pr.ets[1,j] <- pre$mean[h]
    
    # FAVAR
    f4 <- VAR(cbind(temp, temp3), p = favar.1, type= "trend")
    forecast4<-forecast(f4, h=h)
    pr.favar[1,j] <- forecast4$forecast$temp$mean[h]
    
    print(j)
  } 
  
  pr.rec.h2.b[i,1] <- mean(pr.arima)
  pr.rec.h2.b[i,2] <- mean(pr.arimax)
  pr.rec.h2.b[i,3] <- mean(pr.ets)
  pr.rec.h2.b[i,4] <- mean(pr.favar, na.rm = TRUE)
  print(i)
}

# VAR.

for(i in 1:57){
  pr.var <- matrix(nrow=1,ncol=1000,NA)
  temp<-window(data1[,2], start = c(2019,12), end = 2020.192 + (i-1)/365)
  temp3 <- window(PC, start = c(2019,12), end = 2020.192 + (i-1)/365)
  
  # VAR
  var.1 <-VARselect(cbind(temp, temp3), lag.max = 13, type = "trend")$selection[1]
  
  for (j in 1:1000) {
    data.var.bag.ex <- window(cbind(data.bag.2[,j],data.bag.3[,j],data.bag.5[,j],data.bag.6[,j],data.bag.7[,j], 
                                    data.bag.9[,j],data.bag.10[,j],data.bag.11[,j],data.bag.12[,j],data.bag.13[,j],
                                    data.bag.14[,j],data.bag.15[,j]), start = c(2019,12), end = 2020.192 + (i-1)/365)
    var.bag.ex.diff<- window(cbind(diff(data.bag.4[,j]), diff(data.bag.8[,j])), start = c(2019,12), end = 2020.192 + (i-1)/365)
    f5 <- VAR(cbind(data.var.bag.ex[-1,],var.bag.ex.diff), p = var.1, type = "trend")
    pr.var[1,j]<- forecast(f5)$forecast$data.var.bag.ex..1....data.bag.2...j.$mean[h]
    print(j)
  }
  pr.rec.h2.b[i,5] <- mean(pr.var)
  print(paste("VAMOS:", i, "PERIODOS"))
}


# Importamos.

pr.rec.h2.b <- read.csv("https://raw.githubusercontent.com/tomas-pacheco/ComoSeSienteElPresidente/main/Forecasts/pr.rec.h2.b.csv")
pr.rec.h2.b <- ts(pr.rec.h2.b, frequency = 365, start = c(2020,11))

# Graficamos.

autoplot(ts.union(out.of.sample[2:58,2], pr.rec.h2.b[,1], pr.rec.h2.b[,2], pr.rec.h2.b[,3], pr.rec.h2.b[,4], pr.rec.h2.b[,5]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX",
                                           "ETS","VAR", "FAVAR"), 
                     values = c("#4b4b4b", colores[1], colores[2],colores[3],
                                colores[5], colores[6]))+
  theme_minimal() +  
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Tiempo", 
       y = "Sentimiento del presidente", 
       title = "Pronósticos 'bagged' con esquema recursivo",
       subtitle = "Dos pasos adelante (h=2)",
       caption = "Fuente: elaboración propia") +
  guides(colour = guide_legend(nrow = 1))

#### Esquema rolling ####

pr.rol.h2.b <- matrix(nrow=57,ncol=5, NA)

h <- 2

for(i in 1:57){
  pr.arima <- matrix(nrow=1,ncol=1000,NA)
  pr.arimax <- matrix(nrow=1,ncol=1000,NA)
  pr.ets <- matrix(nrow=1,ncol=1000,NA)
  pr.favar <-  matrix(nrow=1,ncol=1000,NA)
  
  # Estimamos los modelos que luego aplicaremos a las muestras bootstrap
  
  temp<-window(data1[,2], start = 2019.030 + (i-1)/365, end = 2020.192 + (i-1)/365)
  temp2 <-window(data1[,3:15], start = 2019.030 + (i-1)/365, end = 2020.192 + (i-1)/365)
  temp3 <- window(PC, start = 2019.030 + (i-1)/365, end = 2020.192 + (i-1)/365)
  
  # ARIMA 
  arima.1 <- auto.arima(temp)
  
  
  # ETS 
  ets.1 <- ets(temp)
  
  # FAVAR
  favar.1 <-VARselect(cbind(temp, temp3), lag.max = 13, type = "trend")$selection[1]
  
  
  for (j in 1:1000) {
    
    temp<-window(data.bag.2[,j], start = 2019.030 + (i-1)/365, end = 2020.192 + (i-1)/365)
    data.var.bag.ex <- cbind(data.bag.3[,j],data.bag.4[,j],data.bag.5[,j],
                             data.bag.6[,j],data.bag.7[,j],data.bag.8[,j],
                             data.bag.9[,j],data.bag.10[,j],data.bag.11[,j],
                             data.bag.12[,j],data.bag.13[,j],data.bag.14[,j],
                             data.bag.15[,j])
    temp2 <-window(data.var.bag.ex, start = c(2019,12), end = 2020.192 + (i-1)/365)
    temp3 <- window(cbind(PC.bag.1[,j],PC.bag.2[,j],PC.bag.3[,j],PC.bag.4[,j]), 
                    start = 2019.030 + (i-1)/365, end = 2020.192 + (i-1)/365)
    
    # ARIMA 
    f1 <- Arima(temp, model = arima.1)
    forecast <- forecast(f1,h = h)
    pr.arima[1,j] <- forecast$mean[h]
    
    #ARIMAX
    f2 <- Arima(temp,model=arima.1,newxreg=temp2)
    forecast2 <- forecast(f2$fitted,h=h)
    pr.arimax[1,j] <- forecast2$mean[h]
    
    #ETS 
    f1 <- ets(temp, model = ets.1, use.initial.values = TRUE)
    pre <- forecast(f1, n.ahead=h)
    pr.ets[1,j] <- pre$mean[h]
    
    # FAVAR
    f4 <- VAR(cbind(temp, temp3), p = favar.1, type= "trend")
    forecast4<-forecast(f4, h=h)
    pr.favar[1,j] <- forecast4$forecast$temp$mean[h]
    
    print(j)
  } 
  
  pr.rol.h2.b[i,1] <- mean(pr.arima)
  pr.rol.h2.b[i,2] <- mean(pr.arimax)
  pr.rol.h2.b[i,3] <- mean(pr.ets)
  pr.rol.h2.b[i,4] <- mean(pr.favar, na.rm = TRUE)
  print(i)
}

# VAR.

for(i in 1:57){
  pr.var <- matrix(nrow=1,ncol=1000,NA)
  
  # Estimamos los modelos que luego aplicaremos a las muestras bootstrap
  
  temp<-window(data1[,2], start = 2019.030 + (i-1)/365, end = 2020.192 + (i-1)/365)
  temp3 <- window(PC, start = 2019.030 + (i-1)/365, end = 2020.192 + (i-1)/365)
  
  # VAR
  var.1 <-VARselect(cbind(temp, temp3), lag.max = 13, type = "trend")$selection[1]
  
  for (j in 1:1000){
    data.var.bag.ex <- window(cbind(data.bag.2[,j],data.bag.3[,j],data.bag.5[,j],data.bag.6[,j],
                                    data.bag.7[,j], data.bag.9[,j],data.bag.10[,j],data.bag.11[,j],
                                    data.bag.12[,j],data.bag.13[,j],data.bag.14[,j],data.bag.15[,j]), 
                              start = 2019.030 + (i-1)/365, end = 2020.192 + (i-1)/365)
    var.bag.ex.diff.1<- window(cbind(data.bag.4[,j], data.bag.8[,j]), start = 2019.030 + (i-1)/365, end = 2020.192 + (i-1)/365)
    var.bag.ex.diff <- diff(var.bag.ex.diff.1)
    f5 <- VAR(cbind(data.var.bag.ex[-1,],var.bag.ex.diff), p = var.1, type = "trend")
    pr.var[1,j] <- forecast(f5)$forecast$data.var.bag.ex..1....data.bag.2...j.$mean[h]
    print(j)
  }
  pr.rol.h2.b[i,5] <- mean(pr.var)
  print(paste("VAMOS:", i, "PERIODOS"))
}


# Importamos.

pr.rol.h2.b <- read.csv("https://raw.githubusercontent.com/tomas-pacheco/ComoSeSienteElPresidente/main/Forecasts/pr.rol.h2.b.csv")
pr.rol.h2.b <- ts(pr.rol.h2.b, frequency = 365, start = c(2020,11))

# Graficamos.

autoplot(ts.union(out.of.sample[2:58,2], pr.rol.h2.b[,1], pr.rol.h2.b[,2],
                  pr.rol.h2.b[,3], pr.rol.h2.b[,4], pr.rol.h2.b[,5]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX",
                                           "ETS","VAR", "FAVAR"), 
                     values = c("#4b4b4b", colores[1], colores[2],colores[3],
                                colores[5], colores[6]))+
  theme_minimal() +  
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Tiempo", 
       y = "Sentimiento del presidente", 
       title = "Pronósticos 'bagged' con esquema 'rolling'",
       subtitle = "Dos pasos adelante (h=2)",
       caption = "Fuente: elaboración propia") +
  guides(colour = guide_legend(nrow = 1))


#### h = 7 ####

#### Esquema fijo ####

pr.f.h7.b <- matrix(nrow=52,ncol=5, NA)

h <- 7

for(i in 1:52){
  pr.arima <- matrix(nrow=1,ncol=1000,NA)
  pr.arimax <- matrix(nrow=1,ncol=1000,NA)
  pr.ets <- matrix(nrow=1,ncol=1000,NA)
  pr.favar <- matrix(nrow=1,ncol=1000,NA)
  
  for (j in 1:1000){
    temp<-window(data.bag.2[,j], start = c(2019,12), end = 2020.192 + (i-1)/365)
    data.var.bag.ex <- cbind(data.bag.3[,j],data.bag.4[,j],data.bag.5[,j],
                             data.bag.6[,j],data.bag.7[,j],data.bag.8[,j],
                             data.bag.9[,j],data.bag.10[,j],data.bag.11[,j],
                             data.bag.12[,j],data.bag.13[,j],data.bag.14[,j],
                             data.bag.15[,j])
    temp2 <-window(data.var.bag.ex, start = c(2019,12), end = 2020.192 + (i-1)/365)
    temp3 <- window(cbind(PC.bag.1[,j],PC.bag.2[,j],PC.bag.3[,j],PC.bag.4[,j]),
                    start = c(2019,12), end = 2020.192 + (i-1)/365)
    
    # ARIMA 
    f1 <- Arima(temp, model=arima.1)
    forecast <- forecast(f1,h = h)
    pr.arima[1,j] <- forecast$mean[h]
    
    # ARIMAX
    f2 <- Arima(temp,model=arima.1,newxreg=temp2)
    forecast2 <- forecast(f2$fitted,h=h)
    pr.arimax[1,j] <- forecast2$mean[h]
    
    # ETS 
    f1 <- ets(temp, model=ets.1, use.initial.values=TRUE)
    pre <- forecast(f1, n.ahead=h)
    pr.ets[1,j] <- pre$mean[h]
    
    # FAVAR
    f4 <- VAR(cbind(temp, temp3), p = 8, type= "trend")
    forecast4<-forecast(f4, h=h)
    pr.favar[1,j] <- forecast4$forecast$temp$mean[h]
    
    print(j)
  } 
  pr.f.h7.b[i,1] <- mean(pr.arima)
  pr.f.h7.b[i,2] <- mean(pr.arimax)
  pr.f.h7.b[i,3] <- mean(pr.ets)
  pr.f.h7.b[i,4] <- mean(pr.favar, na.rm = TRUE)
  print(i)
}

# VAR

for(i in 1:52){
  pr.var <- matrix(nrow=1,ncol=1000,NA)
  for (j in 1:1000) {
    data.var.bag.ex <- window(cbind(data.bag.2[,j],data.bag.3[,j],data.bag.5[,j],
                                    data.bag.6[,j],data.bag.7[,j], data.bag.9[,j],
                                    data.bag.10[,j],data.bag.11[,j],data.bag.12[,j],
                                    data.bag.13[,j],data.bag.14[,j],data.bag.15[,j]), 
                              start = c(2019,12), end = 2020.192 + (i-1)/365)
    var.bag.ex.diff<- window(cbind(diff(data.bag.4[,j]), diff(data.bag.8[,j])), start = c(2019,12), end = 2020.192 + (i-1)/365)
    f5 <- VAR(cbind(data.var.bag.ex[-1,],var.bag.ex.diff), p = 6, type = "trend")
    pr.var[1,j] <- forecast(f5)$forecast$data.var.bag.ex..1....data.bag.2...j.$mean[h]
    print(j)
  }
  pr.f.h7.b[i,5] <- mean(pr.var)
  print(paste("VAMOS:", i, "PERIODOS"))
}

# Importamos.

pr.f.h7.b <- read.csv("https://raw.githubusercontent.com/tomas-pacheco/ComoSeSienteElPresidente/main/Forecasts/pr.f.h7.b.csv")
pr.f.h7.b <- ts(pr.f.h7.b, frequency = 365, start = c(2020,11))

# Graficamos.

autoplot(ts.union(out.of.sample[7:58,2], pr.f.h7.b[,1], pr.f.h7.b[,2], pr.f.h7.b[,3],
                  pr.f.h7.b[,4], pr.f.h7.b[,5]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX",
                                           "ETS","VAR", "FAVAR"), 
                     values = c("#4b4b4b", colores[1], colores[2],colores[3],
                                colores[5], colores[6]))+
  theme_minimal() +  
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Tiempo", 
       y = "Sentimiento del presidente", 
       title = "Pronósticos 'bagged' con esquema fijo",
       subtitle = "Siete pasos adelante (h=7)",
       caption = "Fuente: elaboración propia") +
  guides(colour = guide_legend(nrow = 1))


#### Esquema recursivo ####

pr.rec.h7.b <- matrix(nrow=52,ncol=5, NA)

h <- 7

for(i in 1:52){
  pr.arima <- matrix(nrow=1,ncol=1000,NA)
  pr.arimax <- matrix(nrow=1,ncol=1000,NA)
  pr.ets <- matrix(nrow=1,ncol=1000,NA)
  pr.favar <- matrix(nrow=1,ncol=1000,NA)
  
  temp<-window(data1[,2], start = c(2019,12), end = 2020.192 + (i-1)/365)
  temp2 <-window(data1[,3:15], start = c(2019,12), end = 2020.192 + (i-1)/365)
  temp3 <- window(PC, start = c(2019,12), end = 2020.192 + (i-1)/365)
  
  # ARIMA 
  arima.1 <- auto.arima(temp)
  
  
  # ETS 
  ets.1 <- ets(temp)
  
  # FAVAR
  favar.1 <-VARselect(cbind(temp, temp3), lag.max = 13, type = "trend")$selection[1]
  
  for (j in 1:1000) {
    temp<-window(data.bag.2[,j], start = c(2019,12), end = 2020.192 + (i-1)/365)
    data.var.bag.ex <- cbind(data.bag.3[,j],data.bag.4[,j],data.bag.5[,j],data.bag.6[,j],data.bag.7[,j],
                             data.bag.8[,j],data.bag.9[,j],data.bag.10[,j],data.bag.11[,j],data.bag.12[,j],
                             data.bag.13[,j],data.bag.14[,j],data.bag.15[,j])
    temp2 <-window(data.var.bag.ex, start = c(2019,12), end = 2020.192 + (i-1)/365)
    temp3 <- window(cbind(PC.bag.1[,j],PC.bag.2[,j],PC.bag.3[,j],PC.bag.4[,j]), 
                    start = c(2019,12), end = 2020.192 + (i-1)/365)
    
    # ARIMA 
    f1 <- Arima(temp, model = arima.1)
    forecast <- forecast(f1,h = h)
    pr.arima[1,j] <- forecast$mean[h]
    
    # ARIMAX
    f2 <- Arima(temp,model=arima.1,newxreg=temp2)
    forecast2 <- forecast(f2$fitted,h=h)
    pr.arimax[1,j] <- forecast2$mean[h]
    
    # ETS 
    f1 <- ets(temp, model = ets.1, use.initial.values = TRUE)
    pre <- forecast(f1, n.ahead=h)
    pr.ets[1,j] <- pre$mean[h]
    
    # FAVAR
    f4 <- VAR(cbind(temp, temp3), p = favar.1, type= "trend")
    forecast4<-forecast(f4, h=h)
    pr.favar[1,j] <- forecast4$forecast$temp$mean[h]
    
    print(j)
  } 
  
  pr.rec.h7.b[i,1] <- mean(pr.arima)
  pr.rec.h7.b[i,2] <- mean(pr.arimax)
  pr.rec.h7.b[i,3] <- mean(pr.ets)
  pr.rec.h7.b[i,4] <- mean(pr.favar, na.rm = TRUE)
  print(i)
}

# VAR.

for(i in 1:52){
  pr.var <- matrix(nrow=1,ncol=1000,NA)
  temp<-window(data1[,2], start = c(2019,12), end = 2020.192 + (i-1)/365)
  temp3 <- window(PC, start = c(2019,12), end = 2020.192 + (i-1)/365)
  
  # VAR
  var.1 <-VARselect(cbind(temp, temp3), lag.max = 13, type = "trend")$selection[1]
  for (j in 1:1000) {
    data.var.bag.ex <- window(cbind(data.bag.2[,j],data.bag.3[,j],data.bag.5[,j],data.bag.6[,j],
                                    data.bag.7[,j], data.bag.9[,j],data.bag.10[,j],data.bag.11[,j],
                                    data.bag.12[,j],data.bag.13[,j],data.bag.14[,j],data.bag.15[,j]), 
                              start = c(2019,12), end = 2020.192 + (i-1)/365)
    var.bag.ex.diff<- window(cbind(diff(data.bag.4[,j]), diff(data.bag.8[,j])), start = c(2019,12), end = 2020.192 + (i-1)/365)
    f5 <- VAR(cbind(data.var.bag.ex[-1,], var.bag.ex.diff), p = var.1, type = "trend")
    pr.var[1,j]<- forecast(f5)$forecast$data.var.bag.ex..1....data.bag.2...j.$mean[h]
    print(j)
  }
  pr.rec.h7.b[i,5] <- mean(pr.var)
  print(paste("VAMOS:", i, "PERIODOS"))
}


# Importamos.

pr.rec.h7.b <- read.csv("https://raw.githubusercontent.com/tomas-pacheco/ComoSeSienteElPresidente/main/Forecasts/pr.rec.h7.b.csv")
pr.rec.h7.b <- ts(pr.rec.h7.b, frequency = 365, start = c(2020,11))


# Graficamos.

autoplot(ts.union(out.of.sample[7:58,2], pr.rec.h7.b[,1], pr.rec.h7.b[,2], pr.rec.h7.b[,3], pr.rec.h7.b[,4], pr.rec.h7.b[,5]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX",
                                           "ETS","VAR", "FAVAR"), 
                     values = c("#4b4b4b", colores[1], colores[2],colores[3],
                                colores[5], colores[6]))+
  theme_minimal() +  
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Tiempo", 
       y = "Sentimiento del presidente", 
       title = "Pronósticos 'bagged' con esquema recursivo",
       subtitle = "Siete pasos adelante (h=7)",
       caption = "Fuente: elaboración propia") +
  guides(colour = guide_legend(nrow = 1))


#### Esquema rolling ####

pr.rol.h7.b <- matrix(nrow=52,ncol=5, NA)

h<- 7

for(i in 1:52){
  pr.arima <- matrix(nrow=1,ncol=1000,NA)
  pr.arimax <- matrix(nrow=1,ncol=1000,NA)
  pr.ets <- matrix(nrow=1,ncol=1000,NA)
  pr.favar <- matrix(nrow=1,ncol=1000,NA)
  
  # Estimamos los modelos que luego aplicaremos a las muestras bootstrap
  
  temp<-window(data1[,2], start = 2019.030 + (i-1)/365, end = 2020.192 + (i-1)/365)
  temp2 <-window(data1[,3:15], start = 2019.030 + (i-1)/365, end = 2020.192 + (i-1)/365)
  temp3 <- window(PC, start = 2019.030 + (i-1)/365, end = 2020.192 + (i-1)/365)
  
  # ARIMA 
  arima.1 <- auto.arima(temp)
  
  
  # ETS 
  ets.1 <- ets(temp)
  
  # FAVAR
  favar.1 <-VARselect(cbind(temp, temp3), lag.max = 13, type = "trend")$selection[1]
  
  for (j in 1:1000) {
    temp<-window(data.bag.2[,j], start = 2019.030 + (i-1)/365, end = 2020.192 + (i-1)/365)
    data.var.bag.ex <- cbind(data.bag.3[,j],data.bag.4[,j],data.bag.5[,j],data.bag.6[,j],
                             data.bag.7[,j],data.bag.8[,j],data.bag.9[,j],data.bag.10[,j],
                             data.bag.11[,j],data.bag.12[,j],data.bag.13[,j],data.bag.14[,j],
                             data.bag.15[,j])
    temp2 <-window(data.var.bag.ex, start = 2019.030 + (i-1)/365, end = 2020.192 + (i-1)/365)
    temp3 <- window(cbind(PC.bag.1[,j],PC.bag.2[,j],PC.bag.3[,j],PC.bag.4[,j]), start = 2019.030 + (i-1)/365, end = 2020.192 + (i-1)/365)
    
    # ARIMA 
    f1 <- Arima(temp, model = arima.1)
    forecast <- forecast(f1,h = h)
    pr.arima[1,j] <- forecast$mean[h]
    
    # ARIMAX
    f2 <- Arima(temp,model=arima.1,newxreg=temp2)
    forecast2 <- forecast(f2$fitted,h=h)
    pr.arimax[1,j] <- forecast2$mean[h]
    
    # ETS 
    f1 <- ets(temp, model = ets.1, use.initial.values = TRUE)
    pre <- forecast(f1, n.ahead=h)
    pr.ets[1,j] <- pre$mean[h]
    
    # FAVAR
    f4 <- VAR(cbind(temp, temp3), p = favar.1, type= "trend")
    forecast4<-forecast(f4, h=h)
    pr.favar[1,j] <- forecast4$forecast$temp$mean[h]
    
    print(j)
  } 
  
  pr.rol.h7.b[i,1] <- mean(pr.arima)
  pr.rol.h7.b[i,2] <- mean(pr.arimax)
  pr.rol.h7.b[i,3] <- mean(pr.ets)
  pr.rol.h7.b[i,4] <- mean(pr.favar, na.rm = TRUE)
  print(i)
}

# VAR.

for(i in 1:52){
  pr.var <- matrix(nrow=1,ncol=1000,NA)
  
  # Estimamos el modelo que luego aplicaremos a las series bootstrap
  
  temp<-window(data1[,2], start = 2019.030 + (i-1)/365, end = 2020.192 + (i-1)/365)
  temp3 <- window(PC, start = 2019.030 + (i-1)/365, end = 2020.192 + (i-1)/365)
  
  # VAR
  var.1 <-VARselect(cbind(temp, temp3), lag.max = 13, type = "trend")$selection[1]
  
  for (j in 1:1000) {
    data.var.bag.ex <- window(cbind(data.bag.2[,j],data.bag.3[,j],data.bag.5[,j],data.bag.6[,j],
                                    data.bag.7[,j], data.bag.9[,j],data.bag.10[,j],data.bag.11[,j],
                                    data.bag.12[,j],data.bag.13[,j],data.bag.14[,j],data.bag.15[,j]), 
                              start = 2019.030 + (i-1)/365, end = 2020.192 + (i-1)/365)
    var.bag.ex.diff.1<- window(cbind(data.bag.4[,j], data.bag.8[,j]), start = 2019.030 + (i-1)/365, end = 2020.192 + (i-1)/365)
    var.bag.ex.diff <- diff(var.bag.ex.diff.1)
    f5 <- VAR(cbind(data.var.bag.ex[-1,],var.bag.ex.diff), p = var.1, type = "trend")
    pr.var[1,j] <- forecast(f5)$forecast$data.var.bag.ex..1....data.bag.2...j.$mean[h]
    print(j)
  }
  pr.rol.h7.b[i,5] <- mean(pr.var)
  print(paste("VAMOS:", i, "PERIODOS"))
}


# Importamos.

pr.rol.h7.b <- read.csv("https://raw.githubusercontent.com/tomas-pacheco/ComoSeSienteElPresidente/main/Forecasts/pr.rol.h7.b.csv")
pr.rol.h7.b <- ts(pr.rol.h7.b, frequency = 365, start = c(2020,11))

# Graficamos.

autoplot(ts.union(out.of.sample[7:58,2], pr.rol.h7.b[,1], pr.rol.h7.b[,2], pr.rol.h7.b[,3], pr.rol.h7.b[,4], pr.rol.h7.b[,5]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX",
                                           "ETS","VAR", "FAVAR"), 
                     values = c("#4b4b4b", colores[1], colores[2],colores[3],
                                colores[5], colores[6]))+
  theme_minimal() +  
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Tiempo", 
       y = "Sentimiento del presidente", 
       title = "Pronósticos 'bagged' con esquema 'rolling'",
       subtitle = "Siete pasos adelante (h=7)",
       caption = "Fuente: elaboración propia") +
  guides(colour = guide_legend(nrow = 1))

